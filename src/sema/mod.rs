#![allow(dead_code)]

pub mod error;
mod frame;
mod scope;

use std::ffi::{CStr, CString};

use crate::{
    ast::{self, ASTView, EnumVariant, Literal, Node, Ref},
    lexer::{Token, TokenType},
    typed_ast::{TypeMetadata, TypedAST},
    types::{ADT, ADTKind, Field, FloatKind, IntKind, Type, TypeContext, TypeID},
};

pub use error::{Result, SemaError};
use frame::Frame;
use log::info;
use scope::{Def, Scope};

pub struct Sema<'ast: 'tcx, 'tcx> {
    ast: ASTView<'ast>,
    scopes: Scope<'ast>,

    ctx: TypeContext<'tcx>,
    callstack: Vec<Frame>,

    metadata: TypeMetadata,
}

impl<'ast, 'tcx> Sema<'ast, 'tcx> {
    pub fn new(ast: ASTView<'ast>) -> Self {
        Self {
            ast,
            ctx: TypeContext::new(),
            scopes: Scope::new(),
            callstack: vec![],

            metadata: TypeMetadata::new(ast),
        }
    }

    fn subtype(&self, lhs: TypeID, rhs: TypeID) -> bool {
        match (self.ctx.get(lhs), self.ctx.get(rhs)) {
            (Type::Int(_), Type::Int(_)) => true,
            (Type::Float(_), Type::Float(_)) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Slice(t1), Type::Slice(t2)) => self.subtype(t1, t2),
            _ => false,
        }
    }

    fn add(&mut self, lterm: Ref, rterm: Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.infer(rterm)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!(
                "Cannot add types {} and {}",
                self.ctx.display(t1),
                self.ctx.display(t2)
            ))),
        }
    }

    fn sub(&mut self, lterm: Ref, rterm: Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.infer(rterm)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!(
                "Cannot subtract types {} and {}",
                self.ctx.display(t1),
                self.ctx.display(t2)
            ))),
        }
    }

    fn mul(&mut self, lterm: Ref, rterm: Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.infer(rterm)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!(
                "Cannot multiply types {} and {}",
                self.ctx.display(t1),
                self.ctx.display(t2)
            ))),
        }
    }

    fn div(&mut self, lterm: Ref, rterm: Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.infer(rterm)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!(
                "Cannot divide types {} and {}",
                self.ctx.display(t1),
                self.ctx.display(t2)
            ))),
        }
    }

    fn check(&mut self, term: Ref, against: TypeID) -> Result<'ast, TypeID> {
        let ty = self.infer(term)?;
        if self.subtype(ty, against) {
            Ok(ty)
        } else {
            Err(SemaError::InvalidType)
        }
    }

    fn infer(&mut self, term: Ref) -> Result<'ast, TypeID> {
        match self.ast.get(term) {
            Node::Binary { lhs, rhs, op } => match op.ty {
                TokenType::Plus => self.add(lhs, rhs),
                TokenType::Minus => self.sub(lhs, rhs),
                TokenType::Asterix => self.mul(lhs, rhs),
                TokenType::Slash => self.div(lhs, rhs),
                _ => Err(SemaError::InvalidType),
            },
            Node::Unary { value, op } => match op.ty {
                TokenType::Plus | TokenType::MinusEq => self.infer(value),
                _ => Err(SemaError::InvalidTerm(term)),
            },

            Node::Literal(lit) => match lit {
                Literal::String(value) => {
                    Ok(self.ctx.alloc_array(Type::Int(IntKind::U8), value.len()))
                }
                Literal::Int(_) => Ok(self.ctx.alloc(Type::Int(IntKind::I32))),
                Literal::Float(_) => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
                Literal::Bool(_) => Ok(self.ctx.alloc(Type::Bool)),
            },
            Node::Ident(ident) => match self.scopes.get(ident.text) {
                Some(Def::Const(ty)) => Ok(ty),
                Some(Def::Var(ty)) => Ok(ty),
                Some(Def::Fn(ty)) => Ok(ty),
                Some(Def::Type(ty)) => Ok(ty),
                None => Err(SemaError::NotDefined(ident)),
            },

            Node::If { cond, t, f } => {
                let b = self.ctx.alloc(Type::Bool);
                self.check(cond, b)?;

                let t1 = self.infer(t)?;
                self.check(f, t1)?;

                Ok(t1)
            }

            Node::Dereference(term) => {
                let t = self.infer(term)?;

                let Type::Ptr(inner) = self.ctx.get(t) else {
                    return Err(SemaError::InvalidType);
                };

                Ok(inner)
            }
            Node::Reference(term) => {
                let t = self.infer(term)?;
                Ok(self.ctx.alloc(Type::Ptr(t)))
            }

            Node::Comptime(term) => self.infer(term),

            Node::Cast { value, ty } => {
                let term_ty = self.infer(value)?;
                let ty = self.term_to_ty(ty, None)?;
                if self.can_cast(term_ty, ty) {
                    Ok(ty)
                } else {
                    Err(SemaError::InvalidCast {
                        from: term_ty,
                        into: ty,
                    })
                }
            }

            _ => Err(SemaError::InvalidTerm(term)),
        }
    }

    fn can_cast(&self, from: TypeID, into: TypeID) -> bool {
        match (self.ctx.get(from), self.ctx.get(into)) {
            (Type::Int(_), Type::Int(_)) => true,
            (Type::Float(_), Type::Float(_)) => true,
            (Type::Int(_), Type::Float(_)) => true,
            (Type::Float(_), Type::Int(_)) => true,
            (
                Type::Array {
                    inner: frominner,
                    len: fromlen,
                },
                Type::Array {
                    inner: intoinner,
                    len: intolen,
                },
            ) => fromlen == intolen && self.can_cast(frominner, intoinner),
            (Type::Slice(fromty), Type::Slice(intoty)) => self.can_cast(fromty, intoty),
            (l, r) if l == r => true,
            _ => false,
        }
    }

    fn term_to_ty(&mut self, term: Ref, _arguments: Option<Vec<Ref>>) -> Result<'ast, TypeID> {
        match self.ast.get(term) {
            Node::StructType(fields) => {
                let mut adt_fields = Vec::<Field>::new();
                for node in fields {
                    adt_fields.push(Field {
                        ident: self.get_ident(node.ident)?.text,
                        ty: Some(self.term_to_ty(node.ty, None)?),
                    });
                }

                let adt = ADT {
                    kind: ADTKind::Struct,
                    fields: adt_fields,
                    generic_args: vec![],
                };

                Ok(self.ctx.alloc(Type::ADT(adt)))
            }
            Node::EnumType(variants) => {
                let mut adt_fields = Vec::<Field>::new();
                for node in variants {
                    adt_fields.push(Field {
                        ident: self.get_ident(node.ident)?.text,
                        ty: if let Some(ty) = node.ty {
                            Some(self.term_to_ty(ty, None)?)
                        } else {
                            None
                        },
                    });
                }

                let adt = ADT {
                    kind: ADTKind::Struct,
                    fields: adt_fields,
                    generic_args: vec![],
                };

                Ok(self.ctx.alloc(Type::ADT(adt)))
            }
            Node::PtrType(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                Ok(self.ctx.alloc(Type::Ptr(inner)))
            }
            Node::SliceType(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                Ok(self.ctx.alloc(Type::Slice(inner)))
            }
            Node::ArrayType(inner, len) => {
                let inner = self.term_to_ty(inner, None)?;
                Ok(self.ctx.alloc(Type::Array { inner, len }))
            }

            Node::Ident(token) => match token.text {
                "void" => Ok(self.ctx.alloc(Type::Void)),

                "i8" => Ok(self.ctx.alloc(Type::Int(IntKind::I8))),
                "i16" => Ok(self.ctx.alloc(Type::Int(IntKind::I16))),
                "i32" => Ok(self.ctx.alloc(Type::Int(IntKind::I32))),
                "i64" => Ok(self.ctx.alloc(Type::Int(IntKind::I64))),
                "isize" => Ok(self.ctx.alloc(Type::Int(IntKind::ISize))),

                "u8" => Ok(self.ctx.alloc(Type::Int(IntKind::U8))),
                "u16" => Ok(self.ctx.alloc(Type::Int(IntKind::U16))),
                "u32" => Ok(self.ctx.alloc(Type::Int(IntKind::U32))),
                "u64" => Ok(self.ctx.alloc(Type::Int(IntKind::U64))),
                "usize" => Ok(self.ctx.alloc(Type::Int(IntKind::USize))),

                "f32" => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
                "f64" => Ok(self.ctx.alloc(Type::Float(FloatKind::F64))),

                "bool" => Ok(self.ctx.alloc(Type::Bool)),

                text => match self.scopes.get(text) {
                    Some(Def::Type(ty)) => Ok(ty),
                    _ => Err(SemaError::NotDefined(token)),
                },
            },
            _ => Err(SemaError::InvalidTerm(term)),
        }
    }

    fn get_ident(&self, node: Ref) -> Result<'ast, Token<'ast>> {
        match self.ast.get(node) {
            Node::Ident(token) => Ok(token),
            _ => Err(SemaError::InvalidTerm(node)),
        }
    }

    fn eval(&mut self, node: Ref) -> Result<'ast, ()> {
        match self.ast.get(node) {
            Node::VarDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident.text, Def::Var(ty));
            }
            Node::ConstDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                info!("{:?}", self.ctx.get(ty));

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident.text, Def::Const(ty));
            }

            Node::Scope(lst) => {
                for node in lst {
                    self.eval(node)?;
                }
            }
            Node::Assignment { ident, value } => {
                let ident = self.get_ident(ident)?;
                let ty = match self.scopes.get(ident.text) {
                    Some(Def::Var(ty)) => Ok(ty),
                    Some(_) => Err(SemaError::CannotAssignToConst(ident)),
                    None => Err(SemaError::NotDefined(ident)),
                }?;

                self.check(value, ty)?;
            }

            Node::Return(value) => {
                let Some(frame) = self.callstack.last() else {
                    return Err(SemaError::CannotReturnOutsideOfFunction);
                };

                match frame.return_type {
                    Some(return_type) => {
                        self.check(value, return_type)?;
                    }
                    None => return Err(SemaError::FunctionDoesNotHaveReturnType),
                }
            }
            _ => return Err(SemaError::InvalidTerm(node)),
        }

        Ok(())
    }

    fn eval_toplevel(&mut self, node: Ref) -> Result<'ast, ()> {
        match self.ast.get(node) {
            Node::VarDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident.text, Def::Var(ty));

                self.metadata.set(node, ty);
            }
            Node::ConstDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident.text, Def::Const(ty));

                self.metadata.set(node, ty);
            }
            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                let ident = self.get_ident(ident)?;
                let returnty = self.term_to_ty(ret, None)?;

                let parameters = {
                    let mut vec = Vec::<TypeID>::new();
                    for param in params {
                        vec.push(self.term_to_ty(param.ty, None)?);
                    }

                    vec
                };

                let ty = self.ctx.alloc(Type::Fn {
                    parameters,
                    return_ty: returnty,
                });

                self.callstack.push(Frame {
                    return_type: Some(returnty),
                });
                self.eval(block)?;

                self.metadata.set(node, ty);
            }
            Node::Type { ident, args, body } => {
                let ident = self.get_ident(ident)?;
                let ty = self.term_to_ty(body, Some(args))?;

                self.scopes.push(ident.text, Def::Type(ty));

                self.metadata.set(node, ty);
            }
            _ => {}
        }

        Ok(())
    }

    pub fn run(mut self) -> (TypedAST<'tcx>, Vec<SemaError<'ast>>) {
        let mut errors: Vec<SemaError<'ast>> = vec![];

        match self.ast.root {
            Some(node) => match self.ast.get(node) {
                Node::TopLevelScope(lst) => {
                    for node in lst {
                        if let Err(err) = self.eval_toplevel(node) {
                            errors.push(err);
                        }
                    }
                }
                _ => errors.push(SemaError::InvalidRootNode(node)),
            },
            None => errors.push(SemaError::NoRootNode),
        }

        (
            TypedAST {
                ast: self.ast,
                meta: self.metadata,
                context: self.ctx,
            },
            errors,
        )
    }
}
