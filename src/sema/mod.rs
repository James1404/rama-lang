#![allow(dead_code)]

pub mod error;
mod frame;
mod scope;
mod types;

use crate::{
    ast::{self, ASTView, EnumVariant, Literal, Node},
    tokens::{Token, TokenType},
};

pub use error::{Result, SemaError};
use frame::Frame;
use scope::{Def, Scope};
use types::{ADT, ADTKind, Field, FloatKind, IntKind, Type, TypeContext, TypeID};

pub struct Sema<'ast: 'tcx, 'tcx> {
    ast: ASTView<'ast>,
    scopes: Scope<'ast>,

    ctx: TypeContext<'tcx>,
    callstack: Vec<Frame>,
}

impl<'ast, 'tcx> Sema<'ast, 'tcx> {
    pub fn new(ast: ASTView<'ast>) -> Self {
        Self {
            ast,
            ctx: TypeContext::new(),
            scopes: Scope::new(),
            callstack: vec![],
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

    fn add(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.infer(rterm)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::InvalidBinaryTypes {
                lhs: t1,
                op: TokenType::Plus,
                rhs: t2,
            }),
        }
    }

    fn sub(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;
        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::InvalidBinaryTypes {
                lhs: t1,
                op: TokenType::Minus,
                rhs: t2,
            }),
        }
    }

    fn mul(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::InvalidBinaryTypes {
                lhs: t1,
                op: TokenType::Asterix,
                rhs: t2,
            }),
        }
    }

    fn div(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::InvalidBinaryTypes {
                lhs: t1,
                op: TokenType::Slash,
                rhs: t2,
            }),
        }
    }

    fn check(&mut self, term: ast::Ref, against: TypeID) -> Result<'ast, TypeID> {
        let ty = self.infer(term)?;
        match (self.ctx.get(ty), self.ctx.get(against)) {
            _ => todo!(),
        }
    }

    fn infer(&mut self, term: ast::Ref) -> Result<'ast, TypeID> {
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
                let ty = self.infer(ty)?;
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

    fn term_to_ty(
        &mut self,
        term: ast::Ref,
        arguments: Option<Vec<ast::Ref>>,
    ) -> Result<'ast, TypeID> {
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
            Node::Ident(token) => match token.text {
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

                text => {
                    if let Some(Def::Type(ty)) = self.scopes.get(text) {
                        Ok(ty)
                    } else {
                        Err(SemaError::NotDefined(token))
                    }
                }
            },
            _ => Err(SemaError::InvalidTerm(term)),
        }
    }

    fn get_ident(&self, node: ast::Ref) -> Result<'ast, Token<'ast>> {
        match self.ast.get(node) {
            Node::Ident(token) => Ok(token),
            _ => Err(SemaError::InvalidTerm(node)),
        }
    }

    fn eval(&mut self, node: ast::Ref) -> Result<'ast, ()> {
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

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident.text, Def::Const(ty));
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
            _ => {}
        }

        Ok(())
    }

    fn eval_toplevel(&mut self, node: ast::Ref) -> Result<'ast, ()> {
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

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident.text, Def::Const(ty));
            }
            Node::FnDecl {
                ident,
                params: _,
                ret,
                block,
            } => {
                let _ident = self.get_ident(ident)?;
                let returnty = self.term_to_ty(ret, None)?;

                self.callstack.push(Frame {
                    return_type: Some(returnty),
                });
                self.eval(block)?;
            }
            Node::Type { ident, args, body } => {
                let ident = self.get_ident(ident)?;
                let ty = self.term_to_ty(body, Some(args))?;

                self.scopes.push(ident.text, Def::Type(ty));
            }
            _ => {}
        }

        Ok(())
    }

    pub fn run(&'tcx mut self) -> Vec<SemaError<'ast>> {
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

        errors
    }
}
