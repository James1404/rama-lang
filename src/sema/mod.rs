#![allow(dead_code)]

pub mod error;
mod frame;

use crate::{
    ast::{ASTView, Literal, Node, Ref},
    lexer::{Token, TokenType},
    tir::{TIRGen, TIR},
    typed_ast::{TypeMetadata, TypedAST},
    types::{ADTKind, Field, FloatKind, IntSize, Type, TypeContext, TypeID, ADT}, valuescope::ScopeArena,
};

pub use error::{Result, SemaError};
use frame::Frame;
use itertools::{Itertools, izip};
use log::info;

#[derive(Debug, Clone)]
enum Def {
    Const(TypeID),
    Var(TypeID),
    Fn(TypeID),
    Type(TypeID),
}

type Scope<'a> = ScopeArena<'a, Def>;

pub struct Sema<'ast> {
    ast: ASTView<'ast>,
    scopes: Scope<'ast>,

    ctx: TypeContext<'ast>,
    callstack: Vec<Frame>,

    tir: TIRGen<'ast>,
}

impl<'ast> Sema<'ast> {
    pub fn new(ast: ASTView<'ast>) -> Self {
        let ctx = TypeContext::new();

        Self {
            ast,
            ctx,
            scopes: Scope::new(),
            callstack: vec![],
            tir: TIRGen::new(),
        }
    }

    fn subtype(&self, lhs: TypeID, rhs: TypeID) -> bool {
        match (self.ctx.get(lhs), self.ctx.get(rhs)) {
            (Type::Int { .. }, Type::Int { .. }) => true,
            (Type::Float(_), Type::Float(_)) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Slice(t1), Type::Slice(t2)) => self.subtype(t1, t2),
            (Type::ADT(adt1), Type::ADT(adt2)) => {
                if adt1.kind != adt2.kind {
                    return false;
                }

                for (f1, f2) in izip!(adt1.fields, adt2.fields) {
                    match (f1.ty, f2.ty) {
                        (Some(f1), Some(f2)) => {
                            if !self.subtype(f1, f2) {
                                return false;
                            }
                        }
                        _ => {}
                    }
                }

                true
            }
            _ => false,
        }
    }

    fn add(&mut self, lterm: Ref, rterm: Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.infer(rterm)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
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
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
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
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
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
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
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
            self.metadata.set(term, ty);
            Ok(ty)
        } else {
            Err(SemaError::Err(format!(
                "Type {} is not compatible with {}",
                self.ctx.display(ty),
                self.ctx.display(against)
            )))
        }
    }

    fn infer(&mut self, term: Ref) -> Result<'ast, TypeID> {
        let ty = match self.ast.get(term) {
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
                Literal::String(value) => Ok(self
                    .ctx
                    .alloc_array(Type::uint(IntSize::Bits8), value.len())),
                Literal::Int(_) => Ok(self.ctx.alloc(Type::int(IntSize::Bits32))),
                Literal::Float(_) => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
                Literal::Bool(_) => Ok(self.ctx.alloc(Type::Bool)),
                Literal::Struct { fields } => {
                    let mut adt_fields = Vec::<Field>::new();
                    for node in fields {
                        adt_fields.push(Field {
                            ident: self.get_ident(node.ident)?,
                            ty: Some(self.infer(node.value)?),
                        });
                    }

                    let adt = ADT {
                        kind: ADTKind::Struct,
                        fields: adt_fields,
                        generic_args: vec![],
                    };

                    Ok(self.ctx.alloc(Type::ADT(adt)))
                }
            },
            Node::Ident(ident) => match self.scopes.get(ident.text) {
                Some(Def::Const(ty)) => Ok(ty),
                Some(Def::Var(ty)) => Ok(ty),
                Some(Def::Fn(ty)) => Ok(ty),
                Some(Def::Type(ty)) => Ok(ty),
                None => Err(SemaError::NotDefined(ident.text)),
            },

            Node::FieldAccess(value, field) => {
                let ty = self.infer(value)?;
                let field = self.get_ident(field)?;

                match self.ctx.get(ty) {
                    Type::ADT(ADT {
                        kind,
                        fields,
                        generic_args,
                    }) => {
                        if let Some(field) = fields.iter().find(|f| f.ident == field) {
                            Ok(field.ty.unwrap())
                        } else {
                            panic!("Doesnt have field")
                        }
                    }
                    _ => panic!("Fixme"),
                }
            }

            Node::FnCall { func, args } => {
                let func = self.infer(func)?;

                if let Type::Fn {
                    parameters,
                    return_ty,
                } = self.ctx.get(func)
                {
                    let args = args
                        .iter()
                        .map(|arg| self.infer(*arg))
                        .flatten()
                        .collect_vec();

                    for (param, arg) in izip!(parameters, args) {
                        if !self.subtype(arg, param) {
                            return Err(SemaError::Err(format!(
                                "Argument expected {} but got {}",
                                self.ctx.display(param),
                                self.ctx.display(arg)
                            )));
                        }
                    }

                    Ok(return_ty)
                } else {
                    Err(SemaError::Err("".to_owned()))
                }
            }

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
        };

        if let Ok(ty) = ty {
            self.metadata.set(term, ty);
        }

        ty
    }

    fn can_cast(&self, from: TypeID, into: TypeID) -> bool {
        match (self.ctx.get(from), self.ctx.get(into)) {
            (Type::Int { .. }, Type::Int { .. }) => true,
            (Type::Float(_), Type::Float(_)) => true,
            (Type::Int { .. }, Type::Float(_)) => true,
            (Type::Float(_), Type::Int { .. }) => true,
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
        let ty = match self.ast.get(term) {
            Node::StructType(fields) => {
                let mut adt_fields = Vec::<Field>::new();
                for node in fields {
                    adt_fields.push(Field {
                        ident: self.get_ident(node.ident)?,
                        ty: Some(self.term_to_ty(node.ty, None)?),
                    });
                }

                let adt = ADT {
                    kind: ADTKind::Struct,
                    fields: adt_fields,
                    generic_args: vec![],
                };

                self.ctx.alloc(Type::ADT(adt))
            }
            Node::EnumType(variants) => {
                let mut adt_fields = Vec::<Field>::new();
                for node in variants {
                    adt_fields.push(Field {
                        ident: self.get_ident(node.ident)?,
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

                self.ctx.alloc(Type::ADT(adt))
            }
            Node::PtrType(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Ptr(inner))
            }
            Node::SliceType(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Slice(inner))
            }
            Node::ArrayType(inner, len) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Array { inner, len })
            }

            Node::Ident(token) => match token.text {
                "void" => self.ctx.alloc(Type::Void),

                "i8" => self.ctx.alloc(Type::int(IntSize::Bits8)),
                "i16" => self.ctx.alloc(Type::int(IntSize::Bits16)),
                "i32" => self.ctx.alloc(Type::int(IntSize::Bits32)),
                "i64" => self.ctx.alloc(Type::int(IntSize::Bits64)),
                "isize" => self.ctx.alloc(Type::int(IntSize::BitsPtr)),

                "u8" => self.ctx.alloc(Type::uint(IntSize::Bits8)),
                "u16" => self.ctx.alloc(Type::uint(IntSize::Bits16)),
                "u32" => self.ctx.alloc(Type::uint(IntSize::Bits32)),
                "u64" => self.ctx.alloc(Type::uint(IntSize::Bits64)),
                "usize" => self.ctx.alloc(Type::uint(IntSize::BitsPtr)),

                "f32" => self.ctx.alloc(Type::Float(FloatKind::F32)),
                "f64" => self.ctx.alloc(Type::Float(FloatKind::F64)),

                "bool" => self.ctx.alloc(Type::Bool),

                text => match self.scopes.get(text) {
                    Some(Def::Type(ty)) => self.ctx.alloc(Type::Ref(ty)),
                    _ => return Err(SemaError::NotDefined(text)),
                },
            },
            _ => return Err(SemaError::InvalidTerm(term)),
        };

        self.metadata.set(term, ty);
        Ok(ty)
    }

    fn get_ident(&self, node: Ref) -> Result<'ast, &'ast str> {
        match self.ast.get(node) {
            Node::Ident(token) => Ok(token.text),
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
                self.scopes.push(ident, Def::Var(ty));
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

                info!("{:?}", self.ctx.get(ty));

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, Def::Const(ty));
                self.metadata.set(node, ty);
            }

            Node::Scope(lst) => {
                for node in lst {
                    self.eval(node)?;
                }
            }
            Node::Assignment { ident, value } => {
                let ty = self.infer(ident)?;
                self.check(value, ty)?;
                self.metadata.set(node, ty);
            }

            Node::Return(value) => {
                let Some(frame) = self.callstack.last() else {
                    return Err(SemaError::CannotReturnOutsideOfFunction);
                };

                match frame.return_type {
                    Some(return_type) => {
                        self.check(value, return_type)?;
                        self.metadata.set(node, return_type);
                    }
                    None => return Err(SemaError::FunctionDoesNotHaveReturnType),
                }
            }
            _ => {
                self.infer(node)?;
            }
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
                self.scopes.push(ident, Def::Var(ty));

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
                self.scopes.push(ident, Def::Const(ty));

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

                self.scopes.push(ident, Def::Fn(ty));

                self.eval(block)?;

                self.metadata.set(node, ty);
            }
            Node::Type { ident, args, body } => {
                let ident = self.get_ident(ident)?;
                let ty = self.term_to_ty(body, Some(args))?;

                self.scopes.push(ident, Def::Type(ty));

                self.metadata.set(node, ty);
            }
            _ => {}
        }

        Ok(())
    }

    pub fn run(mut self) -> (TIR<'ast>, Vec<SemaError<'ast>>) {
        let mut errors: Vec<SemaError> = vec![];

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

        (TIR::new(self.tir, self.ctx), errors)
    }
}
