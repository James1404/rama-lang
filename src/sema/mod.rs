#![allow(dead_code)]

pub mod error;
mod frame;

use crate::{
    ast::{ASTView, BinOp, Literal, Node, Ref, UnOp},
    scope::ScopeArena,
    tast::{TypeMetadata, TypedAST},
    ty::{
        Enum, Field, FloatKind, FnType, IntSize, Struct, Sum, Type, TypeContext, TypeID, Variant,
    },
};

pub use error::{Result, SemaError};
use frame::Frame;
use itertools::{Itertools, izip};

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

    metadata: TypeMetadata,
}

impl<'ast> Sema<'ast> {
    pub fn new(ast: ASTView<'ast>) -> Self {
        Self {
            ast,
            ctx: TypeContext::new(),
            scopes: Scope::new(),
            callstack: vec![],

            metadata: TypeMetadata::new(ast),
        }
    }

    fn eq(&self, lhs: TypeID, rhs: TypeID) -> bool {
        self.ctx.eq(lhs, rhs)
    }

    fn subtype(&self, lhs: TypeID, rhs: TypeID) -> bool {
        match (self.ctx.get(lhs), self.ctx.get(rhs)) {
            (Type::Unit, _) => true, // check this is right
            (Type::Bool, Type::Bool) => true,
            (Type::Slice(t1), Type::Slice(t2)) => self.subtype(t1, t2),
            (Type::Array { inner, len: _ }, Type::Slice(ty)) => self.subtype(inner, ty),
            _ => self.eq(lhs, rhs),
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
            Node::Binary { lhs, rhs, op } => match op {
                BinOp::Invalid => panic!(),

                BinOp::Add => self.add(lhs, rhs),
                BinOp::Sub => self.sub(lhs, rhs),
                BinOp::Mul => self.mul(lhs, rhs),
                BinOp::Div => self.div(lhs, rhs),

                BinOp::Eq
                | BinOp::NotEq
                | BinOp::Less
                | BinOp::LessEq
                | BinOp::Greater
                | BinOp::GreaterEq => {
                    let t1 = self.infer(lhs)?;
                    let t2 = self.infer(rhs)?;

                    if self.subtype(t1, t2) {
                        Ok(self.ctx.alloc(Type::Bool))
                    } else {
                        Err(SemaError::Err(format!(
                            "Cannot compare types {} and {}",
                            self.ctx.display(t1),
                            self.ctx.display(t2)
                        )))
                    }
                }
            },
            Node::Unary { value, op } => match op {
                UnOp::Negate => self.infer(value),
                _ => Err(SemaError::InvalidTerm(term)),
            },

            Node::Literal(lit) => match lit {
                Literal::String(_) => Ok(self.ctx.alloc(Type::Str)),
                Literal::Int(_) => Ok(self.ctx.alloc(Type::int(IntSize::Bits32))),
                Literal::Float(_) => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
                Literal::Bool(_) => Ok(self.ctx.alloc(Type::Bool)),
                Literal::Struct { fields: v } => {
                    let mut fields = Vec::<Field>::new();
                    for node in v {
                        fields.push(Field {
                            name: self.get_ident(node.ident)?,
                            ty: self.infer(node.value)?,
                        });
                    }

                    Ok(self.ctx.alloc(Type::Struct(Struct {
                        fields,
                        typevariables: vec![],
                    })))
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
                let ident = self.get_ident(field)?;

                match self.ctx.get(ty) {
                    Type::Struct(Struct {
                        fields,
                        typevariables: _,
                    }) => {
                        if let Some(Field { name: _, ty }) = fields.iter().find(|f| f.name == ident)
                        {
                            self.metadata.set(field, *ty);
                            Ok(*ty)
                        } else {
                            Err(SemaError::Err("Doesnt have field".to_owned()))
                        }
                    }
                    _ => panic!("Fixme"),
                }
            }

            Node::FnCall { func, args } => {
                let func = self.infer(func)?;

                if let Type::Fn(FnType {
                    parameters,
                    return_ty,
                }) = self.ctx.get(func)
                {
                    let args = args.iter().flat_map(|arg| self.infer(*arg)).collect_vec();

                    for (param, arg) in izip!(parameters, args) {
                        if !self.subtype(arg, param.1) {
                            return Err(SemaError::Err(format!(
                                "Argument expected {} but got {}",
                                self.ctx.display(param.1),
                                self.ctx.display(arg)
                            )));
                        }
                    }

                    Ok(return_ty)
                } else {
                    Err(SemaError::Err("".to_owned()))
                }
            }
            Node::Block { stmts, result } => {
                for stmt in stmts {
                    self.eval(stmt)?;
                }

                if let Some(result) = result {
                    let ty = self.infer(result)?;
                    Ok(ty)
                } else {
                    Ok(self.ctx.alloc(Type::Unit))
                }
            }

            Node::IfElse { cond, t, f } => {
                let b = self.ctx.alloc(Type::Bool);
                self.check(cond, b)?;

                let ty = self.infer(t)?;
                self.check(f, ty)?;

                Ok(ty)
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
            _ => self.eq(from, into),
        }
    }

    fn term_to_ty(&mut self, term: Ref, _arguments: Option<Vec<Ref>>) -> Result<'ast, TypeID> {
        let ty = match self.ast.get(term) {
            Node::StructType(f) => {
                let mut fields = Vec::<Field>::new();
                for node in f {
                    fields.push(Field {
                        name: self.get_ident(node.ident)?,
                        ty: self.term_to_ty(node.ty, None)?,
                    });
                }

                self.ctx.alloc(Type::Struct(Struct {
                    fields,
                    typevariables: vec![],
                }))
            }
            Node::EnumType(v) => {
                let mut variants = Vec::<Variant>::new();
                for node in v {
                    variants.push(Variant {
                        name: self.get_ident(node.ident)?,
                        ty: if let Some(ty) = node.ty {
                            Some(self.term_to_ty(ty, None)?)
                        } else {
                            None
                        },
                    });
                }

                self.ctx.alloc(Type::Sum(Sum {
                    variants,
                    typevariables: vec![],
                }))
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

            Node::Ident(token) => match Type::from_str(token.text) {
                Some(ty) => self.ctx.alloc(ty),
                None => match self.scopes.get(token.text) {
                    Some(Def::Type(ty)) => self.ctx.alloc(Type::Ref(ty)),
                    _ => return Err(SemaError::NotDefined(token.text)),
                },
            },

            Node::TypeConstructor { ty, args } => {
                todo!()
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

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, Def::Const(ty));
                self.metadata.set(node, ty);
            }

            Node::Assignment { ident, value } => {
                let ty = self.infer(ident)?;
                self.check(value, ty)?;
                self.metadata.set(node, ty);
            }

            Node::If { cond, block } => {
                let b = self.ctx.alloc(Type::Bool);
                self.check(cond, b)?;

                self.eval(block)?;
            }

            Node::ReturnNone => {}
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
            Node::ExternFnDecl { ident, params, ret } => {
                let ident = self.get_ident(ident)?;

                let returnty = self.term_to_ty(ret, None)?;

                let parameters = {
                    let mut vec = Vec::<(&'ast str, TypeID)>::new();
                    for param in &params {
                        let ident = self.get_ident(param.ident)?;
                        vec.push((ident, self.term_to_ty(param.ty, None)?));
                    }

                    vec
                };

                let ty = self.ctx.alloc(Type::Fn(FnType {
                    parameters,
                    return_ty: returnty,
                }));

                self.scopes.push(ident, Def::Fn(ty));

                self.scopes.down();

                for param in params {
                    let ident = self.get_ident(param.ident)?;
                    let ty = self.term_to_ty(param.ty, None)?;
                    self.scopes.push(ident, Def::Const(ty));
                }

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
                    let mut vec = Vec::<(&'ast str, TypeID)>::new();
                    for param in &params {
                        let ident = self.get_ident(param.ident)?;
                        vec.push((ident, self.term_to_ty(param.ty, None)?));
                    }

                    vec
                };

                let ty = self.ctx.alloc(Type::Fn(FnType {
                    parameters,
                    return_ty: returnty,
                }));

                self.scopes.push(ident, Def::Fn(ty));

                self.scopes.down();

                for param in params {
                    let ident = self.get_ident(param.ident)?;
                    let ty = self.term_to_ty(param.ty, None)?;
                    self.scopes.push(ident, Def::Const(ty));
                }

                self.callstack.push(Frame {
                    return_type: Some(returnty),
                });

                self.eval(block)?;
                self.scopes.up();

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

    pub fn run(mut self) -> (TypedAST<'ast>, Vec<SemaError<'ast>>) {
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

        let tast = TypedAST {
            data: self.ast.data,
            root: self.ast.root,
            meta: self.metadata,
            context: self.ctx,
        };

        (tast, errors)
    }
}
