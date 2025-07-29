#![allow(dead_code)]

pub mod error;
mod frame;

use std::rc::Rc;

use crate::{
    ast::{
        self, AST, BinOp, Block, ConstDecl, Expr, ExternFn, Fn, Ident, If, LetDecl, Statement,
        TopLevelStatement, UnOp, Value,
    },
    ril::{self, Builder, RIL, builder::CFGBuilder},
    scope::ScopeArena,
    ty::{self, Field, FloatKind, FnType, IntSize, Record, Sum, Type, TypeRef, Variant},
};

pub use error::{Result, SemaError};
use frame::Frame;
use itertools::{Itertools, izip};

#[derive(Debug, Clone)]
enum Def<'a> {
    Const(TypeRef<'a>),
    Let(TypeRef<'a>),
    Fn(TypeRef<'a>),
    Type(TypeRef<'a>),
}

type Scope<'a> = ScopeArena<'a, Def<'a>>;

pub struct Sema<'a: 'b, 'b> {
    ast: AST<'a>,
    scopes: Scope<'a>,
    builder: Builder<'a, 'b>,
    callstack: Vec<Frame<'a>>,
}

impl<'a, 'b> Sema<'a, 'b> {
    pub fn new(ast: AST<'a>) -> Self {
        Self {
            ast,
            scopes: Scope::new(),
            callstack: vec![],
            builder: Builder::new(),
        }
    }

    fn subtype(&self, lhs: TypeRef<'a>, rhs: TypeRef<'a>) -> bool {
        lhs == rhs
    }

    fn add(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeRef<'a>> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

        match (t1.as_ref(), t2.as_ref()) {
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!("Cannot add types {t1} and {t2}",))),
        }
    }

    fn sub(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeRef<'a>> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

        match (t1.as_ref(), t2.as_ref()) {
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!(
                "Cannot subtract types {t1} and {t2}"
            ))),
        }
    }

    fn mul(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeRef<'a>> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

        match (t1.as_ref(), t2.as_ref()) {
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!(
                "Cannot multiply types {t1} and {t2}"
            ))),
        }
    }

    fn div(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeRef<'a>> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

        match (t1.as_ref(), t2.as_ref()) {
            (Type::Int { .. }, Type::Int { .. }) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),
            _ => Err(SemaError::Err(format!("Cannot divide types {t1} and {t2}"))),
        }
    }

    fn check_block(&mut self, block: Block<'a>, against: TypeRef<'a>) -> Result<'a, TypeRef<'a>> {
        let ty = self.infer_block(block)?;

        if self.subtype(ty, against) {
            self.metadata.set(term, ty);
            Ok(ty)
        } else {
            Err(SemaError::Err(format!(
                "Type {ty} is not compatible with {against}"
            )))
        }
    }

    fn check_expr(&mut self, term: Rc<Expr<'a>>, against: TypeRef<'a>) -> Result<'a, TypeRef<'a>> {
        let ty = self.infer_expr(term)?;
        if self.subtype(ty, against) {
            self.metadata.set(term, ty);
            Ok(ty)
        } else {
            Err(SemaError::Err(format!(
                "Type {ty} is not compatible with {against}"
            )))
        }
    }

    fn infer_block(&mut self, block: Block<'a>) -> Result<'a, TypeRef<'a>> {
        self.scopes.down();

        for statement in &block.statements {
            self.eval_statement(statement)?;
        }

        let result = if let Some(result) = block.result.clone() {
            self.infer_expr(result)
        } else {
            Ok(Type::Unit)
        };

        self.scopes.up();

        result
    }

    fn infer_expr(&mut self, expr: Rc<Expr<'a>>) -> Result<'a, TypeRef<'a>> {
        let ty = match *expr {
            Expr::Binary { lhs, rhs, op } => match op {
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
                    let t1 = self.infer_expr(lhs)?;
                    let t2 = self.infer_expr(rhs)?;

                    if self.subtype(t1, t2) {
                        Ok(Type::Bool)
                    } else {
                        Err(SemaError::Err(format!(
                            "Cannot compare types {t1} and {t2}",
                        )))
                    }
                }
            },
            Expr::Unary { value, op } => match op {
                UnOp::Negate => self.infer_expr(value),
                _ => Err(SemaError::InvalidTerm(expr)),
            },

            Expr::Value(value) => match value {
                Value::String(_) => Ok(Rc::new(Type::Str)),
                Value::Int(_) => Ok(Type::int(Rc::new(IntSize::Bits32))),
                Value::Float(_) => Ok(Type::Float(Rc::new(FloatKind::F32))),
                Value::Bool(_) => Ok(Rc::new(Type::Bool)),
                Value::Record { fields: v } => {
                    let mut fields = Vec::<Field>::new();
                    for field in v {
                        fields.push(Field {
                            name: field.ident.0,
                            ty: self.infer_expr(field.value)?,
                        });
                    }

                    Ok(Rc::new(Type::Record(Record { fields })))
                }
                Value::Ident(ident) => match self.scopes.get(ident) {
                    Some(Def::Const(ty)) => Ok(ty),
                    Some(Def::Let(ty)) => Ok(ty),
                    Some(Def::Fn(ty)) => Ok(ty),
                    Some(Def::Type(ty)) => Ok(ty),
                    None => Err(SemaError::NotDefined(ident.0)),
                },
                Value::Index { value, index } => {
                    let ty = self.infer_expr(value)?;

                    Ok(match self.ctx.get(ty) {
                        Type::Array { inner, len } => inner,
                        Type::Slice(inner) => inner,
                        _ => panic!(),
                    })
                }
                Value::FieldAccess { value, field } => {
                    let ty = self.infer_expr(value)?;

                    match self.ctx.get(ty) {
                        Type::Record(Record { fields }) => {
                            if let Some(Field { name: _, ty }) =
                                fields.iter().find(|f| f.name == field.0)
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
                Value::Call { func, args } => {
                    let func = self.infer_expr(func)?;

                    if let Type::Fn(FnType {
                        parameters,
                        return_ty,
                    }) = self.ctx.get(func)
                    {
                        let args = args
                            .iter()
                            .flat_map(|arg| self.infer_expr(*arg))
                            .collect_vec();

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
                Value::Deref(expr) => {
                    let t = self.infer_expr(expr)?;

                    let Type::Ptr(inner) = self.ctx.get(t) else {
                        return Err(SemaError::InvalidType);
                    };

                    Ok(inner)
                }
                Value::Ref(expr) => {
                    let t = self.infer_expr(expr)?;
                    Ok(Rc::new(Type::Ptr(t)))
                }
            },

            Expr::If(If {
                cond,
                then,
                otherwise,
            }) => {
                let b = Rc::new(Type::Bool);
                self.check_expr(cond, b)?;

                if let Some(otherwise) = otherwise {
                    let ty = self.infer_block(then)?;
                    self.check_block(otherwise, ty)?;
                    Ok(ty)
                } else {
                    Ok(Rc::new(Type::Unit))
                }
            }

            Expr::Cast { value, ty } => {
                let term_ty = self.infer_expr(value)?;
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
            Expr::Assign { lhs, value } => {
                let lhs = self.infer_expr(lhs)?;
                let value = self.infer_expr(value)?;

                if self.eq(lhs, value) {
                    Ok(Rc::new(Type::Unit))
                } else {
                    Err(SemaError::InvalidAssignment { var: lhs, value })
                }
            }
        };

        if let Ok(ty) = ty {
            self.metadata.set(expr, ty);
        }

        ty
    }

    fn can_cast(&self, from: TypeRef<'a>, into: TypeRef<'a>) -> bool {
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

    fn term_to_ty(
        &mut self,
        term: Rc<ast::Type<'a>>,
        _arguments: Option<Vec<ast::Type<'a>>>,
    ) -> Result<'a, TypeRef<'a>> {
        let ty = match *term {
            ast::Type::Record(f) => {
                let mut fields = Vec::<Field>::new();
                for field in f {
                    fields.push(Field {
                        name: field.ident.0,
                        ty: self.term_to_ty(field.ty, None)?,
                    });
                }

                Rc::new(Type::Record(Record { fields }))
            }
            ast::Type::Enum(v) => {
                let mut variants = Vec::<Variant>::new();
                for node in v {
                    variants.push(Variant {
                        name: node.ident.0,
                        ty: if let Some(ty) = node.ty {
                            Some(self.term_to_ty(ty, None)?)
                        } else {
                            None
                        },
                    });
                }

                Rc::new(Type::Sum(Sum { variants }))
            }
            ast::Type::Ptr(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                Rc::new(Type::Ptr(inner))
            }
            ast::Type::Slice(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                Rc::new(Type::Slice(inner))
            }
            ast::Type::Array(inner, len) => {
                let inner = self.term_to_ty(inner, None)?;
                Rc::new(Type::Array { inner, len })
            }

            ast::Type::Ident(ident) => match Type::from_str(ident.0) {
                Some(ty) => ty,
                None => match self.scopes.get(ident) {
                    Some(Def::Type(ty)) => Rc::new(Type::Ref(ty)),
                    _ => return Err(SemaError::NotDefined(ident.0)),
                },
            },

            _ => return Err(SemaError::InvalidTerm(term)),
        };

        self.metadata.set(term, ty);
        Ok(ty)
    }

    fn eval_statement(&mut self, statement: &Statement<'a>) -> Result<'a, ()> {
        match statement {
            Statement::LetDecl(LetDecl { ident, ty, value }) => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check_expr(value, ty)?;
                    ty
                } else {
                    self.infer_expr(value)?
                };

                self.scopes.push(ident, Def::Let(ty));
                self.metadata.set(statement, ty);
            }
            Statement::ConstDecl(ConstDecl { ident, ty, value }) => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;

                    self.check_expr(value, ty)?;
                    ty
                } else {
                    self.infer_expr(value)?
                };

                self.scopes.push(ident, Def::Const(ty));
                self.metadata.set(statement, ty);
            }

            Statement::ReturnNone => {}
            Statement::Return(value) => {
                let Some(frame) = self.callstack.last() else {
                    return Err(SemaError::CannotReturnOutsideOfFunction);
                };

                match frame.return_type {
                    Some(return_type) => {
                        self.check_expr(value, return_type)?;
                        self.metadata.set(statement, return_type);
                    }
                    None => return Err(SemaError::FunctionDoesNotHaveReturnType),
                }
            }
            Statement::Expr(expr) => {
                self.infer_expr(expr)?;
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn eval_toplevel_statement(&mut self, statement: &TopLevelStatement<'a>) -> Result<'a, ()> {
        match *statement {
            TopLevelStatement::LetDecl(LetDecl { ident, ty, value }) => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check_expr(value, ty)?;
                    ty
                } else {
                    self.infer_expr(value)?
                };

                self.scopes.push(ident, Def::Let(ty));

                self.metadata.set(statement, ty);
            }
            TopLevelStatement::ConstDecl(ConstDecl { ident, ty, value }) => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check_expr(value, ty)?;
                    ty
                } else {
                    self.infer_expr(value)?
                };

                self.scopes.push(ident, Def::Const(ty));

                self.metadata.set(statement, ty);
            }
            TopLevelStatement::ExternFn(ExternFn { ident, params, ret }) => {
                let return_ty = if let Some(ret) = ret {
                    self.term_to_ty(ret, None)?
                } else {
                    Rc::new(Type::Unit)
                };

                let parameters = {
                    let mut vec = Vec::<ril::ExternParam>::new();
                    for param in &params {
                        let ty = self.term_to_ty(param.ty, None)?;
                        vec.push(ril::ExternParam {
                            name: param.ident.0,
                            ty,
                        });
                    }

                    vec
                };

                let ty = Type::Fn(FnType {
                    parameters,
                    return_ty,
                });

                self.scopes.push(ident, Def::Fn(Rc::new(ty)));

                self.scopes.down();

                for param in params {
                    let ty = self.term_to_ty(param.ty, None)?;
                    self.scopes.push(param.ident, Def::Const(ty));
                }

                self.builder
                    .append_extern_fn(ident.0, return_ty, parameters);
            }
            TopLevelStatement::Fn(Fn {
                ident,
                params,
                ret,
                block,
            }) => {
                let cfg = CFGBuilder::new();

                let return_ty = if let Some(ret) = ret {
                    self.term_to_ty(ret, None)?
                } else {
                    Rc::new(Type::Unit)
                };

                let parameters = {
                    let mut vec = Vec::<ty::Param>::new();
                    for param in &params {
                        let ty = self.term_to_ty(param.ty, None)?;
                        vec.push(ty::Param {
                            name: param.ident.0,
                            ty,
                        });
                    }

                    vec
                };

                let ty = Type::Fn(FnType {
                    parameters,
                    return_ty,
                });

                self.scopes.push(ident, Def::Fn(Rc::new(ty)));

                self.scopes.down();

                for param in params {
                    let ty = self.term_to_ty(param.ty, None)?;
                    self.scopes.push(param.ident, Def::Const(ty));
                }

                self.callstack.push(Frame {
                    return_type: Some(return_ty),
                });

                self.infer_block(block)?;

                self.scopes.up();

                self.metadata.set(statement, ty);
            }
            TopLevelStatement::Type { ident, inner } => {
                let ty = self.term_to_ty(inner, None)?;

                self.scopes.push(ident, Def::Type(ty));

                self.metadata.set(statement, ty);
            }
            TopLevelStatement::Import(_) => todo!(),
        }

        Ok(())
    }

    pub fn run(mut self) -> (RIL<'a, 'b>, Vec<SemaError<'a>>) {
        let mut errors: Vec<SemaError> = vec![];

        let statements = std::mem::take(&mut self.ast.statements);

        for statement in statements {
            match self.eval_toplevel_statement(&statement) {
                Ok(_) => {}
                Err(e) => errors.push(e),
            }
        }

        let ril = self.builder.build();

        (ril, errors)
    }
}
