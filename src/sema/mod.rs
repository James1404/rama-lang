#![allow(dead_code)]

pub mod error;
mod frame;

use std::rc::Rc;

use crate::{
    ast::{self, BinOp, Block, ConstDecl, Expr, Ident, If, LetDecl, Statement, UnOp, Value, AST}, ril::{Builder, RIL}, scope::ScopeArena, tast::{TypeMetadata, TypedAST}, ty::{Field, FloatKind, FnType, IntSize, Record, Sum, Type, TypeContext, TypeID, Variant}
};

pub use error::{Result, SemaError};
use frame::Frame;
use itertools::{Itertools, izip};

#[derive(Debug, Clone)]
enum Def {
    Const(TypeID),
    Let(TypeID),
    Fn(TypeID),
    Type(TypeID),
}

type Scope<'a> = ScopeArena<'a, Def>;

pub struct Sema<'a: 'b, 'b> {
    ast: AST<'a>,
    scopes: Scope<'a>,

    ril: Builder<'a, 'b>,

    ctx: TypeContext<'a>,
    callstack: Vec<Frame>,
}

impl<'a, 'b> Sema<'a, 'b> {
    pub fn new(ast: AST<'a>) -> Self {
        let ctx = TypeContext::new();
        Self {
            ast,
            ctx,
            scopes: Scope::new(),
            callstack: vec![],
            ril: Builder::new(&ctx)
        }
    }

    fn eq(&self, lhs: TypeID, rhs: TypeID) -> bool {
        self.ctx.eq(lhs, rhs)
    }

    fn subtype(&self, lhs: TypeID, rhs: TypeID) -> bool {
        match (self.ctx.get(lhs), self.ctx.get(rhs)) {
            (Type::Unit, Type::Unit) => true, // check this is right
            (Type::Bool, Type::Bool) => true,
            _ => self.eq(lhs, rhs),
        }
    }

    fn add(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeID> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

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

    fn sub(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeID> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

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

    fn mul(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeID> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

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

    fn div(&mut self, lterm: Rc<Expr<'a>>, rterm: Rc<Expr<'a>>) -> Result<'a, TypeID> {
        let t1 = self.infer_expr(lterm)?;
        let t2 = self.infer_expr(rterm)?;

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

    fn check_block(&mut self, block: Block<'a>, against: TypeID) -> Result<'a, TypeID> {
        let ty = self.infer_block(block)?;

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

    fn check_expr(&mut self, term: Rc<Expr<'a>>, against: TypeID) -> Result<'a, TypeID> {
        let ty = self.infer_expr(term)?;
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

    fn infer_block(&mut self, block: Block<'a>) -> Result<'a, TypeID> {
        self.scopes.down();

        for statement in &block.statements {
            self.eval_statement(statement)?;
        }

        let result = if let Some(result) = block.result.clone() {
            self.infer_expr(result)
        } else {
            Ok(self.ctx.alloc(Type::Unit))
        };

        self.scopes.up();

        result
    }

    fn infer_expr(&mut self, expr: Rc<Expr<'a>>) -> Result<'a, TypeID> {
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
            Expr::Unary { value, op } => match op {
                UnOp::Negate => self.infer_expr(value),
                _ => Err(SemaError::InvalidTerm(expr)),
            },

            Expr::Value(value) => match value {
                Value::String(_) => Ok(self.ctx.alloc(Type::Str)),
                Value::Int(_) => Ok(self.ctx.alloc(Type::int(IntSize::Bits32))),
                Value::Float(_) => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
                Value::Bool(_) => Ok(self.ctx.alloc(Type::Bool)),
                Value::Record { fields: v } => {
                    let mut fields = Vec::<Field>::new();
                    for field in v {
                        fields.push(Field {
                            name: field.ident.0,
                            ty: self.infer_expr(field.value)?,
                        });
                    }

                    Ok(self.ctx.alloc(Type::Record(Record {
                        fields,
                        typevariables: vec![],
                    })))
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
                        Type::Record(Record {
                            fields,
                            typevariables: _,
                        }) => {
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
                    Ok(self.ctx.alloc(Type::Ptr(t)))
                }
            },

            Expr::If(If {
                cond,
                then,
                otherwise,
            }) => {
                let b = self.ctx.alloc(Type::Bool);
                self.check_expr(cond, b)?;

                if let Some(otherwise) = otherwise {
                    let ty = self.infer_block(then)?;
                    self.check_block(otherwise, ty)?;
                    Ok(ty)
                } else {
                    Ok(self.ctx.alloc(Type::Unit))
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
                    Ok(self.ctx.alloc(Type::Unit))
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

    fn term_to_ty(
        &mut self,
        term: Rc<ast::Type<'a>>,
        _arguments: Option<Vec<ast::Type<'a>>>,
    ) -> Result<'a, TypeID> {
        let ty = match *term {
            ast::Type::Record(f) => {
                let mut fields = Vec::<Field>::new();
                for field in f {
                    fields.push(Field {
                        name: field.ident.0,
                        ty: self.term_to_ty(field.ty, None)?,
                    });
                }

                self.ctx.alloc(Type::Record(Record {
                    fields,
                    typevariables: vec![],
                }))
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

                self.ctx.alloc(Type::Sum(Sum {
                    variants,
                    typevariables: vec![],
                }))
            }
            ast::Type::Ptr(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Ptr(inner))
            }
            ast::Type::Slice(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Slice(inner))
            }
            ast::Type::Array(inner, len) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Array { inner, len })
            }

            ast::Type::Ident(ident) => match Type::from_str(ident.0) {
                Some(ty) => self.ctx.alloc(ty),
                None => match self.scopes.get(ident) {
                    Some(Def::Type(ty)) => self.ctx.alloc(Type::Ref(ty)),
                    _ => return Err(SemaError::NotDefined(ident.0)),
                },
            },

            _ => return Err(SemaError::InvalidTerm(term)),
        };

        self.metadata.set(term, ty);
        Ok(ty)
    }

    fn eval_statement(&mut self, statement: Statement<'a>) -> Result<'a, ()> {
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

    fn eval_toplevel(&mut self, node: Ref) -> Result<'a, ()> {
        match self.ast.get(node) {
            Node::LetDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check_expr(value, ty)?;
                    ty
                } else {
                    self.infer_expr(value)?
                };

                self.scopes.push(ident, Def::Let(ty));

                self.metadata.set(node, ty);
            }
            Node::ConstDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    self.check_expr(value, ty)?;
                    ty
                } else {
                    self.infer_expr(value)?
                };

                self.scopes.push(ident, Def::Const(ty));

                self.metadata.set(node, ty);
            }
            Node::ExternFnDecl { ident, params, ret } => {
                let return_ty = self.term_to_ty(ret, None)?;

                let parameters = {
                    let mut vec = Vec::<(&'a str, TypeID)>::new();
                    for param in &params {
                        vec.push((param.ident, self.term_to_ty(param.ty, None)?));
                    }

                    vec
                };

                let ty = self.ctx.alloc(Type::Fn(FnType {
                    parameters,
                    return_ty,
                }));

                self.scopes.push(ident, Def::Fn(ty));

                self.scopes.down();

                for param in params {
                    let ty = self.term_to_ty(param.ty, None)?;
                    self.scopes.push(param.ident, Def::Const(ty));
                }

                self.metadata.set(node, ty);
            }
            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                let return_ty = if let Some(ret) = ret {
                    self.term_to_ty(ret, None)?
                } else {
                    self.ctx.alloc(Type::Unit)
                };

                let parameters = {
                    let mut vec = Vec::<(&'a str, TypeID)>::new();
                    for param in &params {
                        vec.push((param.ident, self.term_to_ty(param.ty, None)?));
                    }

                    vec
                };

                let ty = self.ctx.alloc(Type::Fn(FnType {
                    parameters,
                    return_ty,
                }));

                self.scopes.push(ident, Def::Fn(ty));

                self.scopes.down();

                for param in params {
                    let ty = self.term_to_ty(param.ty, None)?;
                    self.scopes.push(param.ident, Def::Const(ty));
                }

                self.callstack.push(Frame {
                    return_type: Some(return_ty),
                });

                self.eval_statement(block)?;
                self.scopes.up();

                self.metadata.set(node, ty);
            }
            Node::Type { ident, args, body } => {
                let ty = self.term_to_ty(body, Some(args))?;

                self.scopes.push(ident, Def::Type(ty));

                self.metadata.set(node, ty);
            }
            _ => {}
        }

        Ok(())
    }

    pub fn run(mut self) -> (TypedAST<'a>, Vec<SemaError<'a>>) {
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
