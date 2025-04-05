#![allow(dead_code)]

pub mod error;
mod frame;
mod scope;
mod types;

use crate::{
    ast::{self, ASTView, Node},
    tokens::TokenType,
};

pub use error::{Result, SemaError};
use frame::Frame;
use scope::Scope;
use types::{FloatKind, IntKind, Type, TypeContext, TypeID};

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

    fn is_subtype(&self, _lhs: TypeID, _rhs: TypeID) -> bool {
        true
    }

    fn add(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<'ast, TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.infer(rterm)?;

        match (self.ctx.get(t1), self.ctx.get(t2)) {
            (Type::Int(_), Type::Int(_)) => Ok(t1),
            (Type::Float(_), Type::Float(_)) => Ok(t1),

            (Type::Float(_), Type::Int(_)) | (Type::Int(_), Type::Float(_)) => {
                Err(SemaError::InvalidType)
            }
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

            (Type::Float(_), Type::Int(_)) | (Type::Int(_), Type::Float(_)) => {
                Err(SemaError::InvalidType)
            }
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

            (Type::Float(_), Type::Int(_)) | (Type::Int(_), Type::Float(_)) => {
                Err(SemaError::InvalidType)
            }
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

            (Type::Float(_), Type::Int(_)) | (Type::Int(_), Type::Float(_)) => {
                Err(SemaError::InvalidType)
            }
            _ => Err(SemaError::InvalidBinaryTypes {
                lhs: t1,
                op: TokenType::Slash,
                rhs: t2,
            }),
        }
    }

    fn check(&self, term: ast::Ref, _against: TypeID) -> Result<'ast, TypeID> {
        match self.ast.get(term) {
            _ => {}
        }

        todo!()
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

            Node::String(text) => Ok(self.ctx.alloc_array(Type::Int(IntKind::U8), text.len())),
            Node::Float(_) => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
            Node::Int(_) => Ok(self.ctx.alloc(Type::Int(IntKind::I32))),
            Node::Bool(_) => Ok(self.ctx.alloc(Type::Bool)),
            Node::Ident(ident) => match self.scopes.get(ident.text) {
                Some(ty) => Ok(ty),
                None => Err(SemaError::NotDefined { token: ident }),
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

    fn term_to_ty(&self, term: ast::Ref) -> Result<'ast, TypeID> {
        match self.ast.get(term) {
            Node::Ident(token) => match self.scopes.get(token.text) {
                Some(ty) => Ok(ty),
                None => Err(SemaError::NotDefined { token }),
            },
            _ => Err(SemaError::InvalidTerm(term)),
        }
    }

    fn get_ident(&self, node: ast::Ref) -> Result<'ast, &'ast str> {
        match self.ast.get(node) {
            Node::Ident(token) => Ok(token.text),
            _ => Err(SemaError::InvalidTerm(node)),
        }
    }

    fn eval(&mut self, node: ast::Ref) -> Result<'ast, ()> {
        match self.ast.get(node) {
            Node::VarDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, ty);
            }
            Node::ConstDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, ty);
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
                    let ty = self.term_to_ty(ty)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, ty);
            }
            Node::ConstDecl { ident, ty, value } => {
                let ty = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty)?;
                    self.check(value, ty)?;
                    ty
                } else {
                    self.infer(value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, ty);
            }
            Node::FnDecl {
                ident,
                params: _,
                ret,
                block,
            } => {
                let _ident = self.get_ident(ident)?;
                let returnty = self.term_to_ty(ret)?;

                self.callstack.push(Frame {
                    return_type: Some(returnty),
                });
                self.eval(block)?;
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
