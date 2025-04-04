use std::{collections::HashMap, result};

use crate::{
    ast::{self, AST, Node},
    tokens::{Token, TokenType},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeID(usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Field<'a> {
    ident: &'a str,
    ty: TypeID,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ADTKind {
    Struct,
    Enum,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ADT<'a> {
    kind: ADTKind,
    fields: Vec<Field<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntKind {
    I8,
    I16,
    I32,
    I64,
    ISize,

    U8,
    U16,
    U32,
    U64,
    USize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Bool,
    Int(IntKind),
    Float(FloatKind),

    Slice(TypeID),
    Array { inner: TypeID, len: usize },

    ADT(ADT<'a>),

    Ptr(TypeID),

    Existential,
    Universal,
}

#[derive(Debug, Clone)]
pub enum SemaError<'a> {
    InvalidType,
    InvalidTerm,
    InvalidCast {
        from: TypeID,
        into: TypeID,
    },
    InvalidBinaryTypes {
        lhs: TypeID,
        op: TokenType,
        rhs: TypeID,
    },
    NotDefined {
        ident: &'a str,
    },
}

pub type Result<'a, T> = result::Result<T, SemaError<'a>>;

#[derive(Debug, Default, Clone)]
struct TypeContext<'a> {
    data: Vec<Type<'a>>,
}

impl<'a> TypeContext<'a> {
    fn new() -> Self {
        Self { data: vec![] }
    }

    fn alloc(&mut self, ty: Type<'a>) -> TypeID {
        let index = self.data.len();
        self.data.push(ty);
        return TypeID(index);
    }

    fn alloc_slice(&mut self, inner: Type<'a>) -> TypeID {
        let inner = self.alloc(inner);
        self.alloc(Type::Slice(inner))
    }

    fn alloc_array(&mut self, inner: Type<'a>, len: usize) -> TypeID {
        let inner = self.alloc(inner);
        self.alloc(Type::Array { inner, len })
    }

    fn get(&self, id: TypeID) -> Type {
        self.data[id.0].clone()
    }

    fn get_mut(&mut self, id: TypeID) -> &mut Type<'a> {
        &mut self.data[id.0]
    }
}

#[derive(Debug, Clone)]
struct Scope<'a> {
    definitions: HashMap<&'a str, TypeID>,
    parent: Option<&'a Scope<'a>>,
    children: Vec<Scope<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            parent: None,
            children: vec![],
        }
    }

    fn new_with_parent(parent: &'a Scope<'a>) -> Self {
        Self {
            definitions: HashMap::new(),
            parent: Some(parent),
            children: vec![],
        }
    }

    fn push(&mut self, ident: &'a str, ty: TypeID) {
        self.definitions.insert(ident, ty);
    }

    fn get(&self, ident: &'a str) -> Option<TypeID> {
        match self.definitions.get(ident) {
            Some(ty) => Some(*ty),
            None => match self.parent {
                Some(parent) => parent.get(ident),
                None => None,
            },
        }
    }
}

pub struct Sema<'a> {
    ast: AST<'a>,
    ctx: TypeContext<'a>,
    scopes: Scope<'a>,
}

impl<'a> Sema<'a> {
    pub fn new(ast: AST<'a>) -> Self {
        Self {
            ast,
            ctx: TypeContext::new(),
            scopes: Scope::new(),
        }
    }

    fn equal(&self, lhs: TypeID, rhs: TypeID) -> bool {
        true
    }

    fn is_subtype(&self, lhs: TypeID, rhs: TypeID) -> bool {
        true
    }

    fn add(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<TypeID> {
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

    fn sub(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        Ok(t1)
    }

    fn mul(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        Ok(t1)
    }

    fn div(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> Result<TypeID> {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        Ok(t1)
    }

    fn check(&self, term: ast::Ref, against: TypeID) -> Result<TypeID> {
        match self.ast.get(term) {
            _ => {}
        }

        todo!()
    }

    fn infer(&mut self, term: ast::Ref) -> Result<TypeID> {
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
                _ => Err(SemaError::InvalidTerm),
            },

            Node::String(text) => Ok(self.ctx.alloc_array(Type::Int(IntKind::U8), text.len())),
            Node::Float(_) => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
            Node::Int(_) => Ok(self.ctx.alloc(Type::Int(IntKind::I32))),
            Node::Bool(_) => Ok(self.ctx.alloc(Type::Bool)),
            Node::Ident(ident) => match self.scopes.get(ident.text) {
                Some(ty) => Ok(ty),
                None => Err(SemaError::NotDefined {
                    ident: ident.text,
                }),
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

                if let Type::Ptr(inner) = self.ctx.get(t) {
                    Ok(inner)
                } else {
                    Err(SemaError::InvalidType)
                }
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

            _ => Err(SemaError::InvalidTerm),
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

    fn term_to_ty(&self, term: ast::Ref) -> Result<TypeID> {
        todo!()
    }

    fn get_ident(&'a self, node: ast::Ref) -> Result<&'a str> {
        match self.ast.get(node) {
            Node::Ident(token) => Ok(token.text),
            _ => todo!(),
        }
    }

    fn eval_function(&mut self, node: ast::Ref) -> Result<()> {
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
            _ => {}
        }

        Ok(())
    }

    pub fn run(self) -> Result<()>{
        Ok(())
    }
}
