#![allow(dead_code)]

use crate::{
    ast::{self, AST, Node},
    tokens::TokenType,
};

#[derive(Debug, Clone, Copy)]
struct TypeID(usize);

#[derive(Debug, Clone, Copy)]
struct Field<'a> {
    ident: &'a str,
    ty: TypeID,
}

#[derive(Debug, Clone, Copy)]
enum ADTKind {
    Struct,
    Enum,
}

#[derive(Debug, Clone)]
struct ADT<'a> {
    kind: ADTKind,
    fields: Vec<Field<'a>>,
}

#[derive(Debug, Clone, Copy)]
enum FloatKind {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy)]
enum IntKind {
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

#[derive(Debug, Clone)]
enum Type<'a> {
    Bool,
    Int(IntKind),
    Float(FloatKind),

    Slice(TypeID),
    Array { inner: TypeID, len: usize },

    ADT(ADT<'a>),

    Existential,
    Universal,
}

#[derive(Debug, Clone, Copy)]
enum TypeError {
    InvalidType,
    InvalidTerm,
}
type TypeResult = Result<TypeID, TypeError>;

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

    fn get(&self, id: TypeID) -> Type {
        self.data[id.0].clone()
    }

    fn get_mut(&mut self, id: TypeID) -> &mut Type<'a> {
        &mut self.data[id.0]
    }
}

pub struct Sema<'a> {
    ast: AST<'a>,
    ctx: TypeContext<'a>,
}

impl<'a> Sema<'a> {
    pub fn new(ast: AST<'a>) -> Self {
        Self {
            ast,
            ctx: TypeContext::new(),
        }
    }

    fn equal(&self, lhs: TypeID, rhs: TypeID) -> bool {
        true
    }

    fn is_subtype(&self, lhs: TypeID, rhs: TypeID) -> bool {
        true
    }

    fn add(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> TypeResult {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        Ok(t1)
    }

    fn sub(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> TypeResult {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        Ok(t1)
    }

    fn mul(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> TypeResult {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        Ok(t1)
    }

    fn div(&mut self, lterm: ast::Ref, rterm: ast::Ref) -> TypeResult {
        let t1 = self.infer(lterm)?;
        let t2 = self.check(rterm, t1)?;

        Ok(t1)
    }

    fn check(&self, term: ast::Ref, against: TypeID) -> TypeResult {
        match self.ast.get(term) {
            _ => {}
        }

        todo!()
    }

    fn infer(&mut self, term: ast::Ref) -> TypeResult {
        match self.ast.get(term) {
            Node::Binary { lhs, rhs, op } => match op.ty {
                TokenType::Plus => self.add(lhs, rhs),
                TokenType::Minus => self.sub(lhs, rhs),
                TokenType::Asterix => self.mul(lhs, rhs),
                TokenType::Slash => self.div(lhs, rhs),
                _ => Err(TypeError::InvalidType),
            },
            Node::Unary { value, op } => {
                let ty = self.infer(value);
                ty
            }
            Node::Float(_) => Ok(self.ctx.alloc(Type::Float(FloatKind::F32))),
            Node::Int(_) => Ok(self.ctx.alloc(Type::Int(IntKind::I32))),
            Node::Bool(_) => Ok(self.ctx.alloc(Type::Bool)),

            _ => Err(TypeError::InvalidTerm),
        }
    }

    pub fn run(self) {}
}
