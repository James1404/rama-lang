#![allow(dead_code)]

use crate::{ast::{self, AST}, tir::TIR, uir::UIR};

#[derive(Clone, Copy)]
struct TypeID(usize);

#[derive(Clone, Copy)]
struct Field<'a> {
    ident: &'a str,
    ty: TypeID,
}

#[derive(Clone, Copy)]
enum ADTKind {
    Struct,
    Enum,
}

#[derive(Clone)]
struct ADT<'a> {
    kind: ADTKind,
    fields: Vec<Field<'a>>,
}

#[derive(Clone)]
enum Type<'a> {
    Bool,
    Int,
    Float,

    Slice(TypeID),
    Array { inner: TypeID, len: usize },

    ADT(ADT<'a>),
}

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
}

impl<'a> Sema<'a> {
    pub fn new(ast: AST<'a>) -> Self {
        Self {
            ast
        }
    }

    fn check(&self, expected: TypeID, r: ast::Ref) -> TypeID {
        todo!()
    }

    fn infer(&self, r: ast::Ref) -> TypeID {
        todo!()
    }

    pub fn run(self) {
        
    }
}
