use crate::{
    ast::{ASTView, Ref},
    types::{Type, TypeContext, TypeID},
};

#[derive(Debug, Clone)]
pub struct TypeMetadata {
    data: Vec<TypeID>,
    pub root: Option<Ref>,
}

impl TypeMetadata {
    pub fn new(ast: ASTView) -> Self {
        Self {
            data: (0..ast.len()).map(|x| TypeID(0)).collect(),
            root: None,
        }
    }

    pub fn set(&mut self, node: Ref, ty: TypeID) {
        self.data[node.0] = ty;
    }

    pub fn get(&mut self, node: Ref) -> TypeID {
        self.data[node.0]
    }
}

#[derive(Debug, Clone)]
pub struct TypedAST<'a> {
    pub ast: ASTView<'a>,
    pub meta: TypeMetadata,
    pub context: TypeContext<'a>,
}
