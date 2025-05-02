use crate::{
    ast::{ASTView, Node, Ref},
    ty::{Type, TypeContext, TypeID},
};

#[derive(Debug, Clone)]
pub struct TypeMetadata {
    pub data: Vec<Option<TypeID>>,
}

impl TypeMetadata {
    pub fn new(ast: ASTView) -> Self {
        Self {
            data: (0..ast.len()).map(|_| None).collect(),
        }
    }

    pub fn set(&mut self, node: Ref, ty: TypeID) {
        self.data[node.0] = Some(ty);
    }

    pub fn get(&self, node: Ref) -> TypeID {
        self.data[node.0].unwrap_or_else(|| panic!("Unitinialized type at {}", node))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EntryPoint {
    Exe(Ref),
    Lib(Ref),
}

#[derive(Debug, Clone)]
pub struct TypedAST<'a> {
    pub data: &'a [Node<'a>],
    pub root: Option<Ref>,
    pub meta: TypeMetadata,
    pub context: TypeContext<'a>,
    pub entrypoint: Option<EntryPoint>,
}

impl<'a> TypedAST<'a> {
    pub fn get_ident(&self, node: Ref) -> &'a str {
        match self.get_node(node) {
            Node::Ident(token) => token.text,
            _ => panic!(),
        }
    }

    pub fn get_node(&self, node: Ref) -> Node<'a> {
        self.data[node.0].clone()
    }

    pub fn get_type_id(&self, index: Ref) -> TypeID {
        self.meta.get(index)
    }

    pub fn get_ty(&self, index: Ref) -> Type<'a> {
        let ty = self.get_type_id(index);
        self.context.get(ty)
    }
}
