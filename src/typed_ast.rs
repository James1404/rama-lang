use itertools::izip;

use crate::{
    ast::{ASTView, Node, Ref},
    types::{Type, TypeContext, TypeID},
};

#[derive(Debug, Clone)]
pub struct TypeMetadata {
    pub data: Vec<Option<TypeID>>,
    pub root: Option<Ref>,
}

impl TypeMetadata {
    pub fn new(ast: ASTView) -> Self {
        Self {
            data: (0..ast.len()).map(|_| None).collect(),
            root: None,
        }
    }

    pub fn set(&mut self, node: Ref, ty: TypeID) {
        self.data[node.0] = Some(ty);
    }

    pub fn get(&self, node: Ref) -> TypeID {
        self.data[node.0].expect(format!("Unitinialized type at {}", node).as_str())
    }
}

#[derive(Debug, Clone)]
pub struct TypedAST<'a> {
    pub ast: ASTView<'a>,
    pub meta: TypeMetadata,
    pub context: TypeContext<'a>,
    pub root: Option<Ref>,
}

impl<'a> TypedAST<'a> {
    pub fn new(ast: ASTView<'a>, meta: TypeMetadata, context: TypeContext<'a>) -> Self {
        Self {
            ast,
            meta,
            context,
            root: ast.root,
        }
    }

    pub fn print(&self) {
        for (idx, (ast, ty)) in izip!(self.ast.data.iter(), self.meta.data.iter().flatten()).enumerate() {
            println!(
                "[{}] {}: {}",
                idx,
                Into::<&'static str>::into(ast),
                self.context.display(*ty)
            );
        }
    }

    pub fn get_ident(&self, node: Ref) -> &'a str {
        match self.get_node(node) {
            Node::Ident(token) => token.text,
            _ => panic!(),
        }
    }

    pub fn get_node(&self, node: Ref) -> Node<'a> {
        self.ast.get(node)
    }

    pub fn get_type_id(&self, index: Ref) -> TypeID {
        self.meta.get(index)
    }

    pub fn get_ty(&self, index: Ref) -> Type<'a> {
        let ty = self.get_type_id(index);
        self.context.get(ty)
    }

    pub fn get(&self, index: Ref) -> (Node<'a>, TypeID) {
        (self.get_node(index), self.get_type_id(index))
    }
}
