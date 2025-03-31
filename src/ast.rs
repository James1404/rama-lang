use log::error;
use strum_macros::IntoStaticStr;

use crate::tokens::Token;

pub type Ref = usize;

#[derive(Clone, IntoStaticStr)]
pub enum Node<'a> {
    Error {
        msg: &'a str,
        token: Token<'a>,
    },

    Binary {
        lhs: Ref,
        rhs: Ref,
        op: Token<'a>,
    },
    Unary {
        value: Ref,
        op: Token<'a>,
    },

    Float(&'a str),
    Int(&'a str),
    String(&'a str),
    Bool(bool),
    Ident(Token<'a>),

    TopLevelScope(Vec<Ref>),
    Scope(Vec<Ref>),

    ConstDecl {
        ident: Ref,
        ty: Option<Ref>,
        value: Ref,
    },
    VarDecl {
        ident: Ref,
        ty: Option<Ref>,
        value: Ref,
    },

    Assignment {
        ident: Ref,
        value: Ref,
    },

    ParameterList(Vec<Ref>),
    Paramater {
        ident: Ref,
        ty: Ref,
    },

    FnDecl {
        params: Ref,
        ret: Ref,
        block: Ref,
    },
    FnCall {
        func: Ref,
        args: Vec<Ref>,
    },

    Return(Ref),
    ImplicitReturn(Ref),

    Dot {
        lhs: Ref,
        ident: Ref,
    },

    If {
        cond: Ref,
        t: Ref,
        f: Ref,
    },

    Match {
        value: Ref,
        branches: Vec<Ref>,
    },
    MatchBranch {
        pattern: Ref,
        value: Ref,
    },

    Comptime(Ref),
    Extern(Ref),

    FnType {
        params: Ref,
        ret: Ref,
    },

    Field {
        ident: Ref,
        ty: Option<Ref>,
        default: Option<Ref>,
    },

    Struct {
        fields: Vec<Ref>,
    },
    Distinct(Ref),
    PtrType(Ref),

    Defer(Ref),

    Cast {
        value: Ref,
        ty: Ref,
    },
}

#[derive(Clone)]
pub struct AST<'a> {
    data: Vec<Node<'a>>,
    pub root: Option<Ref>,
}

impl<'a> AST<'a> {
    pub fn new() -> Self {
        Self {
            data: vec![],
            root: None,
        }
    }

    pub fn get(&self, handle: Ref) -> Node {
        self.data[handle].clone()
    }

    pub fn alloc(&'a mut self, node: Node<'a>) -> Ref {
        let index = self.data.len();
        self.data.push(node);
        return index;
    }

    fn print(&self, handle: Ref, indentation: usize) {
        print!(
            "{}{}:",
            "\t".repeat(indentation),
            Into::<&'static str>::into(self.get(handle))
        );

        match self.get(handle) {
            Node::Error { msg, token } => todo!(),
            Node::Binary { lhs, rhs, op } => todo!(),
            Node::Unary { value, op } => todo!(),
            Node::Float(_) => todo!(),
            Node::Int(_) => todo!(),
            Node::String(_) => todo!(),
            Node::Bool(_) => todo!(),
            Node::Ident(token) => todo!(),
            Node::TopLevelScope(items) => todo!(),
            Node::Scope(items) => todo!(),
            Node::ConstDecl { ident, ty, value } => todo!(),
            Node::VarDecl { ident, ty, value } => todo!(),
            Node::Assignment { ident, value } => todo!(),
            Node::ParameterList(items) => todo!(),
            Node::Paramater { ident, ty } => todo!(),
            Node::FnDecl { params, ret, block } => todo!(),
            Node::FnCall { func, args } => todo!(),
            Node::Return(_) => todo!(),
            Node::ImplicitReturn(_) => todo!(),
            Node::Dot { lhs, ident } => todo!(),
            Node::If { cond, t, f } => todo!(),
            Node::Match { value, branches } => todo!(),
            Node::MatchBranch { pattern, value } => todo!(),
            Node::Comptime(_) => todo!(),
            Node::Extern(_) => todo!(),
            Node::FnType { params, ret } => todo!(),
            Node::Field { ident, ty, default } => todo!(),
            Node::Struct { fields } => todo!(),
            Node::Distinct(_) => todo!(),
            Node::PtrType(_) => todo!(),
            Node::Defer(_) => todo!(),
            Node::Cast { value, ty } => todo!(),
        }
    }

    pub fn pretty_print(&self) {
        if let Some(root) = self.root {
            self.print(root, 0);
        } else {
            error!("Error: AST has not been generated");
        }
    }
}
