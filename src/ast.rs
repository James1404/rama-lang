use log::error;
use strum_macros::IntoStaticStr;

use crate::tokens::Token;

#[derive(Debug, Clone, Copy)]
pub struct Ref(usize);

#[derive(Debug, Clone, IntoStaticStr)]
pub enum Node<'a> {
    None,

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
        ident: Ref,
        params: Ref,
        ret: Ref,
        block: Ref,
    },
    FnCall {
        func: Ref,
        args: Vec<Ref>,
    },

    Import(&'a str),

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

    Type {
        ident: Ref,
        args: Vec<Ref>,
        body: Ref,
    },
    StructType {
        fields: Vec<Ref>,
    },
    EnumType {
        variants: Vec<Ref>,
    },
    TypeConstructor {
        ty: Ref,
        args: Vec<Ref>,
    },

    StructField {
        ident: Ref,
        ty: Ref,
    },
    EnumVariant {
        ident: Ref,
        ty: Option<Ref>,
    },
    FnType {
        params: Ref,
        ret: Ref,
        external_linkage: bool,
    },
    DistinctType(Ref),
    PtrType(Ref),
    Interface {
        ident: Ref,
        fields: Vec<Ref>,
    },

    Reference(Ref),
    Dereference(Ref),

    Comptime(Ref),
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

#[derive(Debug, Clone, Copy)]
pub struct ASTView<'a> {
    data: &'a [Node<'a>],
    pub root: Option<Ref>,
}

impl<'a> AST<'a> {
    pub fn new() -> Self {
        Self {
            data: vec![],
            root: None,
        }
    }

    pub fn to_view(&'a self) -> ASTView<'a> {
        ASTView {
            data: self.data.as_slice(),
            root: self.root,
        }
    }

    pub fn get(&self, handle: Ref) -> Node {
        self.data[handle.0].clone()
    }

    pub fn alloc(&mut self, node: Node<'a>) -> Ref {
        let index = self.data.len();
        self.data.push(node);
        return Ref(index);
    }
}

impl<'a> ASTView<'a> {
    pub fn get(self, handle: Ref) -> Node<'a> {
        self.data[handle.0].clone()
    }

    fn print(self, handle: Ref, indentation: usize) {
        macro_rules! out {
            ($indentation:expr, $($arg:tt)*) => {{
                print!("{}", "\t".repeat($indentation));
                println!($($arg)*);
            }}
        }

        out!(
            indentation,
            "{}:",
            Into::<&'static str>::into(self.get(handle))
        );

        match self.get(handle) {
            Node::None => {}

            Node::Error { msg, token } => {
                out!(
                    indentation + 1,
                    "\"{}\" at [{};{}]",
                    msg,
                    token.line,
                    token.location
                )
            }

            Node::Binary { lhs, rhs, op } => {
                out!(indentation + 1, "{}", Into::<&'static str>::into(op.ty));
                self.print(lhs, indentation + 1);
                self.print(rhs, indentation + 1);
            }
            Node::Unary { value, op } => {
                out!(indentation + 1, "{}", Into::<&'static str>::into(op.ty));
                self.print(value, indentation + 1);
            }
            Node::Float(val) => out!(indentation + 1, "{}", val),
            Node::Int(val) => out!(indentation + 1, "{}", val),
            Node::String(val) => out!(indentation + 1, "{}", val),
            Node::Bool(val) => out!(indentation + 1, "{}", val),
            Node::Ident(token) => out!(indentation + 1, "{}", token.text),
            Node::TopLevelScope(items) => {
                for node in items {
                    self.print(node, indentation + 1);
                }
            }
            Node::Scope(items) => {
                for node in items {
                    self.print(node, indentation + 1);
                }
            }
            Node::ConstDecl { ident, ty, value } => {
                self.print(ident, indentation + 1);
                if let Some(ty) = ty {
                    self.print(ty, indentation + 1);
                }
                self.print(value, indentation + 1);
            }
            Node::VarDecl { ident, ty, value } => {
                self.print(ident, indentation + 1);
                if let Some(ty) = ty {
                    self.print(ty, indentation + 1);
                }
                self.print(value, indentation + 1);
            }
            Node::Assignment { ident, value } => {
                self.print(ident, indentation + 1);
                self.print(value, indentation + 1);
            }
            Node::ParameterList(items) => {
                for node in items {
                    self.print(node, indentation + 1);
                }
            }
            Node::Paramater { ident, ty } => {
                self.print(ident, indentation + 1);
                self.print(ty, indentation + 1);
            }
            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                self.print(ident, indentation + 1);
                self.print(params, indentation + 1);
                self.print(ret, indentation + 1);
                self.print(block, indentation + 1);
            }
            Node::FnCall { func, args } => {
                self.print(func, indentation + 1);

                for node in args {
                    self.print(node, indentation + 1);
                }
            }
            Node::Import(path) => {
                out!(indentation + 1, "{}", path);
            }
            Node::Return(value) => self.print(value, indentation + 1),
            Node::ImplicitReturn(value) => self.print(value, indentation + 1),
            Node::Dot { lhs, ident } => {
                self.print(lhs, indentation + 1);
                self.print(ident, indentation + 1);
            }
            Node::If { cond, t, f } => {
                self.print(cond, indentation + 1);
                self.print(t, indentation + 1);
                self.print(f, indentation + 1);
            }
            Node::Match { value, branches } => {
                self.print(value, indentation + 1);
                for node in branches {
                    self.print(node, indentation + 1);
                }
            }
            Node::MatchBranch { pattern, value } => {
                self.print(pattern, indentation + 1);
                self.print(value, indentation + 1);
            }

            Node::Type { ident, args, body } => {
                self.print(ident, indentation + 1);
                for node in args {
                    self.print(node, indentation + 1);
                }
                self.print(body, indentation + 1);
            }
            Node::StructType { fields } => {
                for node in fields {
                    self.print(node, indentation + 1);
                }
            }
            Node::EnumType { variants } => {
                for node in variants {
                    self.print(node, indentation + 1);
                }
            }
            Node::TypeConstructor { ty, args } => {
                self.print(ty, indentation + 1);
                for node in args {
                    self.print(node, indentation + 1);
                }
            }

            Node::StructField { ident, ty } => {
                self.print(ident, indentation + 1);
                self.print(ty, indentation + 1);
            }
            Node::EnumVariant { ident, ty } => {
                self.print(ident, indentation + 1);
                if let Some(ty) = ty {
                    self.print(ty, indentation + 1);
                }
            }
            Node::FnType {
                params,
                ret,
                external_linkage,
            } => {
                self.print(params, indentation + 1);
                self.print(ret, indentation + 1);
                print!("Extern: {}", external_linkage);
            }
            Node::DistinctType(value) => self.print(value, indentation + 1),
            Node::PtrType(value) => self.print(value, indentation + 1),
            Node::Interface { ident, fields } => {
                self.print(ident, indentation + 1);
                for node in fields {
                    self.print(node, indentation + 1);
                }
            }

            Node::Reference(value) => self.print(value, indentation + 1),
            Node::Dereference(value) => self.print(value, indentation + 1),

            Node::Comptime(value) => self.print(value, indentation + 1),
            Node::Defer(value) => self.print(value, indentation + 1),

            Node::Cast { value, ty } => {
                self.print(value, indentation + 1);
                self.print(ty, indentation + 1);
            }
        }
    }

    pub fn pretty_print(self) {
        match self.root {
            Some(root) => {
                println!("<== Printing AST ==>");
                self.print(root, 0);
                println!();
            }
            None => error!("Error: AST has not been generated"),
        }
    }
}
