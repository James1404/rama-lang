use log::error;
use strum_macros::IntoStaticStr;

use crate::tokens::Token;

#[derive(Clone, Copy)]
pub struct Ref(usize);

#[derive(Clone, IntoStaticStr)]
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
        self.data[handle.0].clone()
    }

    pub fn get_mut(&'a mut self, handle: Ref) -> &'a mut Node<'a> {
        &mut self.data[handle.0]
    }


    pub fn alloc(&mut self, node: Node<'a>) -> Ref {
        let index = self.data.len();
        self.data.push(node);
        return Ref(index);
    }

    fn print(&self, handle: Ref, indentation: usize) {
        print!(
            "{}{}:",
            "\t".repeat(indentation),
            Into::<&'static str>::into(self.get(handle))
        );

        match self.get(handle) {
            Node::None => {},

            Node::Error { msg, token } => {
                println!("\"{}\" at [{};{}]", msg, token.line, token.location)
            }

            Node::Binary { lhs, rhs, op } => {
                println!("{}", Into::<&'static str>::into(op.ty));
                self.print(lhs, indentation + 1);
                self.print(rhs, indentation + 1);
            }
            Node::Unary { value, op } => {
                println!("{}", Into::<&'static str>::into(op.ty));
                self.print(value, indentation + 1);
            }
            Node::Float(val) => println!("{}", val),
            Node::Int(val) => println!("{}", val),
            Node::String(val) => println!("{}", val),
            Node::Bool(val) => println!("{}", val),
            Node::Ident(token) => println!("{}", token.text),
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
            Node::FnDecl { params, ret, block } => {
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
            Node::Comptime(value) => self.print(value, indentation + 1),
            Node::Extern(value) => self.print(value, indentation + 1),
            Node::FnType { params, ret } => {
                self.print(params, indentation + 1);
                self.print(ret, indentation + 1);
            }
            Node::Field { ident, ty, default } => {
                self.print(ident, indentation + 1);

                if let Some(ty) = ty {
                    self.print(ty, indentation + 1);
                }

                if let Some(default) = default {
                    self.print(default, indentation + 1);
                }
            }
            Node::Struct { fields } => {
                for node in fields {
                    self.print(node, indentation + 1);
                }
            }
            Node::Distinct(value) => self.print(value, indentation + 1),
            Node::PtrType(value) => self.print(value, indentation + 1),
            Node::Defer(value) => self.print(value, indentation + 1),
            Node::Cast { value, ty } => {
                self.print(value, indentation + 1);
                self.print(ty, indentation + 1);
            }
        }

        println!();
    }

    pub fn pretty_print(&self) {
        if let Some(root) = self.root {
            self.print(root, 0);
        } else {
            error!("Error: AST has not been generated");
        }
    }
}
