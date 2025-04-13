use derive_more::Display;
use log::error;

use crate::lexer::Token;

#[derive(Debug, Clone, Copy, Display)]
pub struct Ref(pub usize);

#[derive(Debug, Clone)]
pub struct LiteralStructField {
    pub ident: Ref,
    pub value: Ref,
}

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Float(&'a str),
    Int(&'a str),
    String(&'a str),
    Bool(bool),
    Struct { fields: Vec<LiteralStructField> },
}

#[derive(Debug, Clone, Copy)]
pub struct StructField {
    pub ident: Ref,
    pub ty: Ref,
}

#[derive(Debug, Clone, Copy)]
pub struct EnumVariant {
    pub ident: Ref,
    pub ty: Option<Ref>,
}

#[derive(Debug, Clone, Copy)]
pub struct Param {
    pub ident: Ref,
    pub ty: Ref,
}

#[derive(Debug, Clone, strum_macros::IntoStaticStr)]
pub enum Node<'a> {
    None,

    Binary {
        lhs: Ref,
        rhs: Ref,
        op: Token<'a>,
    },
    Unary {
        value: Ref,
        op: Token<'a>,
    },

    Literal(Literal<'a>),
    Ident(Token<'a>),

    TopLevelScope(Vec<Ref>),
    Block {
        stmts: Vec<Ref>,
        result: Option<Ref>,
    },

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

    FnDecl {
        ident: Ref,
        params: Vec<Param>,
        ret: Ref,
        block: Ref,
    },
    FnCall {
        func: Ref,
        args: Vec<Ref>,
    },

    Import(&'a str),

    ReturnNone,
    Return(Ref),

    Dot {
        lhs: Ref,
        ident: Ref,
    },

    If {
        cond: Ref,
        block: Ref,
    },

    IfElse {
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

    StructType(Vec<StructField>),
    EnumType(Vec<EnumVariant>),

    TypeConstructor {
        ty: Ref,
        args: Vec<Ref>,
    },

    FnType {
        params: Ref,
        ret: Ref,
        external_linkage: bool,
    },
    DistinctType(Ref),
    PtrType(Ref),
    SliceType(Ref),
    ArrayType(Ref, usize),

    Interface {
        ident: Ref,
        fields: Vec<StructField>,
    },

    Index(Ref, Ref),
    FieldAccess(Ref, Ref),

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
    pub data: &'a [Node<'a>],
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

    pub fn len(&self) -> usize {
        self.data.len()
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

            Node::Binary { lhs, rhs, op } => {
                out!(indentation + 1, "{}", Into::<&'static str>::into(op.ty));
                self.print(lhs, indentation + 1);
                self.print(rhs, indentation + 1);
            }
            Node::Unary { value, op } => {
                out!(indentation + 1, "{}", Into::<&'static str>::into(op.ty));
                self.print(value, indentation + 1);
            }

            Node::Literal(lit) => match lit {
                Literal::Float(val) => out!(indentation + 1, "float({})", val),
                Literal::Int(val) => out!(indentation + 1, "int({})", val),
                Literal::String(val) => out!(indentation + 1, "string({})", val),
                Literal::Bool(val) => out!(indentation + 1, "bool({})", val),
                Literal::Struct { fields } => {
                    for field in fields {
                        out!(indentation + 1, "field");
                        self.print(field.ident, indentation + 2);
                        self.print(field.value, indentation + 2);
                    }
                }
            },

            Node::Ident(token) => out!(indentation + 1, "{}", token.text),
            Node::TopLevelScope(items) => {
                for node in items {
                    self.print(node, indentation + 1);
                }
            }
            Node::Block { stmts, result } => {
                for node in stmts {
                    self.print(node, indentation + 1);
                }

                out!(indentation + 1, "result:");
                if let Some(result) = result {
                    self.print(result, indentation + 2);
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

            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                self.print(ident, indentation + 1);

                out!(indentation + 1, "Params:");
                for node in params {
                    out!(indentation + 2, "Param:");
                    self.print(node.ident, indentation + 3);
                    self.print(node.ty, indentation + 3);
                }
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
            Node::ReturnNone => {}
            Node::Return(value) => self.print(value, indentation + 1),
            Node::Dot { lhs, ident } => {
                self.print(lhs, indentation + 1);
                self.print(ident, indentation + 1);
            }
            Node::If { cond, block } => {
                self.print(cond, indentation + 1);
                self.print(block, indentation + 1);
            }
            Node::IfElse { cond, t, f } => {
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
            Node::StructType(fields) => {
                for node in fields {
                    self.print(node.ident, indentation + 1);
                    self.print(node.ty, indentation + 1);
                }
            }
            Node::EnumType(variants) => {
                for node in variants {
                    self.print(node.ident, indentation + 1);
                    if let Some(ty) = node.ty {
                        self.print(ty, indentation + 1);
                    }
                }
            }
            Node::TypeConstructor { ty, args } => {
                self.print(ty, indentation + 1);
                for node in args {
                    self.print(node, indentation + 1);
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
            Node::SliceType(value) => self.print(value, indentation + 1),
            Node::ArrayType(value, len) => {
                self.print(value, indentation + 1);
                out!(indentation + 1, "len: {}", len);
            }

            Node::Interface { ident, fields } => {
                self.print(ident, indentation + 1);
                for node in fields {
                    self.print(node.ident, indentation + 1);
                    self.print(node.ty, indentation + 1);
                }
            }

            Node::Index(value, index) => {
                self.print(value, indentation + 1);
                self.print(index, indentation + 1);
            }
            Node::FieldAccess(value, field) => {
                self.print(value, indentation + 1);
                self.print(field, indentation + 1);
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
