#![allow(dead_code)]

use std::fmt::Display;

use derive_more::Display;
use typed_index_collections::TiVec;

mod builder;

pub use builder::Builder;

use crate::{
    ast,
    types::{FnType, Type, TypeContext, TypeID, TypeVariable},
};

#[derive(Debug, Clone, Copy, From, Into, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Loc(pub usize);

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Loc.{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ProjectionKind {
    Field(usize),
}

#[derive(Debug, Clone, Copy)]
pub struct Place(usize, Option<ProjectionKind>);

impl Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}
pub enum ConstKind<'ast> {
    Float(&'ast str),
    Integer(&'ast str),
    String(&'ast str),

    True,
    False,
    Unit,
}

pub enum AggregateKind {
    Array(TypeID),
    Adt(TypeID, Vec<TypeVariable>),
}

pub enum UnOp {
    Not,
    Negate,
}

impl From<ast::UnOp> for UnOp {
    fn from(value: ast::UnOp) -> Self {
        match value {
            ast::UnOp::Not => Self::Not,
            ast::UnOp::Negate => Self::Negate,
        }
    }
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl From<ast::BinOp> for BinOp {
    fn from(value: ast::BinOp) -> Self {
        match value {
            ast::BinOp::Invalid => panic!(),
            ast::BinOp::Add => Self::Add,
            ast::BinOp::Sub => Self::Sub,
            ast::BinOp::Mul => Self::Mul,
            ast::BinOp::Div => Self::Div,
            ast::BinOp::Less => Self::Lt,
            ast::BinOp::LessEq => Self::Le,
            ast::BinOp::Greater => Self::Gt,
            ast::BinOp::GreaterEq => Self::Ge,
            ast::BinOp::Eq => Self::Eq,
            ast::BinOp::NotEq => Self::Ne,
        }
    }
}

pub enum Operand<'ast> {
    Const(ConstKind<'ast>),
    Copy(Place),
}

pub enum RValue<'ast> {
    Use(Operand<'ast>),
    BinaryOp(BinOp, Operand<'ast>, Operand<'ast>),
    UnaryOp(UnOp, Operand<'ast>),
    Cast(Operand<'ast>, TypeID),
    Ref(Place),
    Call(FuncRef, Vec<Operand<'ast>>),
    Aggregate(AggregateKind),
}

pub enum Statement<'ast> {
    Assign(Place, RValue<'ast>),
}

pub enum Terminator<'ast> {
    Goto(Loc),
    If { cond: Place, t: Loc, f: Loc },
    ReturnNone,
    Return(Operand<'ast>),
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "!"),
            Self::Negate => write!(f, "-"),
        }
    }
}

impl<'ast> Display for ConstKind<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(val) => write!(f, "{val}"),
            Self::Integer(val) => write!(f, "{val}"),
            Self::String(val) => write!(f, "\"{val}\""),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Unit => write!(f, "()"),
        }
    }
}

impl<'ast> Display for Operand<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(kind) => write!(f, "const {kind}"),
            Self::Copy(place) => write!(f, "copy {place}"),
        }
    }
}

impl<'ast> Display for RValue<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Use(val) => write!(f, "{val}"),
            Self::BinaryOp(op, lhs, rhs) => write!(f, "{lhs} {op} {rhs}"),
            Self::UnaryOp(op, val) => write!(f, "{op}{val}"),
            Self::Cast(val, ty) => todo!(),
            Self::Ref(val) => write!(f, "&{val}"),
            Self::Call(func, args) => write!(f, "call"),
            Self::Aggregate(kind) => write!(f, "aggregate"),
        }
    }
}

impl<'ast> Display for Statement<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(place, rval) => write!(f, "{} = {}", place, rval),
        }
    }
}

impl<'ast> Display for Terminator<'ast> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Goto(loc) => write!(fmt, "goto {loc}"),
            Self::If { cond, t, f } => {
                write!(fmt, "if {cond} then goto {t} else goto {f}")
            }
            Self::ReturnNone => write!(fmt, "return"),
            Self::Return(value) => write!(fmt, "return {value}"),
        }
    }
}

pub struct BasicBlock<'ast> {
    pub statements: Vec<Statement<'ast>>,
    pub terminator: Option<Terminator<'ast>>,
}

pub struct CFG<'ast> {
    pub blocks: TiVec<Loc, BasicBlock<'ast>>,
}

#[derive(Debug, Clone, Copy, From, Into, Display)]
pub struct TypeRef(pub usize);
#[derive(Debug, Clone)]
pub struct TypeDef<'a> {
    pub name: &'a str,
    pub ty: TypeID,
}

#[derive(Debug, Clone, Copy, From, Into, Display)]
pub struct FuncRef(pub usize);

pub struct Func<'ast> {
    pub name: &'ast str,
    pub ty: TypeID,
    pub cfg: CFG<'ast>,
}

pub struct RIL<'ast> {
    pub funcs: TiVec<FuncRef, Func<'ast>>,
    pub types: TiVec<TypeRef, TypeDef<'ast>>,
    pub ctx: TypeContext<'ast>,
}

impl<'ast> RIL<'ast> {
    pub fn get_func(&self, index: FuncRef) -> &Func<'ast> {
        &self.funcs[index]
    }

    pub fn pretty_print(&self) {
        for func in &self.funcs {
            print!("fn {} (", func.name);
            match self.ctx.get(func.ty) {
                Type::Fn(FnType {
                    parameters,
                    return_ty,
                }) => {
                    let mut iter = parameters.iter().peekable();
                    while let Some(param) = iter.next() {
                        print!("{}: {}", param.0, self.ctx.display(param.1));
                        if iter.peek().is_some() {
                            print!(", ");
                        }
                    }

                    print!(") ");
                    self.ctx.display(return_ty);
                }
                _ => panic!(),
            }

            println!("{{");

            for (idx, bb) in func.cfg.blocks.iter_enumerated() {
                println!("bb.{}:", idx.0);

                for stmt in &bb.statements {
                    println!("\t{}", stmt);
                }

                println!("\t{}", bb.terminator.as_ref().unwrap());
            }

            println!("}}");
        }
    }
}
