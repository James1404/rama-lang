#![allow(dead_code)]

use std::fmt::Display;

use derive_more::Display;
use typed_index_collections::TiVec;

mod builder;

pub use builder::Builder;

use crate::{
    ast,
    types::{TypeContext, TypeID, TypeVariable},
};

#[derive(Debug, Clone, Copy, From, Into, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Loc(pub usize);

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Loc.{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, From, Into, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Place(pub usize);

impl Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConstKind<'ast> {
    Float(&'ast str, TypeID),
    Integer(&'ast str, TypeID),
    String(&'ast str),

    True,
    False,
    Unit,
}

#[derive(Debug, Clone)]
pub enum AggregateKind {
    Array(TypeID),
    Adt(TypeID, Vec<TypeVariable>),
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub enum Operand<'ast> {
    Const(ConstKind<'ast>),
    Copy(Place),
}

#[derive(Debug, Clone)]
pub enum RValue<'ast> {
    Use(Operand<'ast>),
    BinaryOp(BinOp, Operand<'ast>, Operand<'ast>),
    UnaryOp(UnOp, Operand<'ast>),
    Cast(Operand<'ast>, TypeID),
    Ref(Place),
    Call(FuncIdx, Vec<Operand<'ast>>),
    Aggregate(AggregateKind),
    Len(Place),
}

#[derive(Debug, Clone)]
pub enum Statement<'ast> {
    Assign(Place, RValue<'ast>),
}

#[derive(Debug, Clone)]
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
            Self::Float(val, _) => write!(f, "{val}"),
            Self::Integer(val, _) => write!(f, "{val}"),
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

struct RValueDisplay<'a> {
    ctx: &'a RIL<'a>,
    rvalue: &'a RValue<'a>,
}

impl<'ast> RValue<'ast> {
    fn display(&'ast self, ctx: &'ast RIL<'ast>) -> RValueDisplay<'ast> {
        RValueDisplay { ctx, rvalue: self }
    }
}

impl<'ast> Display for RValueDisplay<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.rvalue {
            RValue::Use(val) => write!(f, "{val}"),
            RValue::BinaryOp(op, lhs, rhs) => write!(f, "{lhs} {op} {rhs}"),
            RValue::UnaryOp(op, val) => write!(f, "{op}{val}"),
            RValue::Cast(val, ty) => todo!(),
            RValue::Ref(val) => write!(f, "&{val}"),
            RValue::Call(func, args) => {
                write!(
                    f,
                    "call {} (",
                    match self.ctx.get_func(*func) {
                        Func::Extern { name, .. } => name,
                        Func::Decl { name, .. } => name,
                    }
                )?;

                let mut iter = args.iter().peekable();
                while let Some(arg) = iter.next() {
                    write!(f, "{arg}")?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            }
            RValue::Aggregate(kind) => write!(f, "aggregate"),
            RValue::Len(place) => write!(f, "len({place})"),
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

#[derive(Debug, Clone)]
pub struct BasicBlock<'ast> {
    pub statements: Vec<Statement<'ast>>,
    pub terminator: Terminator<'ast>,
}

#[derive(Debug, Clone)]
pub struct CFG<'ast> {
    pub mapping: TiVec<Place, TypeID>,
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
pub struct FuncIdx(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct ExternParam<'ast> {
    pub name: &'ast str,
    pub ty: TypeID,
}

#[derive(Debug, Clone, Copy)]
pub struct Param<'ast> {
    pub name: &'ast str,
    pub place: Place,
    pub ty: TypeID,
}

#[derive(Debug, Clone)]
pub enum Func<'ast> {
    Extern {
        name: &'ast str,
        return_ty: TypeID,
        params: Vec<ExternParam<'ast>>,
    },
    Decl {
        name: &'ast str,
        cfg: CFG<'ast>,
        return_ty: TypeID,
        params: Vec<Param<'ast>>,
    },
}

#[derive(Debug, Clone)]
pub struct RIL<'ast> {
    pub funcs: TiVec<FuncIdx, Func<'ast>>,
    pub types: TiVec<TypeRef, TypeDef<'ast>>,
    pub ctx: TypeContext<'ast>,
}

impl<'ast> RIL<'ast> {
    pub fn get_func(&self, index: FuncIdx) -> &Func<'ast> {
        &self.funcs[index]
    }

    pub fn pretty_print(&self) {
        for func in &self.funcs {
            match func {
                Func::Extern {
                    name,
                    return_ty,
                    params,
                } => {
                    print!("extern fn {} (", name);

                    let mut iter = params.iter().peekable();
                    while let Some(param) = iter.next() {
                        print!("{}", self.ctx.display(param.ty));
                        if iter.peek().is_some() {
                            print!(", ");
                        }
                    }

                    println!(") -> {}", self.ctx.display(*return_ty));
                }
                Func::Decl {
                    name,
                    cfg,
                    return_ty,
                    params,
                } => {
                    print!("fn {} (", name);

                    let mut iter = params.iter().peekable();
                    while let Some(param) = iter.next() {
                        print!("{}: {}", param.place, self.ctx.display(param.ty));
                        if iter.peek().is_some() {
                            print!(", ");
                        }
                    }

                    print!(") -> {} ", self.ctx.display(*return_ty));

                    println!("{{");

                    for (idx, bb) in cfg.blocks.iter_enumerated() {
                        println!("bb.{}:", idx.0);

                        for stmt in &bb.statements {
                            print!("\t");
                            match stmt {
                                Statement::Assign(place, rvalue) => {
                                    println!("{} = {};", place, rvalue.display(self));
                                }
                            }
                        }

                        println!("\t{};", bb.terminator);
                    }

                    println!("}}");
                }
            }
        }
    }
}
