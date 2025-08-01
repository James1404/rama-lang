#![allow(dead_code)]

use std::{collections::HashMap, fmt::Display};

use bumpalo::Bump;
use derive_more::Display;
use typed_index_collections::TiVec;

pub mod builder;

pub use builder::Builder;

use crate::{ast, ty::TypeRef};

#[derive(Debug, Clone, Copy, From, Into, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
#[display("Loc.{_0}")]
pub struct Loc(pub usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Subplace {
    Field(usize),
}

#[derive(Debug, Clone, Copy, From, Into, Eq, PartialEq, Ord, PartialOrd, Hash, Display)]
#[display("%{_0}")]
pub struct Place<'a>(pub usize, pub &'a [Subplace]);

#[derive(Debug, Clone, Display)]
pub enum ConstKind<'a> {
    #[display("{_0}")]
    Float(&'a str, TypeRef<'a>),
    #[display("{_0}")]
    Integer(&'a str, TypeRef<'a>),
    #[display("\"{_0}\"")]
    String(&'a str),

    #[display("true")]
    True,
    #[display("false")]
    False,
    #[display("()")]
    Unit,
}

#[derive(Debug, Clone, Copy, Display)]
pub enum UnOp {
    #[display("!")]
    Not,
    #[display("-")]
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

#[derive(Debug, Clone, Display)]
pub enum BinOp {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,

    #[display("==")]
    Eq,
    #[display("!=")]
    Ne,
    #[display("<")]
    Lt,
    #[display("<=")]
    Le,
    #[display(">")]
    Gt,
    #[display(">=")]
    Ge,
}

impl From<ast::BinOp> for BinOp {
    fn from(value: ast::BinOp) -> Self {
        match value {
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

#[derive(Debug, Clone, Display)]
pub enum Operand<'a> {
    #[display("const {_0}")]
    Const(ConstKind<'a>),
    #[display("copy {_0}")]
    Copy(Place<'a>),
}

#[derive(Debug, Clone)]
pub enum RValue<'a> {
    Use(Operand<'a>),
    BinaryOp(BinOp, Operand<'a>, Operand<'a>, TypeRef<'a>),
    UnaryOp(UnOp, Operand<'a>, TypeRef<'a>),
    Cast {
        value: Operand<'a>,
        from: TypeRef<'a>,
        into: TypeRef<'a>,
    },
    Ref(Place<'a>),
    Call(FuncIdx, Vec<Operand<'a>>),

    BuildStruct(TypeRef<'a>, Vec<Operand<'a>>),
    BuildEnum(TypeRef<'a>, i32),
    BuildSum {
        ty: TypeRef<'a>,
        variant: i32,
        operand: Operand<'a>,
    },

    BuildArray(TypeRef<'a>, Vec<Operand<'a>>),
    BuildSlice(TypeRef<'a>, Operand<'a>),
    Len(Place<'a>),
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Assign(Place<'a>, RValue<'a>),
    Expression(RValue<'a>),
}

#[derive(Debug, Clone, Display)]
pub enum Terminator<'a> {
    #[display("goto {_0}")]
    Goto(Loc),
    #[display("if {cond} then goto {t} else goto {f}")]
    If { cond: Place<'a>, t: Loc, f: Loc },
    #[display("return")]
    ReturnNone,
    #[display("return {_0}")]
    Return(Operand<'a>),
}

struct RValueDisplay<'ctx: 'a, 'a> {
    ril: &'a RIL<'ctx, 'a>,
    rvalue: &'a RValue<'a>,
}

impl<'a> RValue<'a> {
    fn display<'ctx: 'a>(&'a self, ril: &'a RIL<'ctx, 'a>) -> RValueDisplay<'ctx, 'a> {
        RValueDisplay { ril, rvalue: self }
    }
}

impl<'ctx, 'a> Display for RValueDisplay<'ctx, 'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.rvalue {
            RValue::Use(val) => write!(f, "{val}"),
            RValue::BinaryOp(op, lhs, rhs, _) => write!(f, "{lhs} {op} {rhs}"),
            RValue::UnaryOp(op, val, _) => write!(f, "{op}{val}"),
            RValue::Cast {
                value,
                from: _,
                into,
            } => write!(f, "{value} as {into}"),
            RValue::Ref(val) => write!(f, "&{val}"),
            RValue::Call(func, args) => {
                write!(
                    f,
                    "call {} (",
                    match self.ril.get_func(*func) {
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
            RValue::BuildStruct(ty, _fields) => {
                write!(f, "build_struct {ty}")
            }
            RValue::BuildEnum(ty, variant) => {
                write!(f, "build_enum {ty} {variant}")
            }
            RValue::BuildSum {
                ty,
                variant,
                operand,
            } => {
                write!(f, "build_sum {ty} {variant} = {operand}")
            }
            RValue::BuildArray(inner, data) => {
                write!(f, "array {inner} [")?;

                let mut iter = data.iter().peekable();
                while let Some(elem) = iter.next() {
                    write!(f, "{}", elem)?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "]")
            }
            RValue::BuildSlice(inner, data) => write!(f, "slice {inner} {data}"),
            RValue::Len(place) => write!(f, "len({place})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock<'a> {
    pub statements: Vec<Statement<'a>>,
    pub terminator: Terminator<'a>,
}

#[derive(Debug, Clone)]
pub struct CFG<'a> {
    pub mapping: HashMap<Place<'a>, TypeRef<'a>>,
    pub blocks: TiVec<Loc, BasicBlock<'a>>,
}

#[derive(Debug, Clone, Copy, From, Into, Display)]
#[display("Ty_{_0}")]
pub struct TypeIdx(pub usize);

#[derive(Debug, Clone)]
pub struct TypeDef<'a> {
    pub name: &'a str,
    pub ty: TypeRef<'a>,
}

#[derive(Debug, Clone, Copy, From, Into, Display)]
#[display("Fn_{_0}")]
pub struct FuncIdx(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct ExternParam<'a> {
    pub name: &'a str,
    pub ty: TypeIdx,
}

#[derive(Debug, Clone)]
pub struct Param<'ctx: 'a, 'a> {
    pub name: &'ctx str,
    pub place: Place<'a>,
    pub ty: TypeRef<'a>,
}

#[derive(Debug, Clone)]
pub enum Func<'ctx: 'a, 'a> {
    Extern {
        name: &'ctx str,
        return_ty: TypeRef<'a>,
        params: Vec<ExternParam<'a>>,
    },
    Decl {
        name: &'ctx str,
        cfg: CFG<'a>,
        return_ty: TypeRef<'a>,
        params: Vec<Param<'ctx, 'a>>,
    },
}

#[derive(Clone, Copy)]
enum EntryPoint {
    None,
    Exe(FuncIdx),
}

pub struct RIL<'ctx, 'a> {
    pub funcs: TiVec<FuncIdx, Func<'ctx, 'a>>,
    pub entrypoint: EntryPoint,
    pub types: TiVec<TypeIdx, TypeDef<'ctx>>,
    pub arena: Bump,
}

impl<'ctx, 'a> RIL<'ctx, 'a> {
    pub fn get_func(&self, index: FuncIdx) -> &Func<'ctx, 'a> {
        &self.funcs[index]
    }

    pub fn pretty_print(&self) {
        let mut iter = self.funcs.iter().peekable();
        while let Some(func) = iter.next() {
            match func {
                Func::Extern {
                    name,
                    return_ty,
                    params,
                } => {
                    print!("extern fn {} (", name);

                    let mut iter = params.iter().peekable();
                    while let Some(param) = iter.next() {
                        print!("{}", param.ty);
                        if iter.peek().is_some() {
                            print!(", ");
                        }
                    }

                    println!(")");
                    println!(" -> {return_ty}");
                }
                Func::Decl {
                    name,
                    cfg,
                    return_ty,
                    params,
                } => {
                    print!("fn {}(", name);

                    let mut iter = params.iter().peekable();
                    while let Some(Param { name: _, place, ty }) = iter.next() {
                        print!("{place}: {ty}");
                        if iter.peek().is_some() {
                            print!(", ");
                        }
                    }

                    println!(")");
                    println!(" -> {return_ty}");

                    println!(" {{");

                    for (idx, bb) in cfg.blocks.iter_enumerated() {
                        println!("bb.{}:", idx.0);

                        for stmt in &bb.statements {
                            print!("\t");
                            match stmt {
                                Statement::Assign(place, rvalue) => {
                                    println!("{place} = {};", rvalue.display(self));
                                }
                                Statement::Expression(rvalue) => {
                                    println!("{};", rvalue.display(self));
                                }
                            }
                        }

                        println!("\t{};", bb.terminator);
                    }

                    println!("}}");
                }
            }

            if iter.peek().is_some() {
                println!();
            }
        }
    }
}
