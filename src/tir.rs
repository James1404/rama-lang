#![allow(dead_code)]

use std::fmt::Display;

use derive_more::Display;
use typed_index_collections::{TiVec, ti_vec};

use crate::types::{FnType, Type, TypeContext, TypeID};

#[derive(Debug, Clone, Copy, From, Into)]
pub struct Loc(pub usize);

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Loc.{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, From, Into)]
pub struct Ref(pub usize);

impl Display for Ref {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    Add {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    Sub {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    Mul {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    Div {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },

    CmpGt {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    CmpLt {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    CmpGe {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    CmpLe {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    CmpEq {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },
    CmpNq {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
    },

    Negate {
        dest: Ref,
        value: Ref,
    },
    Not {
        dest: Ref,
        value: Ref,
    },

    Load {
        dest: Ref,
        reg: Ref,
    },
    Store {
        reg: Ref,
        value: Ref,
    },

    Ref {
        dest: Ref,
        value: Ref,
    },
    Deref {
        dest: Ref,
        value: Ref,
    },

    Cast {
        dest: Ref,
        value: Ref,

        from: TypeID,
        to: TypeID,
    },

    CreateStruct {
        dest: Ref,
        ty: TypeID,
        fields: Vec<(Ref, TypeID)>,
    },
    GetStructField {
        dest: Ref,
        r#struct: Ref,
        idx: usize,
        ty: TypeID,
    },

    FuncRef {
        dest: Ref,
        index: FuncRef,
    },
    TypeRef {
        dest: Ref,
        index: TypeRef,
    },

    Call {
        dest: Ref,
        func: Ref,
        args: Vec<Ref>,
        ty: TypeID,
    },

    // Values
    Integer {
        dest: Ref,
        value: &'a str,
        ty: TypeID,
    },
    Float {
        dest: Ref,
        value: &'a str,
        ty: TypeID,
    },
    Bool {
        dest: Ref,
        value: bool,
    },
    String {
        dest: Ref,
        value: &'a str,
    },
}

impl<'a> Display for Instruction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Add { dest, lhs, rhs } => write!(f, "{} = {} + {}", dest, lhs, rhs),
            Instruction::Sub { dest, lhs, rhs } => write!(f, "{} = {} - {}", dest, lhs, rhs),
            Instruction::Mul { dest, lhs, rhs } => write!(f, "{} = {} * {}", dest, lhs, rhs),
            Instruction::Div { dest, lhs, rhs } => write!(f, "{} = {} / {}", dest, lhs, rhs),
            Instruction::CmpGt { dest, lhs, rhs } => write!(f, "{} = {} > {}", dest, lhs, rhs),
            Instruction::CmpLt { dest, lhs, rhs } => write!(f, "{} = {} < {}", dest, lhs, rhs),
            Instruction::CmpGe { dest, lhs, rhs } => write!(f, "{} = {} >= {}", dest, lhs, rhs),
            Instruction::CmpLe { dest, lhs, rhs } => write!(f, "{} = {} <= {}", dest, lhs, rhs),
            Instruction::CmpEq { dest, lhs, rhs } => write!(f, "{} = {} == {}", dest, lhs, rhs),
            Instruction::CmpNq { dest, lhs, rhs } => write!(f, "{} = {} != {}", dest, lhs, rhs),
            Instruction::Negate { dest, value } => write!(f, "{} = -{}", dest, value),
            Instruction::Not { dest, value } => write!(f, "{} = !{}", dest, value),
            Instruction::Load { dest, reg } => write!(f, "{} = load {}", dest, reg),
            Instruction::Store { reg, value } => write!(f, "{} = {}", reg, value),
            Instruction::Ref { dest, value } => write!(f, "{} = ref {}", dest, value),
            Instruction::Deref { dest, value } => write!(f, "{} = deref {}", dest, value),
            Instruction::Cast {
                dest, value, to, ..
            } => write!(f, "{} = {} as {}", dest, value, to),
            Instruction::CreateStruct { dest, fields, .. } => {
                write!(f, "{} = struct {{", dest)?;
                for field in fields {
                    write!(f, "{},", field.0)?;
                }
                write!(f, "}}")
            }
            Instruction::GetStructField {
                dest,
                r#struct,
                idx,
                ..
            } => write!(f, "{} = getstructfield {} {}", dest, r#struct, idx),
            Instruction::FuncRef { dest, index } => write!(f, "{} = func_ref({})", dest, index.0),
            Instruction::TypeRef { dest, index } => write!(f, "{} = type_ref({})", dest, index.0),
            Instruction::Call {
                dest, func, args, ..
            } => {
                write!(f, "{} = call {} (", dest, func)?;

                for arg in args {
                    write!(f, "{}, ", arg)?;
                }

                write!(f, ")")
            }
            Instruction::Integer { dest, value, .. } => write!(f, "{} = int {}", dest, value),
            Self::Float { dest, value, .. } => write!(f, "{} = float {}", dest, value),
            Instruction::Bool { dest, value } => write!(f, "{} = {}", dest, value),
            Instruction::String { dest, value } => write!(f, "{} = {}", dest, value),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Terminator {
    Goto(Loc),
    If { cond: Ref, t: Loc, f: Loc },
    ReturnNone,
    Return(Ref),
    ImplicitReturn(Ref),
}

impl Display for Terminator {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Goto(loc) => write!(fmt, "goto {}", loc),
            Terminator::If { cond, t, f } => {
                write!(fmt, "if {} then goto {} else goto {}", cond, t, f)
            }
            Terminator::ReturnNone => write!(fmt, "return"),
            Terminator::Return(value) => write!(fmt, "return {}", value),
            Terminator::ImplicitReturn(value) => write!(fmt, "implicit_return {}", value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock<'a> {
    pub instructions: Vec<Instruction<'a>>,
    pub terminator: Terminator,
}


#[derive(Debug, Clone)]
pub struct CFG<'a> {
    pub blocks: TiVec<Loc, BasicBlock<'a>>,
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

#[derive(Debug, Clone)]
pub struct Func<'a> {
    pub name: &'a str,
    pub ty: TypeID,
    pub cfg: CFG<'a>,
}


#[derive(Debug, Clone)]
pub struct TIR<'a> {
    pub funcs: TiVec<FuncRef, Func<'a>>,
    pub types: TiVec<TypeRef, TypeDef<'a>>,
    pub ctx: TypeContext<'a>,
}

impl<'a> TIR<'a> {
    pub fn get_func(&self, index: FuncRef) -> FnType {
        let func = &self.funcs[index];

        match self.ctx.get(func.ty) {
            Type::Fn(func) => func,
            _ => panic!(),
        }
    }

    pub fn func_iter(&self) -> std::iter::Cloned<std::slice::Iter<'_, Func<'a>>> {
        self.funcs.iter().cloned()
    }

    pub fn type_iter(&self) -> std::iter::Cloned<std::slice::Iter<'_, TypeDef<'a>>> {
        self.types.iter().cloned()
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
                        print!("{}", self.ctx.display(*param));
                        if !iter.peek().is_some() {
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

                for inst in &bb.instructions {
                    println!("\t{}", inst);
                }

                println!("\t{}", bb.terminator);
            }

            println!("}}");
        }
    }
}
