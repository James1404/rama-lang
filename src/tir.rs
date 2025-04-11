#![allow(dead_code)]

use std::fmt::Display;

use derive_more::Display;
use typed_index_collections::TiVec;

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

#[derive(Debug, Clone, Copy, Display)]
pub enum CmpKind {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
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

    Cmp {
        dest: Ref,
        lhs: Ref,
        rhs: Ref,
        kind: CmpKind,
        ty: TypeID,
    },

    Negate {
        dest: Ref,
        value: Ref,
    },
    Not {
        dest: Ref,
        value: Ref,
    },

    MakeVar {
        dest: Ref,
        value: Ref,
        ty: TypeID,
    },
    ReadVar {
        dest: Ref,
        var: Ref,
    },
    WriteVar {
        var: Ref,
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

    MakeStruct {
        dest: Ref,
        ty: TypeID,
    },
    ReadField {
        dest: Ref,
        r#struct: Ref,
        field: usize,
        ty: TypeID,
    },
    WriteField {
        r#struct: Ref,
        field: usize,
        value: Ref,
        ty: TypeID,
    },

    ReadArg {
        dest: Ref,
        index: usize,
    },

    Call {
        dest: Ref,
        func: FuncRef,
        args: Vec<Ref>,
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

impl<'a> Instruction<'a> {
    fn display(&'a self, tir: &'a TIR<'a>) -> InstructionFmt<'a> {
        InstructionFmt { tir, inst: self }
    }
}

pub struct InstructionFmt<'a> {
    tir: &'a TIR<'a>,
    inst: &'a Instruction<'a>,
}

impl<'a> Display for InstructionFmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inst {
            Instruction::Add { dest, lhs, rhs } => write!(f, "{} = {} + {}", dest, lhs, rhs),
            Instruction::Sub { dest, lhs, rhs } => write!(f, "{} = {} - {}", dest, lhs, rhs),
            Instruction::Mul { dest, lhs, rhs } => write!(f, "{} = {} * {}", dest, lhs, rhs),
            Instruction::Div { dest, lhs, rhs } => write!(f, "{} = {} / {}", dest, lhs, rhs),
            Instruction::Cmp {
                dest,
                lhs,
                rhs,
                kind,
                ..
            } => write!(f, "{} = cmp {} {} {}", dest, kind, lhs, rhs),
            Instruction::Negate { dest, value } => write!(f, "{} = -{}", dest, value),
            Instruction::Not { dest, value } => write!(f, "{} = !{}", dest, value),

            Instruction::MakeVar { dest, value, ty: _ } => write!(f, "var {} = {}", dest, value),
            Instruction::ReadVar { dest, var } => write!(f, "{} = read_var {}", dest, var),
            Instruction::WriteVar { var, value } => write!(f, "store {} {}", var, value),

            Instruction::Ref { dest, value } => write!(f, "{} = ref {}", dest, value),
            Instruction::Deref { dest, value } => write!(f, "{} = deref {}", dest, value),
            Instruction::Cast {
                dest, value, to, ..
            } => write!(f, "{} = {} as {}", dest, value, self.tir.ctx.display(*to)),

            Instruction::MakeStruct { dest, ty } => {
                write!(f, "{} = make {}", dest, self.tir.ctx.display(*ty))
            }
            Instruction::ReadField {
                dest,
                r#struct,
                field,
                ty: _,
            } => write!(f, "{} = {}.{}", dest, r#struct, field),
            Instruction::WriteField {
                r#struct,
                field,
                value,
                ty: _,
            } => write!(f, "{}.{} = {}", r#struct, field, value),

            Instruction::ReadArg { dest, index } => write!(f, "{} = arg {}", dest, index),

            Instruction::Call { dest, func, args } => {
                write!(f, "{} = call {} (", dest, self.tir.get_func(*func).name)?;

                for arg in args {
                    write!(f, "{}, ", arg)?;
                }

                write!(f, ")")
            }
            Instruction::Integer { dest, value, ty } => {
                write!(f, "{} = {}({})", dest, self.tir.ctx.display(*ty), value)
            }
            Instruction::Float { dest, value, ty } => {
                write!(f, "{} = {}({})", dest, self.tir.ctx.display(*ty), value)
            }
            Instruction::Bool { dest, value } => write!(f, "{} = {}", dest, value),
            Instruction::String { dest, value } => write!(f, "{} = string \"{}\"", dest, value),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Terminator {
    Goto(Loc),
    If { cond: Ref, t: Loc, f: Loc },
    ReturnNone,
    Return(Ref),
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
    pub fn get_func(&self, index: FuncRef) -> &Func<'a> {
        &self.funcs[index]
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
                        print!("{}: {}", param.0, self.ctx.display(param.1));
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
                    println!("\t{}", inst.display(self));
                }

                println!("\t{}", bb.terminator);
            }

            println!("}}");
        }
    }
}
