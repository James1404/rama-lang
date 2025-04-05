#![allow(dead_code)]

#[derive(Debug, Clone, Copy)]
pub struct Loc(usize);
#[derive(Debug, Clone, Copy)]
pub struct Ref(usize);

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Nop,

    Add { dest: Ref, lhs: Ref, rhs: Ref },
    Sub { dest: Ref, lhs: Ref, rhs: Ref },
    Mul { dest: Ref, lhs: Ref, rhs: Ref },
    Div { dest: Ref, lhs: Ref, rhs: Ref },

    CmpGt { dest: Ref, lhs: Ref, rhs: Ref },
    CmpLt { dest: Ref, lhs: Ref, rhs: Ref },
    CmpGe { dest: Ref, lhs: Ref, rhs: Ref },
    CmpLe { dest: Ref, lhs: Ref, rhs: Ref },
    CmpEq { dest: Ref, lhs: Ref, rhs: Ref },
    CmpNq { dest: Ref, lhs: Ref, rhs: Ref },

    Negate { dest: Ref, value: Ref },
    Not { dest: Ref, value: Ref },

    Load { dest: Ref, reg: Ref },
    Store { reg: Ref, value: Ref },

    Ref { dest: Ref, value: Ref },
    Deref { dest: Ref, value: Ref },
}

#[derive(Debug, Clone, Copy)]
pub enum Terminator {
    Goto(Loc),
    If { cond: Ref, t: Loc, f: Loc },
    Return(Ref),
    ImplicitReturn(Ref),
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

#[derive(Debug, Clone)]
pub struct CFG {
    blocks: Vec<BasicBlock>,
    register_count: usize,
}

#[derive(Debug, Clone)]
pub struct Func {
    block: CFG,
}

pub struct TIR {
    funcs: Vec<Func>,
}

impl TIR {
    pub fn new() -> Self {
        Self { funcs: vec![] }
    }

    pub fn to_view<'a>(&'a self) -> TIRView<'a> {
        TIRView { funcs: self.funcs.as_slice() }
    }
}

pub struct TIRView<'a> {
    funcs: &'a [Func],
}

impl<'a> TIRView<'a> {
    pub fn pretty_print(&self) {
        todo!()
    }
}
