#![allow(dead_code)]

use phf::phf_map;

pub enum Builtin {
    Void,
    String,
    Bool,

    I8,
    I16,
    I32,
    I64,
    Isize,

    U8,
    U16,
    U32,
    U64,
    Usize,

    F32,
    F64,

    Any,
    Type,
}

pub static BUILT_IN_KEYWORDS: phf::Map<&'static str, Builtin> = phf_map! {
    "void" => Builtin::Void,
    "string" => Builtin::String,
    "bool" => Builtin::Bool,

    "i8" => Builtin::I8,
    "i16" => Builtin::I16,
    "i32" => Builtin::I32,
    "i64" => Builtin::I64,
    "isize" => Builtin::Isize,

    "u8" => Builtin::U8,
    "u16" => Builtin::U16,
    "u32" => Builtin::U32,
    "u64" => Builtin::U64,
    "usize" => Builtin::Usize,

    "f32" => Builtin::F32,
    "f64" => Builtin::F64,

    "any" => Builtin::Any,
    "type" => Builtin::Type
};

pub enum Instruction {
    Nop,

    Add {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    Sub {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    Div {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    Mul {
        dest: Ref,
        l: Ref,
        r: Ref,
    },

    CmpLt {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    CmpGt {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    CmpLe {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    CmpGe {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    CmpEq {
        dest: Ref,
        l: Ref,
        r: Ref,
    },
    CmpNe {
        dest: Ref,
        l: Ref,
        r: Ref,
    },

    Negate {
        dest: Ref,
        value: Ref,
    },
    Not {
        dest: Ref,
        value: Ref,
    },

    Cast {
        dest: Ref,
        value: Ref,
        ty: Ref,
    },
    TypeOf {
        dest: Ref,
        value: Ref,
    },

    // LoadConstants { dest: Ref, index: ConstantIndex },
    LoadBuiltin {
        dest: Ref,
        builtin: Builtin,
    },

    Load {
        dest: Ref,
        reg: Ref,
    },
    Store {
        reg: Ref,
        value: Ref,
    },

    // LoadDecl { dest: Ref, index: DeclIndex },
    // StoreDecl { index: DeclIndex, value: Ref },
    Ref {
        dest: Ref,
        value: Ref,
    },
    Deref {
        dest: Ref,
        value: Ref,
    },

    TypedValue {
        dest: Ref,
        value: Ref,
        ty: Ref,
    },

    Call {
        dest: Ref,
        func: Ref,
        args: Vec<Ref>,
    },
    // TODO(Implement the rest of the instructions)
}

pub struct Branch {
    pattern: Ref,
    loc: Loc,
}

pub enum Terminator {
    Goto(Loc),
    If { cond: Ref, t: Loc, f: Loc },
    Match { value: Ref, branches: Vec<Branch> },
    Return { value: Ref },
    ImplicitReturn { value: Ref },
}

#[derive(Clone, Copy)]
pub struct Ref(usize);

#[derive(Clone, Copy)]
pub struct Loc(usize);

pub struct BasicBlock {
    instructions: Vec<Instruction>,
    terminator: Terminator,
    comptime: bool,
}

pub struct CFG {
    blocks: Vec<BasicBlock>,
    register_count: usize,
}

pub struct UIR {}

impl UIR {
    pub fn new() -> Self {
        Self {}
    }

    pub fn pretty_print(&self) {
        todo!()
    }
}
