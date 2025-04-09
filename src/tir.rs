#![allow(dead_code)]

use derive_more::Display;
use typed_index_collections::{TiVec, ti_vec};

use crate::types::{FnType, TypeContext, TypeID};

#[derive(Debug, Clone, Copy, From, Into, Display)]
pub struct Loc(usize);
#[derive(Debug, Clone, Copy, From, Into, Display)]
pub struct Ref(usize);

#[derive(Debug, Clone, strum_macros::Display)]
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
        ty: TypeID,
        fields: Vec<(Ref, TypeID)>,
    },
    StructStore {
        r#struct: Ref,
        field: Ref,
        value: Ref,
        ty: TypeID,
    },
    StructLoad {
        dest: Ref,
        r#struct: Ref,
        idx: usize,
        ty: TypeID,
    },

    Call {
        func: FuncRef,
        args: Vec<Ref>,
    },

    // Values
    Integer(&'a str, TypeID),
    Float(&'a str, TypeID),
    Bool(bool),
    String(&'a str),
}

#[derive(Debug, Clone, Copy)]
pub enum Terminator {
    Goto(Loc),
    If { cond: Ref, t: Loc, f: Loc },
    Return(Ref),
    ImplicitReturn(Ref),
}

#[derive(Debug, Clone)]
pub struct BasicBlock<'a> {
    pub instructions: Vec<Instruction<'a>>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub struct CFG<'a> {
    pub blocks: TiVec<Loc, BasicBlock<'a>>,
    pub register_count: usize,
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
    pub ty: FnType,
    pub block: CFG<'a>,
}

pub struct TIR<'a> {
    pub ctx: TypeContext<'a>,
    funcs: TiVec<FuncRef, Func<'a>>,
    types: TiVec<TypeRef, TypeDef<'a>>,
}

impl<'a> TIR<'a> {
    pub fn new(ctx: TypeContext<'a>) -> Self {
        Self {
            funcs: ti_vec![],
            types: ti_vec![],
            ctx,
        }
    }

    pub fn get_func(&self, index: FuncRef) -> &Func<'a> {
        &self.funcs[index]
    }

    pub fn func_iter(&self) -> std::slice::Iter<'_, Func<'a>> {
        self.funcs.iter()
    }

    pub fn type_iter(&self) -> std::slice::Iter<'_, TypeDef<'a>> {
        self.types.iter()
    }
}
