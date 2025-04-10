use typed_index_collections::{TiVec, ti_vec};

use crate::{
    tir::{
        BasicBlock, CFG, Func, FuncRef, Instruction, Loc, Ref, TIR, Terminator,
        TypeDef, TypeRef,
    },
    typed_ast::TypedAST,
    types::{TypeContext, TypeID},
};

#[derive(Debug, Clone)]
pub struct BasicBlockBuilder<'a> {
    instructions: Vec<Instruction<'a>>,
}

impl<'a> BasicBlockBuilder<'a> {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
        }
    }

    pub fn append(&mut self, instruction: Instruction<'a>) {
        self.instructions.push(instruction);
    }

    pub fn build(self, terminator: Terminator) -> BasicBlock<'a> {
        BasicBlock {
            instructions: self.instructions,
            terminator,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CFGBuilder<'a> {
    pub blocks: TiVec<Loc, BasicBlock<'a>>,
    register_count: usize,
    current: BasicBlockBuilder<'a>,
}

impl<'a> CFGBuilder<'a> {
    pub fn new() -> Self {
        Self {
            blocks: ti_vec![],
            register_count: 0,
            current: BasicBlockBuilder::new(),
        }
    }

    pub fn reg(&mut self) -> Ref {
        let index = self.register_count;
        self.register_count += 1;
        Ref(index)
    }

    pub fn append(&mut self, instruction: Instruction<'a>) {
        self.current.instructions.push(instruction);
    }

    pub fn finish_block(&mut self, terminator: Terminator) {
        let block = std::mem::replace(&mut self.current, BasicBlockBuilder::new());
        self.blocks.push(block.build(terminator));
    }

    pub fn build(self) -> CFG<'a> {
        CFG {
            blocks: self.blocks,
        }
    }
}

pub struct TIRBuilder<'a> {
    tast: TypedAST<'a>,
    funcs: TiVec<FuncRef, Func<'a>>,
    types: TiVec<TypeRef, TypeDef<'a>>,
}

impl<'a> TIRBuilder<'a> {
    pub fn new(tast: TypedAST<'a>) -> Self {
        Self {
            tast,
            funcs: ti_vec![],
            types: ti_vec![],
        }
    }

    pub fn func_ref(&mut self) -> FuncRef {
        FuncRef(self.funcs.len())
    }

    pub fn append_func(&mut self, name: &'a str, ty: TypeID, cfg: CFG<'a>) -> FuncRef {
        self.funcs.push_and_get_key(Func { name, ty, cfg })
    }

    pub fn type_ref(&mut self) -> TypeRef {
        TypeRef(self.types.len())
    }

    pub fn append_type(&mut self, name: &'a str, ty: TypeID) -> TypeRef {
        self.types.push_and_get_key(TypeDef { name, ty })
    }

    pub fn build(self, ctx: TypeContext<'a>) -> TIR<'a> {
        TIR {
            funcs: self.funcs,
            types: self.types,
            ctx,
        }
    }
}
