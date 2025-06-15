use std::{
    collections::{BTreeMap, HashMap},
    mem,
};

use bumpalo::Bump;
use itertools::Itertools;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    scope::ScopeArena,
    ty::{FnType, Record, Type, TypeContext, TypeID},
};

use super::{
    BasicBlock, BinOp, CFG, ConstKind, EntryPoint, ExternParam, Func, FuncIdx, Loc, Operand, Param,
    Place, RIL, RValue, Statement, Terminator, TypeDef, TypeRef, UnOp,
};

struct BasicBlockBuilder<'a> {
    loc: Loc,
    statements: Vec<Statement<'a>>,
}

impl<'a> BasicBlockBuilder<'a> {
    fn new(loc: Loc) -> Self {
        Self {
            loc,
            statements: vec![],
        }
    }

    fn append(&mut self, stmt: Statement<'a>) {
        self.statements.push(stmt);
    }

    fn build(self, terminator: Terminator<'a>) -> BasicBlock<'a> {
        BasicBlock {
            statements: self.statements,
            terminator,
        }
    }
}

struct CFGBuilder<'ctx: 'a, 'a> {
    register_count: usize,
    blocks: BTreeMap<Loc, BasicBlock<'ctx>>,
    mapping: HashMap<Place<'ctx>, TypeID>,
    current_bb: usize,
    scope: ScopeArena<'ctx, Place<'ctx>>,
    builder: &'a Builder<'ctx, 'a>,
}

impl<'ctx, 'a> CFGBuilder<'ctx, 'a> {
    fn new(builder: &'a Builder<'ctx, 'a>) -> Self {
        Self {
            register_count: 0,
            blocks: BTreeMap::new(),
            mapping: HashMap::new(),
            current_bb: 0,
            scope: ScopeArena::new(),
            builder,
        }
    }

    fn make_place(&mut self, ty: TypeID) -> Place<'ctx> {
        let index = self.register_count;
        self.register_count += 1;
        let place = Place(index, &[]); //self.arena.alloc(Place(index, vec![]));
        self.mapping.insert(Place(index, &[]), ty);
        place
    }

    fn make_block(&mut self) -> BasicBlockBuilder<'ctx> {
        let loc = self.get_next_loc();
        BasicBlockBuilder::new(loc)
    }

    fn terminate(&mut self, block: BasicBlockBuilder<'ctx>, terminator: Terminator<'ctx>) -> Loc {
        let loc = block.loc;
        self.blocks.insert(loc, block.build(terminator));
        loc
    }

    fn terminate_replace(
        &mut self,
        block: &mut BasicBlockBuilder<'ctx>,
        terminator: Terminator<'ctx>,
    ) -> Loc {
        let block = mem::replace(block, self.make_block());
        let loc = block.loc;
        self.blocks.insert(loc, block.build(terminator));
        loc
    }

    fn get_next_loc(&mut self) -> Loc {
        let idx = Loc(self.current_bb);
        self.current_bb += 1;
        idx
    }

    fn build(self) -> CFG<'ctx> {
        let blocks: TiVec<Loc, BasicBlock> = self.blocks.into_iter().map(|x| x.1).collect();

        CFG {
            blocks,
            mapping: self.mapping,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct FuncDef(FuncIdx, TypeID);

struct FunctionMapping<'a> {
    data: HashMap<&'a str, FuncDef>,
}

impl<'a> FunctionMapping<'a> {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> FuncDef {
        *self.data.get(name).unwrap()
    }

    fn add(&mut self, name: &'a str, func: FuncDef) {
        self.data.insert(name, func);
    }
}

pub struct Builder<'ctx: 'a, 'a> {
    ctx: &'a TypeContext<'ctx>,
    func_mapping: FunctionMapping<'ctx>,
    funcs: TiVec<FuncIdx, Func<'ctx, 'a>>,
    types: TiVec<TypeRef, TypeDef<'ctx>>,
    arena: Bump,
    entrypoint: EntryPoint,
}

impl<'ctx, 'a> Builder<'ctx, 'a> {
    pub fn new(ctx: &'a TypeContext<'ctx>) -> Self {
        Self {
            ctx,
            func_mapping: FunctionMapping::new(),
            funcs: ti_vec![],
            types: ti_vec![],
            arena: Bump::new(),
            entrypoint: EntryPoint::None,
        }
    }

    fn append_type(&mut self, name: &'ctx str, ty: TypeID) -> TypeRef {
        self.types.push_and_get_key(TypeDef { name, ty })
    }

    pub fn build(self) -> RIL<'ctx, 'a> {
        RIL {
            funcs: self.funcs,
            entrypoint: self.entrypoint,
            types: self.types,
            ctx: self.ctx,
            arena: self.arena,
        }
    }
}
