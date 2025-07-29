use std::{
    collections::{BTreeMap, HashMap},
    mem,
};

use bumpalo::Bump;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    scope::ScopeArena,
    ty::{FnType, Record, TypeRef},
};

use super::{
    BasicBlock, BinOp, ConstKind, EntryPoint, ExternParam, Func, FuncIdx, Loc, Operand, Param, Place, RValue, Statement, Terminator, TypeDef, TypeIdx, UnOp, CFG, RIL
};

pub struct BasicBlockBuilder<'a> {
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

pub struct CFGBuilder<'a> {
    register_count: usize,
    blocks: BTreeMap<Loc, BasicBlock<'a>>,
    mapping: HashMap<Place<'a>, TypeRef<'a>>,
    current_bb: usize,
    scope: ScopeArena<'a, Place<'a>>,
}

impl<'a> CFGBuilder<'a> {
    pub fn new() -> Self {
        Self {
            register_count: 0,
            blocks: BTreeMap::new(),
            mapping: HashMap::new(),
            current_bb: 0,
            scope: ScopeArena::new(),
        }
    }

    pub fn make_place(&mut self, ty: TypeRef<'a>) -> Place<'a> {
        let index = self.register_count;
        self.register_count += 1;
        let place = Place(index, &[]);
        self.mapping.insert(Place(index, &[]), ty);
        place
    }

    pub fn make_block(&mut self) -> BasicBlockBuilder<'a> {
        let loc = self.get_next_loc();
        BasicBlockBuilder::new(loc)
    }

    pub fn terminate(&mut self, block: BasicBlockBuilder<'a>, terminator: Terminator<'a>) -> Loc {
        let loc = block.loc;
        self.blocks.insert(loc, block.build(terminator));
        loc
    }

    pub fn terminate_replace(
        &mut self,
        block: &mut BasicBlockBuilder<'a>,
        terminator: Terminator<'a>,
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

    pub fn build(self) -> CFG<'a> {
        let blocks: TiVec<Loc, BasicBlock> = self.blocks.into_iter().map(|x| x.1).collect();

        CFG {
            blocks,
            mapping: self.mapping,
        }
    }
}

#[derive(Debug, Clone)]
struct FuncDef<'a>(FuncIdx, TypeRef<'a>);

struct FunctionMapping<'a> {
    data: HashMap<&'a str, FuncDef<'a>>,
}

impl<'a> FunctionMapping<'a> {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> &FuncDef<'a> {
        self.data.get(name).unwrap()
    }

    fn add(&mut self, name: &'a str, func: FuncDef<'a>) {
        self.data.insert(name, func);
    }
}

pub struct Builder<'ctx: 'a, 'a> {
    func_mapping: FunctionMapping<'ctx>,
    funcs: TiVec<FuncIdx, Func<'ctx, 'a>>,
    types: TiVec<TypeIdx, TypeDef<'ctx>>,
    arena: Bump,
    entrypoint: EntryPoint,
}

impl<'ctx, 'a> Builder<'ctx, 'a> {
    pub fn new() -> Self {
        Self {
            func_mapping: FunctionMapping::new(),
            funcs: ti_vec![],
            types: ti_vec![],
            arena: Bump::new(),
            entrypoint: EntryPoint::None,
        }
    }

    pub fn append_type(&mut self, name: &'ctx str, ty: TypeRef<'ctx>) -> TypeIdx {
        self.types.push_and_get_key(TypeDef { name, ty })
    }

    pub fn append_fn(
        &mut self,
        name: &'ctx str,
        return_ty: TypeRef<'ctx>,
        params: Vec<Param<'ctx, 'a>>,
        cfg: CFGBuilder<'ctx>,
    ) {
        self.funcs.push(Func::Decl {
            name,
            cfg: cfg.build(),
            return_ty,
            params,
        });
    }

    pub fn append_extern_fn(
        &mut self,
        name: &'ctx str,
        return_ty: TypeRef<'ctx>,
        params: Vec<ExternParam<'ctx>>,
    ) {
        self.funcs.push(Func::Extern {
            name,
            return_ty,
            params,
        });
    }

    pub fn build(self) -> RIL<'ctx, 'a> {
        RIL {
            funcs: self.funcs,
            entrypoint: self.entrypoint,
            types: self.types,
            arena: self.arena,
        }
    }
}
