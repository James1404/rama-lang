use std::{
    collections::{BTreeMap, HashMap},
    mem,
};

use bumpalo::Bump;
use itertools::Itertools;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    ast::{self, Literal, Node},
    scope::ScopeArena,
    tast::TypedAST,
    ty::{FnType, Struct, Type, TypeContext, TypeID},
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

    fn get_field_index(&mut self, val: Place<'ctx>, node: ast::Ref) -> usize {
        todo!()
    }

    fn get_place(&mut self, node: ast::Ref) -> Place<'ctx> {
        match self.builder.tast.get_node(node) {
            Node::Ident(ident) => self.scope.get(ident.text).unwrap(),
            Node::FieldAccess(val, field) => self.get_place(val),
            _ => panic!(),
        }
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

    fn eval_operand(&mut self, bb: &mut BasicBlockBuilder<'ctx>, node: ast::Ref) -> Operand<'ctx> {
        match self.builder.tast.get_node(node) {
            Node::Literal(lit) => {
                let ty = self.builder.tast.get_type_id(node);
                match lit {
                    Literal::String(value) => Operand::Const(ConstKind::String(value)),
                    Literal::Int(value) => Operand::Const(ConstKind::Integer(value, ty)),
                    Literal::Float(value) => Operand::Const(ConstKind::Float(value, ty)),
                    Literal::Bool(value) => Operand::Const(if value {
                        ConstKind::True
                    } else {
                        ConstKind::False
                    }),
                    Literal::Struct { fields } => {
                        let ty = self.builder.tast.get_type_id(node);
                        let dest = self.make_place(ty);

                        let fields = fields
                            .iter()
                            .map(|f| self.eval_operand(bb, f.value))
                            .collect_vec();

                        let type_args = vec![];

                        bb.append(Statement::Assign(
                            dest,
                            RValue::BuildStruct(ty, type_args, fields),
                        ));

                        Operand::Copy(dest)
                    }
                }
            }
            Node::Ident(ident) => {
                let place = self.scope.get_unchecked(ident.text);
                Operand::Copy(place)
            }
            _ => Operand::Copy(self.eval_expr(bb, node)),
        }
    }

    fn eval_rvalue(&mut self, bb: &mut BasicBlockBuilder<'ctx>, node: ast::Ref) -> RValue<'ctx> {
        match self.builder.tast.get_node(node) {
            Node::Binary { lhs: l, rhs: r, op } => {
                let lhs = self.eval_operand(bb, l);
                let rhs = self.eval_operand(bb, r);
                let op = BinOp::from(op);
                let ty = self.builder.tast.get_type_id(node);

                RValue::BinaryOp(op, lhs, rhs, ty)
            }
            Node::Unary { op, value } => {
                let value = self.eval_operand(bb, value);
                let op = UnOp::from(op);
                let ty = self.builder.tast.get_type_id(node);

                RValue::UnaryOp(op, value, ty)
            }
            Node::Cast { value, ty } => {
                let from = self.builder.tast.get_type_id(value);
                let value = self.eval_operand(bb, value);
                let into = self.builder.tast.get_type_id(ty);

                RValue::Cast { value, from, into }
            }
            _ => RValue::Use(self.eval_operand(bb, node)),
        }
    }

    fn eval_expr(&mut self, bb: &mut BasicBlockBuilder<'ctx>, node: ast::Ref) -> Place<'ctx> {
        match self.builder.tast.get_node(node) {
            Node::Ident(ident) => self.scope.get(ident.text).unwrap(),
            Node::FieldAccess(value, field) => {
                let field = self.builder.tast.get_ident(field);
                let (idx, field) = match self.builder.tast.get_ty(value) {
                    Type::Struct(Struct { fields, .. }) => fields
                        .into_iter()
                        .find_position(|f| f.name == field)
                        .unwrap(),
                    _ => panic!(),
                };

                let value = self.eval_expr(bb, value);
                todo!()
                //Place(value.0, Some(ProjectionKind::Field(idx)), field.ty.unwrap())
            }

            Node::FnCall { func, args } => {
                let name = self.builder.tast.get_ident(func);
                let func = self.builder.func_mapping.get(name);
                let args = args
                    .iter()
                    .map(|arg| self.eval_operand(bb, *arg))
                    .collect_vec();

                let ty = match self.builder.ctx.get(func.1) {
                    Type::Fn(ty) => ty,
                    _ => panic!(),
                };
                let dest = self.make_place(ty.return_ty);
                bb.append(Statement::Assign(dest, RValue::Call(func.0, args)));

                dest
            }

            Node::Block { stmts, result } => {
                for stmt in stmts {
                    self.eval_fn_stmt(bb, stmt);
                }

                if let Some(result) = result {
                    self.eval_expr(bb, result)
                } else {
                    let dest = self.make_place(self.builder.tast.get_type_id(node));
                    bb.append(Statement::Assign(
                        dest,
                        RValue::Use(Operand::Const(ConstKind::Unit)),
                    ));
                    dest
                }
            }

            Node::IfElse { cond, t, f } => {
                let dest = self.make_place(self.builder.tast.get_type_id(node));

                let cond = self.eval_expr(bb, cond);

                let mut tblock = self.make_block();
                let mut fblock = self.make_block();
                let end = self.make_block();

                let value = self.eval_expr(&mut tblock, t);
                tblock.append(Statement::Assign(dest, RValue::Use(Operand::Copy(value))));
                let t = self.terminate(tblock, Terminator::Goto(end.loc));

                let value = self.eval_expr(&mut fblock, f);
                fblock.append(Statement::Assign(dest, RValue::Use(Operand::Copy(value))));
                let f = self.terminate(fblock, Terminator::Goto(end.loc));

                let bb = mem::replace(bb, end);
                self.terminate(bb, Terminator::If { cond, t, f });

                dest
            }
            _ => {
                let value = self.eval_rvalue(bb, node);
                let dest = self.make_place(self.builder.tast.get_type_id(node));
                bb.append(Statement::Assign(dest, value));
                dest
            }
        }
    }

    fn eval_fn_stmt(&mut self, bb: &mut BasicBlockBuilder<'ctx>, node: ast::Ref) {
        match self.builder.tast.get_node(node) {
            Node::VarDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.builder.tast.get_type_id(value);
                let value = self.eval_rvalue(bb, value);
                let dest = self.make_place(ty);
                bb.append(Statement::Assign(dest, value));
                self.scope.push(self.builder.tast.get_ident(ident), dest);
            }
            Node::ConstDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.builder.tast.get_type_id(value);
                let value = self.eval_rvalue(bb, value);
                let dest = self.make_place(ty);
                bb.append(Statement::Assign(dest, value));
                self.scope.push(self.builder.tast.get_ident(ident), dest);
            }

            Node::Assignment { ident, value } => match self.builder.tast.get_node(ident) {
                Node::FieldAccess(structvalue, field) => {
                    let ty = self.builder.tast.get_type_id(field);

                    let field = self.builder.tast.get_ident(field);
                    let field = match self.builder.tast.get_ty(structvalue) {
                        Type::Struct(Struct { fields, .. }) => {
                            fields.iter().position(|f| f.name == field).unwrap()
                        }
                        _ => panic!(),
                    };

                    let place = self.eval_expr(bb, structvalue);
                    let value = self.eval_rvalue(bb, value);

                    bb.append(Statement::Assign(place, value));

                    // bb.append(Instruction::WriteField {
                    //     r#struct: structvalue,
                    //     field,
                    //     value,
                    //     ty,
                    // });
                }
                _ => {
                    let var = self.scope.get_unchecked(self.builder.tast.get_ident(ident));
                    let value = self.eval_operand(bb, value);
                    bb.append(Statement::Assign(var, RValue::Use(value)));
                }
            },

            Node::If { cond, block } => {
                let cond = self.eval_expr(bb, cond);

                let mut condblock = self.make_block();
                let end = self.make_block();

                self.eval_expr(&mut condblock, block);
                let condblock = self.terminate(condblock, Terminator::Goto(end.loc));

                self.terminate_replace(
                    bb,
                    Terminator::If {
                        cond,
                        t: condblock,
                        f: end.loc,
                    },
                );
            }

            Node::ReturnNone => {
                self.terminate_replace(bb, Terminator::ReturnNone);
            }

            Node::Return(value) => {
                let value = self.eval_operand(bb, value);
                self.terminate_replace(bb, Terminator::Return(value));
            }

            _ => {
                self.eval_expr(bb, node);
            }
        }
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
    tast: &'a TypedAST<'ctx>,
    ctx: &'a TypeContext<'ctx>,
    func_mapping: FunctionMapping<'ctx>,
    funcs: TiVec<FuncIdx, Func<'ctx, 'a>>,
    types: TiVec<TypeRef, TypeDef<'ctx>>,
    arena: Bump,
    entrypoint: EntryPoint,
}

impl<'ctx, 'a> Builder<'ctx, 'a> {
    pub fn new(tast: &'a TypedAST<'ctx>) -> Self {
        Self {
            tast,
            ctx: &tast.context,
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

    fn eval_toplevel(&mut self, node: ast::Ref) {
        match self.tast.get_node(node) {
            Node::FnDecl {
                ident,
                params: _,
                ret: _,
                block,
            } => {
                let func = self.funcs.next_key();
                let ident = self.tast.get_ident(ident);
                let ty = self.tast.get_type_id(node);

                self.func_mapping.add(ident, FuncDef(func, ty));

                let Type::Fn(FnType {
                    parameters,
                    return_ty,
                }) = self.ctx.get(ty)
                else {
                    panic!();
                };

                let mut params = Vec::<Param>::new();

                let cfg = {
                    let mut cfg = CFGBuilder::new(self);

                    for param in parameters.iter() {
                        let place = cfg.make_place(param.1);
                        cfg.scope.push(param.0, place);
                        params.push(Param {
                            name: param.0,
                            place,
                            ty: param.1,
                        });
                    }

                    let mut bb = cfg.make_block();
                    match self.tast.get_node(block) {
                        Node::Block { stmts, result } => {
                            for stmt in stmts {
                                cfg.eval_fn_stmt(&mut bb, stmt);
                            }

                            if let Some(result) = result {
                                let result = cfg.eval_expr(&mut bb, result);
                                cfg.terminate(bb, Terminator::Return(Operand::Copy(result)));
                            } else {
                                cfg.terminate(bb, Terminator::ReturnNone);
                            }
                        }
                        _ => panic!(),
                    }

                    cfg.build()
                };

                let decl = Func::Decl {
                    name: ident,
                    cfg,
                    return_ty,
                    params,
                };

                self.funcs.push(decl);
            }
            Node::ExternFnDecl {
                ident,
                params: _,
                ret: _,
            } => {
                let func = self.funcs.next_key();
                let ident = self.tast.get_ident(ident);
                let ty = self.tast.get_type_id(node);

                self.func_mapping.add(ident, FuncDef(func, ty));

                let Type::Fn(FnType {
                    parameters,
                    return_ty,
                }) = self.ctx.get(ty)
                else {
                    panic!();
                };

                let mut params = Vec::<ExternParam>::new();

                for param in parameters.iter() {
                    params.push(ExternParam {
                        name: param.0,
                        ty: param.1,
                    });
                }

                self.funcs.push(Func::Extern {
                    name: ident,
                    return_ty,
                    params,
                });
            }
            Node::Type {
                ident,
                args: _,
                body,
            } => {
                let ident = self.tast.get_ident(ident);
                let ty = self.tast.get_type_id(body);
                self.append_type(ident, ty);
            }
            _ => {}
        }
    }

    fn run(&mut self) {
        if let Some(node) = self.tast.root {
            if let Node::TopLevelScope(lst) = self.tast.get_node(node) {
                for node in lst {
                    self.eval_toplevel(node);
                }
            }
        }
    }

    pub fn build(mut self) -> RIL<'ctx, 'a> {
        self.run();

        RIL {
            funcs: self.funcs,
            entrypoint: self.entrypoint,
            types: self.types,
            ctx: self.ctx,
            arena: self.arena,
        }
    }
}
