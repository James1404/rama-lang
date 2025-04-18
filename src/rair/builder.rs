use std::{
    collections::{BTreeMap, HashMap},
    mem,
};

use itertools::Itertools;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    ast::{self, Literal, Node},
    rair::ProjectionKind,
    typed_ast::TypedAST,
    types::{ADT, FnType, Type, TypeContext, TypeID},
    valuescope::ScopeArena,
};

use super::{
    BasicBlock, BinOp, ConstKind, Func, FuncIdx, Loc, Operand, Param, Place, RValue, Statement, Terminator, TypeDef, TypeRef, UnOp, CFG, RIL
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

struct CFGBuilder<'a: 'b, 'b> {
    register_count: usize,
    blocks: BTreeMap<Loc, BasicBlock<'a>>,
    current_bb: usize,
    scope: ScopeArena<'a, Place>,
    tast: &'b TypedAST<'a>,
    ctx: &'b TypeContext<'a>,
    funcs: &'b FunctionMapping<'a>,
    builder: &'b Builder<'a>,
}

impl<'a, 'b> CFGBuilder<'a, 'b> {
    fn new(
        builder: &'b Builder<'a>,
        tast: &'b TypedAST<'a>,
        ctx: &'b TypeContext<'a>,
        funcs: &'b FunctionMapping<'a>,
    ) -> Self {
        Self {
            register_count: 0,
            blocks: BTreeMap::new(),
            current_bb: 0,
            scope: ScopeArena::new(),
            tast,
            ctx,
            funcs,
            builder,
        }
    }

    fn make_place(&mut self, ty: TypeID) -> Place {
        let index = self.register_count;
        self.register_count += 1;
        Place(index, None, ty)
    }

    fn make_block(&mut self) -> BasicBlockBuilder<'a> {
        let loc = self.get_next_loc();
        BasicBlockBuilder::new(loc)
    }

    fn terminate(&mut self, block: BasicBlockBuilder<'a>, terminator: Terminator<'a>) -> Loc {
        let loc = block.loc;
        self.blocks.insert(loc, block.build(terminator));
        loc
    }

    fn terminate_replace(
        &mut self,
        block: &mut BasicBlockBuilder<'a>,
        terminator: Terminator<'a>,
    ) -> Loc {
        let block = mem::replace(block, self.make_block());
        let loc = block.loc;
        self.blocks.insert(loc, block.build(terminator));
        loc
    }

    fn eval_operand(&mut self, bb: &mut BasicBlockBuilder<'a>, node: ast::Ref) -> Operand<'a> {
        match self.tast.get_node(node) {
            Node::Literal(lit) => match lit {
                Literal::String(value) => Operand::Const(ConstKind::String(value)),
                Literal::Int(value) => Operand::Const(ConstKind::Integer(value)),
                Literal::Float(value) => Operand::Const(ConstKind::Float(value)),
                Literal::Bool(value) => Operand::Const(if value {
                    ConstKind::True
                } else {
                    ConstKind::False
                }),
                _ => panic!(),
            },
            Node::Ident(ident) => {
                let place = self.scope.get_unchecked(ident.text);
                Operand::Copy(place)
            }
            _ => Operand::Copy(self.eval_expr(bb, node)),
        }
    }

    fn eval_rvalue(&mut self, bb: &mut BasicBlockBuilder<'a>, node: ast::Ref) -> RValue<'a> {
        match self.tast.get_node(node) {
            Node::Binary { lhs: l, rhs: r, op } => {
                let lhs = self.eval_operand(bb, l);
                let rhs = self.eval_operand(bb, r);
                let op = BinOp::from(op);

                RValue::BinaryOp(op, lhs, rhs)
            }
            Node::Unary { op, value } => {
                let value = self.eval_operand(bb, value);
                let op = UnOp::from(op);

                RValue::UnaryOp(op, value)
            }
            Node::Cast { value, ty } => {
                let value = self.eval_operand(bb, value);
                let ty = self.tast.get_type_id(ty);

                RValue::Cast(value, ty)
            }
            _ => RValue::Use(self.eval_operand(bb, node)),
        }
    }

    fn eval_expr(&mut self, bb: &mut BasicBlockBuilder<'a>, node: ast::Ref) -> Place {
        match self.tast.get_node(node) {
            Node::Ident(ident) => self.scope.get(ident.text).unwrap(),
            Node::FieldAccess(value, field) => {
                let field = self.tast.get_ident(field);
                let (idx, field) = match self.tast.get_ty(value) {
                    Type::ADT(ADT { fields, .. }) => fields
                        .into_iter()
                        .find_position(|f| f.ident == field)
                        .unwrap(),
                    _ => panic!(),
                };

                let value = self.eval_expr(bb, value);
                Place(value.0, Some(ProjectionKind::Field(idx)), field.ty.unwrap())
            }

            Node::FnCall { func, args } => {
                let name = self.tast.get_ident(func);
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
                    let dest = self.make_place(self.tast.get_type_id(node));
                    bb.append(Statement::Assign(
                        dest,
                        RValue::Use(Operand::Const(ConstKind::Unit)),
                    ));
                    dest
                }
            }

            Node::IfElse { cond, t, f } => {
                let dest = {
                    let dest = self.make_place(self.tast.get_type_id(node));
                    bb.append(Statement::Assign(
                        dest,
                        RValue::Use(Operand::Const(ConstKind::Unit)),
                    ));
                    dest
                };

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
                let dest = self.make_place(self.tast.get_type_id(node));
                bb.append(Statement::Assign(dest, value));
                dest
            }
        }
    }

    fn eval_fn_stmt(&mut self, bb: &mut BasicBlockBuilder<'a>, node: ast::Ref) {
        match self.tast.get_node(node) {
            Node::VarDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.tast.get_type_id(value);
                let value = self.eval_rvalue(bb, value);
                let dest = self.make_place(ty);
                bb.append(Statement::Assign(dest, value));
                self.scope.push(self.tast.get_ident(ident), dest);
            }
            Node::ConstDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.tast.get_type_id(value);
                let value = self.eval_rvalue(bb, value);
                let dest = self.make_place(ty);
                bb.append(Statement::Assign(dest, value));
                self.scope.push(self.tast.get_ident(ident), dest);
            }

            Node::Assignment { ident, value } => match self.tast.get_node(ident) {
                Node::FieldAccess(structvalue, field) => {
                    let ty = self.tast.get_type_id(field);

                    let field = self.tast.get_ident(field);
                    let field = match self.tast.get_ty(structvalue) {
                        Type::ADT(ADT { fields, .. }) => {
                            fields.iter().position(|f| f.ident == field).unwrap()
                        }
                        _ => panic!(),
                    };

                    let structvalue = self.eval_expr(bb, structvalue);
                    let value = self.eval_expr(bb, value);

                    panic!()
                    // bb.append(Instruction::WriteField {
                    //     r#struct: structvalue,
                    //     field,
                    //     value,
                    //     ty,
                    // });
                }
                _ => {
                    let var = self.scope.get_unchecked(self.tast.get_ident(ident));
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

    fn build(self) -> CFG<'a> {
        let blocks: TiVec<Loc, BasicBlock> = self.blocks.into_iter().map(|x| x.1).collect();

        CFG { blocks }
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

pub struct Builder<'ast> {
    tast: TypedAST<'ast>,
    ctx: TypeContext<'ast>,
    register_count: usize,
    func_mapping: FunctionMapping<'ast>,
    funcs: TiVec<FuncIdx, Func<'ast>>,
    types: TiVec<TypeRef, TypeDef<'ast>>,
}

impl<'ast> Builder<'ast> {
    pub fn new(tast: TypedAST<'ast>) -> Self {
        let ctx = tast.context.clone();
        Self {
            tast,
            ctx,
            register_count: 0,
            func_mapping: FunctionMapping::new(),
            funcs: ti_vec![],
            types: ti_vec![],
        }
    }

    fn append_type(&mut self, name: &'ast str, ty: TypeID) -> TypeRef {
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
                    return_ty: _,
                }) = self.ctx.get(ty)
                else {
                    panic!();
                };

                let mut params = Vec::<Param>::new();

                let cfg = {
                    let mut cfg = CFGBuilder::new(&self, &self.tast, &self.ctx, &self.func_mapping);

                    for (_, param) in parameters.iter().enumerate() {
                        let place = cfg.make_place(param.1);
                        cfg.scope.push(param.0, place);
                        params.push(Param { name: param.0, place, ty: param.1});
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

                self.funcs.push(Func {
                    name: ident,
                    ty,
                    cfg,
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
        match self.tast.root {
            Some(node) => match self.tast.get_node(node) {
                Node::TopLevelScope(lst) => {
                    for node in lst {
                        self.eval_toplevel(node);
                    }
                }
                _ => {}
            },
            None => {}
        }
    }

    pub fn build(mut self) -> RIL<'ast> {
        self.run();

        RIL {
            funcs: self.funcs,
            types: self.types,
            ctx: self.ctx,
        }
    }
}
