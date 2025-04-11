use std::collections::HashMap;

use itertools::Itertools;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    ast::{self, Literal, Node},
    lexer::TokenType,
    tir::{
        BasicBlock, CFG, CmpKind, Func, FuncRef, Instruction, Loc, Ref, TIR, Terminator, TypeDef,
        TypeRef,
    },
    typed_ast::TypedAST,
    types::{ADT, FnType, Type, TypeID},
    valuescope::ScopeArena,
};

#[derive(Debug, Clone)]
pub struct BasicBlockBuilder<'a> {
    instructions: Vec<Instruction<'a>>,
}

struct BlockAnd<'a, T>(BasicBlockBuilder<'a>, T);

macro_rules! unpack {
    ($x:ident = $c:expr) => {{
        let BlockAnd(b, v) = $c;

        $x = b;

        v
    }};
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
enum Def {
    Arg(usize),
    Local(Ref),
}

pub struct CFGBuilder<'a> {
    pub blocks: TiVec<Loc, BasicBlock<'a>>,
    current: BasicBlockBuilder<'a>,
    register_count: usize,
    scope: ScopeArena<'a, Def>,
    tast: &'a TypedAST<'a>,
    funcs: &'a FunctionMapping<'a>,
}

impl<'a> CFGBuilder<'a> {
    pub fn new(tast: &'a TypedAST<'a>, funcs: &'a FunctionMapping<'a>) -> Self {
        Self {
            blocks: ti_vec![],
            current: BasicBlockBuilder::new(),
            register_count: 0,
            scope: ScopeArena::new(),
            tast,
            funcs,
        }
    }

    fn reg(&mut self) -> Ref {
        let index = self.register_count;
        self.register_count += 1;
        Ref(index)
    }

    pub fn append(&mut self, instruction: Instruction<'a>) {
        self.current.instructions.push(instruction);
    }

    fn get_block(&'a mut self, loc: Loc) -> &'a mut BasicBlock<'a> {
        &mut self.blocks[loc]
    }

    pub fn finish_block(&mut self, terminator: Terminator) -> Loc {
        let block = std::mem::replace(&mut self.current, BasicBlockBuilder::new());
        self.blocks.push_and_get_key(block.build(terminator))
    }

    fn eval_expr(&mut self, mut block: BasicBlockBuilder<'a>, node: ast::Ref) -> BlockAnd<'a, Ref> {
        let dest = match self.tast.get_node(node) {
            Node::Binary { lhs: l, rhs: r, op } => {
                let lhs = unpack!(block = self.eval_expr(block, l));
                let rhs = unpack!(block = self.eval_expr(block, r));
                let dest = self.reg();

                match op.ty {
                    TokenType::Plus => self.append(Instruction::Add { dest, lhs, rhs }),
                    TokenType::Minus => self.append(Instruction::Sub { dest, lhs, rhs }),
                    TokenType::Asterix => self.append(Instruction::Mul { dest, lhs, rhs }),
                    TokenType::Slash => self.append(Instruction::Div { dest, lhs, rhs }),

                    TokenType::EqualEqual => self.append(Instruction::Cmp {
                        dest,
                        lhs,
                        rhs,
                        kind: CmpKind::Equal,
                        ty: self.tast.get_type_id(l),
                    }),
                    TokenType::NotEqual => self.append(Instruction::Cmp {
                        dest,
                        lhs,
                        rhs,
                        kind: CmpKind::NotEqual,
                        ty: self.tast.get_type_id(l),
                    }),
                    TokenType::Less => self.append(Instruction::Cmp {
                        dest,
                        lhs,
                        rhs,
                        kind: CmpKind::LessThan,
                        ty: self.tast.get_type_id(l),
                    }),
                    TokenType::LessEq => self.append(Instruction::Cmp {
                        dest,
                        lhs,
                        rhs,
                        kind: CmpKind::LessEqual,
                        ty: self.tast.get_type_id(l),
                    }),
                    TokenType::Greater => self.append(Instruction::Cmp {
                        dest,
                        lhs,
                        rhs,
                        kind: CmpKind::GreaterThan,
                        ty: self.tast.get_type_id(l),
                    }),
                    TokenType::GreaterEq => self.append(Instruction::Cmp {
                        dest,
                        lhs,
                        rhs,
                        kind: CmpKind::GreaterEqual,
                        ty: self.tast.get_type_id(l),
                    }),

                    _ => panic!(),
                }

                dest
            }
            Node::Literal(lit) => match lit {
                Literal::String(value) => {
                    let dest = self.reg();
                    self.append(Instruction::String { dest, value });
                    dest
                }
                Literal::Int(value) => {
                    let ty = self.tast.get_type_id(node);

                    let dest = self.reg();
                    self.append(Instruction::Integer { dest, value, ty });

                    dest
                }
                Literal::Float(value) => {
                    let ty = self.tast.get_type_id(node);

                    let dest = self.reg();
                    self.append(Instruction::Float { dest, value, ty });

                    dest
                }
                Literal::Bool(value) => {
                    let dest = self.reg();
                    self.append(Instruction::Bool { dest, value });
                    dest
                }
                Literal::Struct { fields } => {
                    let ty = self.tast.get_type_id(node);
                    let dest = self.reg();
                    self.append(Instruction::MakeStruct { dest, ty });

                    for (idx, field) in fields.iter().enumerate() {
                        let value = unpack!(block = self.eval_expr(block, field.value));
                        self.append(Instruction::WriteField {
                            r#struct: dest,
                            field: idx,
                            value,
                            ty,
                        });
                    }

                    dest
                }
            },
            Node::Ident(ident) => match self.scope.get(ident.text) {
                Some(Def::Local(reg)) => reg,
                Some(Def::Arg(index)) => {
                    let dest = self.reg();
                    self.append(Instruction::ReadArg { dest, index });

                    dest
                }
                None => panic!(),
            },
            Node::FieldAccess(value, field) => {
                let ty = self.tast.get_type_id(field);

                let field = self.tast.get_ident(field);
                let field = match self.tast.get_ty(value) {
                    Type::ADT(ADT { fields, .. }) => {
                        fields.iter().position(|f| f.ident == field).unwrap()
                    }
                    _ => panic!(),
                };

                let value = unpack!(block = self.eval_expr(block, value));

                let dest = self.reg();
                self.append(Instruction::ReadField {
                    dest,
                    r#struct: value,
                    field,
                    ty,
                });

                dest
            }

            Node::FnCall { func, args } => {
                let name = self.tast.get_ident(func);
                let func = self.funcs.get(name);
                let args = args.iter().map(|arg| unpack!(block = self.eval_expr(block, *arg))).collect_vec();

                let dest = self.reg();
                self.append(Instruction::Call { dest, func, args });

                dest
            }

            Node::Cast { value, ty } => {
                let from = self.tast.get_type_id(value);
                let value = unpack!(block = self.eval_expr(block, value));
                let to = self.tast.get_type_id(ty);
                let dest = self.reg();
                self.append(Instruction::Cast {
                    dest,
                    value,
                    from,
                    to,
                });
                dest
            }

            node => panic!("{:?}", node),
        };

        BlockAnd(block, dest)
    }

    fn eval_fn_stmt(
        &mut self,
        mut block: BasicBlockBuilder<'a>,
        node: ast::Ref,
    ) -> BasicBlockBuilder<'a> {
        match self.tast.get_node(node) {
            Node::VarDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.tast.get_type_id(node);
                let value = unpack!(block = self.eval_expr(block, value));
                let dest = self.reg();
                self.append(Instruction::MakeVar { dest, value, ty });
                self.scope
                    .push(self.tast.get_ident(ident), Def::Local(dest));
            }
            Node::ConstDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.tast.get_type_id(node);
                let value = self.eval_expr(block, value);
                let dest = self.reg();
                self.append(Instruction::MakeVar { dest, value, ty });
                self.scope
                    .push(self.tast.get_ident(ident), Def::Local(dest));
            }

            Node::Scope(lst) => {
                for node in lst {
                    unpack!(block = self.eval_fn_stmt(block, node));
                }
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

                    let structvalue = self.eval_expr(structvalue);
                    let value = self.eval_expr(value);

                    self.append(Instruction::WriteField {
                        r#struct: structvalue,
                        field,
                        value,
                        ty,
                    });
                }
                _ => {
                    self.eval_expr(block, value);
                }
            },

            Node::ReturnNone => {
                self.finish_block(Terminator::ReturnNone);
            }

            Node::Return(value) => {
                let value = unpack!(block = self.eval_expr(block, value));
                self.finish_block(Terminator::Return(value));
            }
            Node::If { cond, t, f } => {
                let cond = unpack!(block = self.eval_expr(block, cond));

                let start = self.finish_block(Terminator::If {
                    cond,
                    t: Loc(0),
                    f: Loc(0),
                });

                self.eval_fn_stmt(cfg, t);
                let t = self.finish_block(Terminator::Goto(Loc(0)));

                self.eval_fn_stmt(cfg, f);
                let f = self.finish_block(Terminator::Goto(Loc(0)));

                match self.blocks[start].terminator {
                    Terminator::If {
                        cond: _,
                        t: ref mut tloc,
                        f: ref mut floc,
                    } => {
                        *tloc = t;
                        *floc = f;
                    }
                    _ => panic!(),
                };

                match self.blocks[t].terminator {
                    Terminator::Goto(ref mut loc) => *loc = t,
                    _ => {}
                };

                match self.blocks[f].terminator {
                    Terminator::Goto(ref mut loc) => *loc = f,
                    _ => {}
                };
            }

            _ => {
                unpack!(block = self.eval_expr(block, node));
            }
        }

        block
    }

    pub fn build(self) -> CFG<'a> {
        CFG {
            blocks: self.blocks,
        }
    }
}

struct FunctionMapping<'a> {
    data: HashMap<&'a str, FuncRef>,
}

impl<'a> FunctionMapping<'a> {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> FuncRef {
        *self.data.get(name).unwrap()
    }

    fn add(&mut self, name: &'a str, func: FuncRef) {
        self.data.insert(name, func);
    }
}

pub struct TIRBuilder<'ast> {
    tast: TypedAST<'ast>,
    register_count: usize,
    func_mapping: FunctionMapping<'ast>,
    funcs: TiVec<FuncRef, Func<'ast>>,
    types: TiVec<TypeRef, TypeDef<'ast>>,
}

impl<'ast> TIRBuilder<'ast> {
    pub fn new(tast: TypedAST<'ast>) -> Self {
        Self {
            tast,
            register_count: 0,
            func_mapping: FunctionMapping::new(),
            funcs: ti_vec![],
            types: ti_vec![],
        }
    }

    fn reset_reg(&mut self) {
        self.register_count = 0;
    }

    pub fn append_type(&mut self, name: &'ast str, ty: TypeID) -> TypeRef {
        self.types.push_and_get_key(TypeDef { name, ty })
    }

    fn eval_toplevel(&mut self, node: ast::Ref) {
        match self.tast.get_node(node) {
            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                let func = self.funcs.next_key();
                let ident = self.tast.get_ident(ident);
                self.func_mapping.add(ident, func);

                let ty = self.tast.get_type_id(node);

                let Type::Fn(FnType {
                    parameters,
                    return_ty: _,
                }) = self.tast.get_ty(node)
                else {
                    panic!();
                };

                let mut cfg = CFGBuilder::new(&self.tast, &self.func_mapping);
                for (idx, param) in parameters.iter().enumerate() {
                    cfg.scope.push(param.0, Def::Arg(idx));
                }

                self.reset_reg();
                cfg.eval_fn_stmt(block);

                self.funcs.push(Func {
                    name: ident,
                    ty,
                    cfg: cfg.build(),
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

    pub fn build(mut self) -> TIR<'ast> {
        self.run();

        TIR {
            funcs: self.funcs,
            types: self.types,
            ctx: self.tast.context,
        }
    }
}
