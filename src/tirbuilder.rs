use std::collections::HashMap;

use itertools::Itertools;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    ast::{self, Literal, Node},
    lexer::TokenType,
    tir::{
        BasicBlock, Func, FuncRef, Instruction, Loc, Ref, Terminator, TypeDef, TypeRef, CFG, TIR
    },
    typed_ast::TypedAST,
    types::{Type, TypeID, ADT},
    valuescope::ScopeArena,
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

#[derive(Debug, Clone)]
enum Def {
    Local(Ref),
}

type Scope<'a> = ScopeArena<'a, Def>;

pub struct TIRBuilder<'ast> {
    tast: TypedAST<'ast>,
    scope: Scope<'ast>,
    func_mapping: HashMap<&'ast str, FuncRef>,
    funcs: TiVec<FuncRef, Func<'ast>>,
    types: TiVec<TypeRef, TypeDef<'ast>>,
}

impl<'ast> TIRBuilder<'ast> {
    pub fn new(tast: TypedAST<'ast>) -> Self {
        Self {
            tast,
            scope: Scope::new(),
            func_mapping: HashMap::new(),
            funcs: ti_vec![],
            types: ti_vec![],
        }
    }

    pub fn append_func(&mut self, name: &'ast str, ty: TypeID, cfg: CFG<'ast>) {
        let func = self.funcs.push_and_get_key(Func { name, ty, cfg });
        self.func_mapping.insert(name, func);
    }

    pub fn append_type(&mut self, name: &'ast str, ty: TypeID) -> TypeRef {
        self.types.push_and_get_key(TypeDef { name, ty })
    }

    fn get_ident(&self, node: ast::Ref) -> &'ast str {
        match self.tast.get_node(node) {
            Node::Ident(token) => token.text,
            _ => panic!(),
        }
    }

    fn get_func(&self, node: ast::Ref) -> FuncRef {
        let ident = self.get_ident(node);
        *self.func_mapping.get(ident).unwrap()
    }

    fn eval_expr(&mut self, cfg: &mut CFGBuilder<'ast>, node: ast::Ref) -> Ref {
        match self.tast.get_node(node) {
            Node::Binary { lhs, rhs, op } => match op.ty {
                TokenType::Plus => {
                    let lhs = self.eval_expr(cfg, lhs);
                    let rhs = self.eval_expr(cfg, rhs);

                    let dest = cfg.reg();
                    cfg.append(Instruction::Add { dest, lhs, rhs });

                    dest
                }
                TokenType::Minus => {
                    let lhs = self.eval_expr(cfg, lhs);
                    let rhs = self.eval_expr(cfg, rhs);

                    let dest = cfg.reg();
                    cfg.append(Instruction::Sub { dest, lhs, rhs });

                    dest
                }
                TokenType::Asterix => {
                    let lhs = self.eval_expr(cfg, lhs);
                    let rhs = self.eval_expr(cfg, rhs);

                    let dest = cfg.reg();
                    cfg.append(Instruction::Mul { dest, lhs, rhs });

                    dest
                }
                TokenType::Slash => {
                    let lhs = self.eval_expr(cfg, lhs);
                    let rhs = self.eval_expr(cfg, rhs);

                    let dest = cfg.reg();
                    cfg.append(Instruction::Div { dest, lhs, rhs });

                    dest
                }

                _ => panic!(),
            },
            Node::Literal(lit) => match lit {
                Literal::String(value) => {
                    let dest = cfg.reg();
                    cfg.append(Instruction::String { dest, value });
                    dest
                }
                Literal::Int(value) => {
                    let ty = self.tast.get_type_id(node);

                    let dest = cfg.reg();
                    cfg.append(Instruction::Integer { dest, value, ty });

                    dest
                }
                Literal::Float(value) => {
                    let ty = self.tast.get_type_id(node);

                    let dest = cfg.reg();
                    cfg.append(Instruction::Float { dest, value, ty });

                    dest
                }
                Literal::Bool(value) => {
                    let dest = cfg.reg();
                    cfg.append(Instruction::Bool { dest, value });
                    dest
                }
                Literal::Struct { fields } => {
                    let ty = self.tast.get_type_id(node);
                    let dest = cfg.reg();
                    cfg.append(Instruction::MakeStruct { dest, ty });

                    for (idx, field) in fields.iter().enumerate() {
                        let value = self.eval_expr(cfg, field.value);
                        cfg.append(Instruction::WriteField {
                            r#struct: dest,
                            field: idx,
                            value,
                            ty,
                        });
                    }

                    dest
                }
            },
            Node::Ident(ident) => {
                let Some((_, Def::Local(reg))) = self.scope.iter().find(|n| {
                    n.0 == ident.text
                        && match n.1 {
                            Def::Local(_) => true,
                            _ => false,
                        }
                }) else {
                    panic!()
                };

                reg
            }
            Node::FieldAccess(value, field) => {
                let ty = self.tast.get_type_id(field);

                let field = self.get_ident(field);
                let field = match self.tast.get_ty(value) {
                    Type::ADT(ADT { fields, .. }) => {
                        fields.iter().position(|f| f.ident == field).unwrap()
                    },
                    _ => panic!()
                };

                let value = self.eval_expr(cfg, value);

                let dest = cfg.reg();
                cfg.append(Instruction::ReadField {
                    dest,
                    r#struct: value,
                    field,
                    ty,
                });

                dest
            }

            Node::FnCall { func, args } => {
                let func = self.get_func(func);
                let args = args
                    .iter()
                    .map(|arg| self.eval_expr(cfg, *arg))
                    .collect_vec();

                let dest = cfg.reg();
                cfg.append(Instruction::Call { dest, func, args });

                dest
            }

            Node::Cast { value, ty } => {
                let from = self.tast.get_type_id(value);
                let value = self.eval_expr(cfg, value);
                let to = self.tast.get_type_id(ty);
                let dest = cfg.reg();
                cfg.append(Instruction::Cast {
                    dest,
                    value,
                    from,
                    to,
                });
                dest
            }

            node => panic!("{:?}", node),
        }
    }

    fn eval_fn_stmt(&mut self, cfg: &mut CFGBuilder<'ast>, node: ast::Ref) {
        match self.tast.get_node(node) {
            Node::VarDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.tast.get_type_id(node);
                let dest = cfg.reg();
                cfg.append(Instruction::MakeVar { dest, ty });

                let value = self.eval_expr(cfg, value);
                cfg.append(Instruction::WriteVar { var: dest, value });

                self.scope.push(self.get_ident(ident), Def::Local(dest));
            }
            Node::ConstDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.tast.get_type_id(node);
                let dest = cfg.reg();
                cfg.append(Instruction::MakeVar { dest, ty });

                let value = self.eval_expr(cfg, value);
                cfg.append(Instruction::WriteVar { var: dest, value });

                self.scope.push(self.get_ident(ident), Def::Local(dest));
            }

            Node::Scope(lst) => {
                for node in lst {
                    self.eval_fn_stmt(cfg, node);
                }
            }
            Node::Assignment { ident, value } => match self.tast.get_node(ident) {
                Node::FieldAccess(structvalue, field) => {
                    let ty = self.tast.get_type_id(field);

                    let field = self.get_ident(field);
                    let field = match self.tast.get_ty(structvalue) {
                        Type::ADT(ADT { fields, .. }) => {
                            fields.iter().position(|f| f.ident == field).unwrap()
                        },
                        _ => panic!()
                    };

                    let structvalue = self.eval_expr(cfg, structvalue);
                    let value = self.eval_expr(cfg, value);

                    cfg.append(Instruction::WriteField {
                        r#struct: structvalue,
                        field,
                        value,
                        ty,
                    });
                }
                _ => {
                    self.eval_expr(cfg, value);
                }
            },

            Node::ReturnNone => {
                cfg.finish_block(Terminator::ReturnNone);
            }

            Node::Return(value) => {
                let value = self.eval_expr(cfg, value);
                cfg.finish_block(Terminator::Return(value));
            }
            _ => todo!(),
        }
    }

    fn eval_toplevel(&mut self, node: ast::Ref) {
        match self.tast.get_node(node) {
            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                let mut cfg = CFGBuilder::new();
                self.eval_fn_stmt(&mut cfg, block);

                let ident = self.get_ident(ident);
                let ty = self.tast.get_type_id(node);
                self.append_func(ident, ty, cfg.build());
            }
            Node::Type { ident, args, body } => {
                let ident = self.get_ident(ident);
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
