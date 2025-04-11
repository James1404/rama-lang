use std::collections::HashMap;

use itertools::Itertools;
use typed_index_collections::{TiVec, ti_vec};

use crate::{
    ast::{self, Literal, Node},
    lexer::TokenType,
    tir::{BasicBlock, CFG, CmpKind, Func, FuncRef, Instruction, Loc, Ref, TIR, TypeDef, TypeRef},
    typed_ast::TypedAST,
    types::{ADT, FnType, Type, TypeID},
    valuescope::ScopeArena,
};

#[derive(Debug, Clone)]
enum Def {
    Arg(usize),
    Local(Ref),
}

struct CFGBuilder<'a: 'b, 'b> {
    pub instructions: TiVec<Loc, Instruction<'a>>,
    register_count: usize,
    scope: ScopeArena<'a, Def>,
    tast: &'b TypedAST<'a>,
    funcs: &'b FunctionMapping<'a>,
}

impl<'a, 'b> CFGBuilder<'a, 'b> {
    fn new(tast: &'b TypedAST<'a>, funcs: &'b FunctionMapping<'a>) -> Self {
        Self {
            instructions: ti_vec![],
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

    fn append(&mut self, instruction: Instruction<'a>) -> Loc {
        self.instructions.push_and_get_key(instruction)
    }

    fn eval_expr(&mut self, node: ast::Ref) -> Ref {
        match self.tast.get_node(node) {
            Node::Binary { lhs: l, rhs: r, op } => {
                let lhs = self.eval_expr(l);
                let rhs = self.eval_expr(r);
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
                };

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
                        let value = self.eval_expr(field.value);
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

                let value = self.eval_expr(value);

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
                let args = args.iter().map(|arg| self.eval_expr(*arg)).collect_vec();

                let dest = self.reg();
                self.append(Instruction::Call { dest, func, args });

                dest
            }

            Node::Cast { value, ty } => {
                let from = self.tast.get_type_id(value);
                let value = self.eval_expr(value);
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
        }
    }

    fn eval_fn_stmt(&mut self, node: ast::Ref) {
        match self.tast.get_node(node) {
            Node::VarDecl {
                ident,
                ty: _,
                value,
            } => {
                let ty = self.tast.get_type_id(node);
                let value = self.eval_expr(value);
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
                let value = self.eval_expr(value);
                let dest = self.reg();
                self.append(Instruction::MakeVar { dest, value, ty });
                self.scope
                    .push(self.tast.get_ident(ident), Def::Local(dest));
            }

            Node::Scope(lst) => {
                for node in lst {
                    self.eval_fn_stmt(node);
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
                    self.eval_expr(value);
                }
            },

            Node::ReturnNone => {
                self.append(Instruction::ReturnNone);
            }

            Node::Return(value) => {
                let value = self.eval_expr(value);
                self.append(Instruction::Return(value));
            }
            Node::If { cond, t, f } => {
                let cond = self.eval_expr(cond);

                let start = self.append(Instruction::If {
                    cond,
                    t: 0.into(),
                    f: 0.into(),
                });

                let tstart = self.get_next_loc();
                self.eval_fn_stmt(t);
                let t = self.append(Instruction::Nop);

                let fstart = self.get_next_loc();
                self.eval_fn_stmt(f);
                let f = self.append(Instruction::Nop);

                self.instructions[start] = Instruction::If {
                    cond,
                    t: tstart,
                    f: fstart,
                };

                let end = self.get_next_loc();

                self.instructions[t] = Instruction::Goto(end);
                self.instructions[f] = Instruction::Goto(end);
            }

            _ => {
                self.eval_expr(node);
            }
        }
    }

    fn get_next_loc(&self) -> Loc {
        self.instructions.next_key()
    }

    fn build(self) -> CFG<'a> {
        let mut leaders = Vec::<Loc>::new();
        leaders.push(0.into());

        for (idx, inst) in self.instructions.iter().enumerate() {
            match *inst {
                Instruction::If { cond: _, t, f } => {
                    leaders.push(t);
                    leaders.push(f);
                }
                Instruction::Goto(loc) => {
                    leaders.push(loc);
                }
                Instruction::ReturnNone | Instruction::Return(_) => {}
                _ => continue,
            }

            leaders.push(Loc(idx));
        }
        leaders.sort();

        let mut blocks = TiVec::<Loc, BasicBlock>::new();

        let mut iter = leaders.iter().peekable();
        while let Some(leader) = iter.next() {
            match iter.peek() {
                Some(_) => blocks.push(BasicBlock {
                    instructions: Vec::from_iter(
                        self.instructions[*leader..**iter.peek().unwrap()]
                            .iter()
                            .cloned(),
                    ),
                }),
                None => {}
            }
        }

        CFG { blocks }
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
                self.func_mapping.add(ident, func);

                let ty = self.tast.get_type_id(node);

                let Type::Fn(FnType {
                    parameters,
                    return_ty: _,
                }) = self.tast.get_ty(node)
                else {
                    panic!();
                };

                let cfg = {
                    let mut cfg = CFGBuilder::new(&self.tast, &self.func_mapping);
                    for (idx, param) in parameters.iter().enumerate() {
                        cfg.scope.push(param.0, Def::Arg(idx));
                    }

                    cfg.eval_fn_stmt(block);

                    cfg.build()
                };

                let func = Func {
                    name: ident,
                    ty,
                    cfg,
                };

                self.funcs.push(func);
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
