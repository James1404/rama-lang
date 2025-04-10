#![allow(dead_code)]

pub mod error;
mod frame;

use crate::{
    ast::{self, ASTView, Literal, Node},
    lexer::TokenType,
    tir::{CFGBuilder, FuncRef, Instruction, Ref, TIR, TIRBuilder, Terminator, TypeRef},
    types::{ADT, ADTKind, Field, FloatKind, FnType, IntSize, Type, TypeContext, TypeID},
    valuescope::ScopeArena,
};

pub use error::{Result, SemaError};
use frame::Frame;
use itertools::{Itertools, izip};

#[derive(Debug, Clone)]
enum Def {
    Const(Ref, TypeID),
    Var(Ref, TypeID),
    Fn(FuncRef, TypeID),
    Type(TypeRef, TypeID),
}

type Scope<'a> = ScopeArena<'a, Def>;

pub struct Sema<'ast> {
    ast: ASTView<'ast>,
    scopes: Scope<'ast>,

    ctx: TypeContext<'ast>,
    callstack: Vec<Frame>,

    builder: TIRBuilder<'ast>,
}

impl<'ast> Sema<'ast> {
    pub fn new(ast: ASTView<'ast>) -> Self {
        let ctx = TypeContext::new();

        Self {
            ast,
            ctx,
            scopes: Scope::new(),
            callstack: vec![],
            builder: TIRBuilder::new(),
        }
    }

    fn subtype(&self, lhs: TypeID, rhs: TypeID) -> bool {
        match (self.ctx.get(lhs), self.ctx.get(rhs)) {
            (Type::Int { .. }, Type::Int { .. }) => true,
            (Type::Float(_), Type::Float(_)) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Slice(t1), Type::Slice(t2)) => self.subtype(t1, t2),
            (Type::ADT(adt1), Type::ADT(adt2)) => {
                if adt1.kind != adt2.kind {
                    return false;
                }

                for (f1, f2) in izip!(adt1.fields, adt2.fields) {
                    match (f1.ty, f2.ty) {
                        (Some(f1), Some(f2)) => {
                            if !self.subtype(f1, f2) {
                                return false;
                            }
                        }
                        _ => {}
                    }
                }

                true
            }
            _ => false,
        }
    }

    fn add(
        &mut self,
        cfg: &mut CFGBuilder<'ast>,
        lterm: ast::Ref,
        rterm: ast::Ref,
    ) -> Result<'ast, (Ref, TypeID)> {
        let lhs = self.infer(cfg, lterm)?;
        let rhs = self.infer(cfg, rterm)?;
        let dest = cfg.reg();

        cfg.append(Instruction::Add {
            dest,
            lhs: lhs.0,
            rhs: rhs.0,
        });

        match (self.ctx.get(lhs.1), self.ctx.get(rhs.1)) {
            (Type::Int { .. }, Type::Int { .. }) => Ok((dest, lhs.1)),
            (Type::Float(_), Type::Float(_)) => Ok((dest, lhs.1)),
            _ => Err(SemaError::Err(format!(
                "Cannot add types {} and {}",
                self.ctx.display(lhs.1),
                self.ctx.display(rhs.1)
            ))),
        }
    }

    fn sub(
        &mut self,
        cfg: &mut CFGBuilder<'ast>,
        lterm: ast::Ref,
        rterm: ast::Ref,
    ) -> Result<'ast, (Ref, TypeID)> {
        let lhs = self.infer(cfg, lterm)?;
        let rhs = self.infer(cfg, rterm)?;
        let dest = cfg.reg();

        cfg.append(Instruction::Sub {
            dest,
            lhs: lhs.0,
            rhs: rhs.0,
        });

        match (self.ctx.get(lhs.1), self.ctx.get(rhs.1)) {
            (Type::Int { .. }, Type::Int { .. }) => Ok((dest, lhs.1)),
            (Type::Float(_), Type::Float(_)) => Ok((dest, lhs.1)),
            _ => Err(SemaError::Err(format!(
                "Cannot subtract types {} and {}",
                self.ctx.display(lhs.1),
                self.ctx.display(rhs.1)
            ))),
        }
    }

    fn mul(
        &mut self,
        cfg: &mut CFGBuilder<'ast>,
        lterm: ast::Ref,
        rterm: ast::Ref,
    ) -> Result<'ast, (Ref, TypeID)> {
        let lhs = self.infer(cfg, lterm)?;
        let rhs = self.infer(cfg, rterm)?;
        let dest = cfg.reg();

        cfg.append(Instruction::Mul {
            dest,
            lhs: lhs.0,
            rhs: rhs.0,
        });

        match (self.ctx.get(lhs.1), self.ctx.get(rhs.1)) {
            (Type::Int { .. }, Type::Int { .. }) => Ok((dest, lhs.1)),
            (Type::Float(_), Type::Float(_)) => Ok((dest, lhs.1)),
            _ => Err(SemaError::Err(format!(
                "Cannot multiply types {} and {}",
                self.ctx.display(lhs.1),
                self.ctx.display(rhs.1)
            ))),
        }
    }

    fn div(
        &mut self,
        cfg: &mut CFGBuilder<'ast>,
        lterm: ast::Ref,
        rterm: ast::Ref,
    ) -> Result<'ast, (Ref, TypeID)> {
        let lhs = self.infer(cfg, lterm)?;
        let rhs = self.infer(cfg, rterm)?;
        let dest = cfg.reg();

        cfg.append(Instruction::Div {
            dest,
            lhs: lhs.0,
            rhs: rhs.0,
        });

        match (self.ctx.get(lhs.1), self.ctx.get(rhs.1)) {
            (Type::Int { .. }, Type::Int { .. }) => Ok((dest, lhs.1)),
            (Type::Float(_), Type::Float(_)) => Ok((dest, lhs.1)),
            _ => Err(SemaError::Err(format!(
                "Cannot divide types {} and {}",
                self.ctx.display(lhs.1),
                self.ctx.display(rhs.1)
            ))),
        }
    }

    fn check(
        &mut self,
        cfg: &mut CFGBuilder<'ast>,
        term: ast::Ref,
        against: TypeID,
    ) -> Result<'ast, Ref> {
        let (reg, ty) = self.infer(cfg, term)?;
        if self.subtype(ty, against) {
            Ok(reg)
        } else {
            Err(SemaError::Err(format!(
                "Type {} is not compatible with {}",
                self.ctx.display(ty),
                self.ctx.display(against)
            )))
        }
    }

    fn infer(&mut self, cfg: &mut CFGBuilder<'ast>, term: ast::Ref) -> Result<'ast, (Ref, TypeID)> {
        match self.ast.get(term) {
            Node::Binary { lhs, rhs, op } => match op.ty {
                TokenType::Plus => self.add(cfg, lhs, rhs),
                TokenType::Minus => self.sub(cfg, lhs, rhs),
                TokenType::Asterix => self.mul(cfg, lhs, rhs),
                TokenType::Slash => self.div(cfg, lhs, rhs),
                _ => Err(SemaError::InvalidType),
            },
            Node::Unary { value, op } => match op.ty {
                TokenType::Plus | TokenType::MinusEq => {
                    let dest = cfg.reg();
                    let value = self.infer(cfg, value)?;
                    cfg.append(Instruction::Negate {
                        dest,
                        value: value.0,
                    });

                    Ok(value)
                }
                _ => Err(SemaError::InvalidTerm(term)),
            },

            Node::Literal(lit) => match lit {
                Literal::String(value) => {
                    let ty = self
                        .ctx
                        .alloc_array(Type::uint(IntSize::Bits8), value.len());

                    let dest = cfg.reg();
                    cfg.append(Instruction::String { dest, value });

                    Ok((dest, ty))
                }
                Literal::Int(value) => {
                    let ty = self.ctx.alloc(Type::int(IntSize::Bits32));

                    let dest = cfg.reg();
                    cfg.append(Instruction::Integer { dest, value, ty });

                    Ok((dest, ty))
                }
                Literal::Float(value) => {
                    let ty = self.ctx.alloc(Type::Float(FloatKind::F32));

                    let dest = cfg.reg();
                    cfg.append(Instruction::Integer { dest, value, ty });

                    Ok((dest, ty))
                }
                Literal::Bool(value) => {
                    let ty = self.ctx.alloc(Type::Bool);

                    let dest = cfg.reg();
                    cfg.append(Instruction::Bool { dest, value });

                    Ok((dest, ty))
                }
                Literal::Struct { fields } => {
                    let mut adt_fields = Vec::<(Field, Ref)>::new();
                    for node in fields {
                        let value = self.infer(cfg, node.value)?;
                        adt_fields.push((
                            Field {
                                ident: self.get_ident(node.ident)?,
                                ty: Some(value.1),
                            },
                            value.0,
                        ));
                    }

                    let adt = ADT {
                        kind: ADTKind::Struct,
                        fields: adt_fields.iter().map(|f| f.0.clone()).collect(),
                        generic_args: vec![],
                    };

                    let ty = self.ctx.alloc(Type::ADT(adt));

                    let dest = cfg.reg();
                    cfg.append(Instruction::CreateStruct {
                        dest,
                        ty,
                        fields: adt_fields
                            .iter()
                            .map(|f| (f.1, f.0.ty.unwrap()))
                            .collect_vec(),
                    });

                    Ok((dest, ty))
                }
            },
            Node::Ident(ident) => match self.scopes.get(ident.text) {
                Some(Def::Const(reg, ty)) => Ok((reg, ty)),
                Some(Def::Var(reg, ty)) => Ok((reg, ty)),
                Some(Def::Fn(index, ty)) => {
                    let dest = cfg.reg();
                    cfg.append(Instruction::FuncRef { dest, index });
                    Ok((dest, ty))
                }
                Some(Def::Type(index, ty)) => {
                    let dest = cfg.reg();
                    cfg.append(Instruction::TypeRef { dest, index });
                    Ok((dest, ty))
                }
                None => Err(SemaError::NotDefined(ident.text)),
            },
            Node::FieldAccess(value, field) => {
                let value = self.infer(cfg, value)?;
                let field = self.get_ident(field)?;

                match self.ctx.get(value.1) {
                    Type::ADT(ADT { fields, .. }) => {
                        if let Some((idx, field)) = fields.iter().find_position(|f| f.ident == field) {
                            let ty = field.ty.unwrap();
                            let dest = cfg.reg();
                            cfg.append(Instruction::GetStructField { dest, r#struct: value.0, idx, ty });
                            Ok((dest, ty))
                        }
                        else {
                            panic!("Doesnt have field")
                        }
                    },
                    _ => Err(SemaError::Err("Field does not exist on struct".to_owned()))
                }
            },

            Node::FnCall { func, args } => {
                let func = self.infer(cfg, func)?;

                if let Type::Fn(FnType {
                    parameters,
                    return_ty,
                }) = self.ctx.get(func.1)
                {
                    let args = args
                        .iter()
                        .map(|arg| self.infer(cfg, *arg))
                        .flatten()
                        .collect_vec();

                    for (param, arg) in izip!(parameters, args.clone()) {
                        if !self.subtype(arg.1, param) {
                            return Err(SemaError::Err(format!(
                                "Argument expected {} but got {}",
                                self.ctx.display(param),
                                self.ctx.display(arg.1)
                            )));
                        }
                    }

                    let dest = cfg.reg();
                    cfg.append(Instruction::Call {
                        dest,
                        func: func.0,
                        args: args.iter().map(|arg| arg.0).collect(),
                        ty: func.1,
                    });

                    Ok((dest, return_ty))
                } else {
                    Err(SemaError::Err("".to_owned()))
                }
            }

            Node::Dereference(term) => {
                let value = self.infer(cfg, term)?;

                let Type::Ptr(inner) = self.ctx.get(value.1) else {
                    return Err(SemaError::InvalidType);
                };

                let dest = cfg.reg();
                cfg.append(Instruction::Deref {
                    dest,
                    value: value.0,
                });
                Ok((dest, inner))
            }
            Node::Reference(term) => {
                let value = self.infer(cfg, term)?;
                let dest = cfg.reg();

                cfg.append(Instruction::Ref {
                    dest,
                    value: value.0,
                });

                Ok((dest, self.ctx.alloc(Type::Ptr(value.1))))
            }

            Node::Comptime(term) => self.infer(cfg, term),

            Node::Cast { value, ty } => {
                let value = self.infer(cfg, value)?;
                let ty = self.term_to_ty(ty, None)?;
                if self.can_cast(value.1, ty) {
                    let dest = cfg.reg();
                    cfg.append(Instruction::Cast {
                        dest,
                        value: value.0,
                        from: value.1,
                        to: ty,
                    });
                    Ok((dest, ty))
                } else {
                    Err(SemaError::InvalidCast {
                        from: value.1,
                        into: ty,
                    })
                }
            }

            node => {
                dbg!(node);
                Err(SemaError::InvalidTerm(term))
            }
        }
    }

    fn can_cast(&self, from: TypeID, into: TypeID) -> bool {
        match (self.ctx.get(from), self.ctx.get(into)) {
            (Type::Int { .. }, Type::Int { .. }) => true,
            (Type::Float(_), Type::Float(_)) => true,
            (Type::Int { .. }, Type::Float(_)) => true,
            (Type::Float(_), Type::Int { .. }) => true,
            (
                Type::Array {
                    inner: frominner,
                    len: fromlen,
                },
                Type::Array {
                    inner: intoinner,
                    len: intolen,
                },
            ) => fromlen == intolen && self.can_cast(frominner, intoinner),
            (Type::Slice(fromty), Type::Slice(intoty)) => self.can_cast(fromty, intoty),
            (l, r) if l == r => true,
            _ => false,
        }
    }

    fn term_to_ty(
        &mut self,
        term: ast::Ref,
        _arguments: Option<Vec<ast::Ref>>,
    ) -> Result<'ast, TypeID> {
        Ok(match self.ast.get(term) {
            Node::StructType(fields) => {
                let mut adt_fields = Vec::<Field>::new();
                for node in fields {
                    adt_fields.push(Field {
                        ident: self.get_ident(node.ident)?,
                        ty: Some(self.term_to_ty(node.ty, None)?),
                    });
                }

                let adt = ADT {
                    kind: ADTKind::Struct,
                    fields: adt_fields,
                    generic_args: vec![],
                };

                self.ctx.alloc(Type::ADT(adt))
            }
            Node::EnumType(variants) => {
                let mut adt_fields = Vec::<Field>::new();
                for node in variants {
                    adt_fields.push(Field {
                        ident: self.get_ident(node.ident)?,
                        ty: if let Some(ty) = node.ty {
                            Some(self.term_to_ty(ty, None)?)
                        } else {
                            None
                        },
                    });
                }

                let adt = ADT {
                    kind: ADTKind::Struct,
                    fields: adt_fields,
                    generic_args: vec![],
                };

                self.ctx.alloc(Type::ADT(adt))
            }
            Node::PtrType(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Ptr(inner))
            }
            Node::SliceType(inner) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Slice(inner))
            }
            Node::ArrayType(inner, len) => {
                let inner = self.term_to_ty(inner, None)?;
                self.ctx.alloc(Type::Array { inner, len })
            }

            Node::Ident(token) => match token.text {
                "void" => self.ctx.alloc(Type::Void),

                "i8" => self.ctx.alloc(Type::int(IntSize::Bits8)),
                "i16" => self.ctx.alloc(Type::int(IntSize::Bits16)),
                "i32" => self.ctx.alloc(Type::int(IntSize::Bits32)),
                "i64" => self.ctx.alloc(Type::int(IntSize::Bits64)),
                "isize" => self.ctx.alloc(Type::int(IntSize::BitsPtr)),

                "u8" => self.ctx.alloc(Type::uint(IntSize::Bits8)),
                "u16" => self.ctx.alloc(Type::uint(IntSize::Bits16)),
                "u32" => self.ctx.alloc(Type::uint(IntSize::Bits32)),
                "u64" => self.ctx.alloc(Type::uint(IntSize::Bits64)),
                "usize" => self.ctx.alloc(Type::uint(IntSize::BitsPtr)),

                "f32" => self.ctx.alloc(Type::Float(FloatKind::F32)),
                "f64" => self.ctx.alloc(Type::Float(FloatKind::F64)),

                "bool" => self.ctx.alloc(Type::Bool),

                text => match self.scopes.get(text) {
                    Some(Def::Type(_, ty)) => self.ctx.alloc(Type::Ref(ty)),
                    _ => return Err(SemaError::NotDefined(text)),
                },
            },
            _ => return Err(SemaError::InvalidTerm(term)),
        })
    }

    fn get_ident(&self, node: ast::Ref) -> Result<'ast, &'ast str> {
        match self.ast.get(node) {
            Node::Ident(token) => Ok(token.text),
            _ => Err(SemaError::InvalidTerm(node)),
        }
    }

    fn eval(&mut self, cfg: &mut CFGBuilder<'ast>, node: ast::Ref) -> Result<'ast, ()> {
        match self.ast.get(node) {
            Node::VarDecl { ident, ty, value } => {
                let (value, ty) = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    let value = self.check(cfg, value, ty)?;
                    (value, ty)
                } else {
                    self.infer(cfg, value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, Def::Var(value, ty));
            }
            Node::ConstDecl { ident, ty, value } => {
                let (value, ty) = if let Some(ty) = ty {
                    let ty = self.term_to_ty(ty, None)?;
                    let value = self.check(cfg, value, ty)?;
                    (value, ty)
                } else {
                    self.infer(cfg, value)?
                };

                let ident = self.get_ident(ident)?;
                self.scopes.push(ident, Def::Var(value, ty));
            }

            Node::Scope(lst) => {
                for node in lst {
                    self.eval(cfg, node)?;
                }
            }
            Node::Assignment { ident, value } => {
                let ty = self.infer(cfg, ident)?;
                self.check(cfg, value, ty.1)?;
            }

            Node::Return(value) => {
                let Some(frame) = self.callstack.last() else {
                    return Err(SemaError::CannotReturnOutsideOfFunction);
                };

                match frame.return_type {
                    Some(return_type) => {
                        let value = self.check(cfg, value, return_type)?;
                        cfg.finish_block(Terminator::Return(value));
                    }
                    None => return Err(SemaError::FunctionDoesNotHaveReturnType),
                }
            }
            _ => {
                self.infer(cfg, node)?;
            }
        }

        Ok(())
    }

    fn eval_toplevel(&mut self, node: ast::Ref) -> Result<'ast, ()> {
        match self.ast.get(node) {
            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                let ident = self.get_ident(ident)?;

                let returnty = self.term_to_ty(ret, None)?;

                let parameters = {
                    let mut vec = Vec::<TypeID>::new();
                    for param in params {
                        vec.push(self.term_to_ty(param.ty, None)?);
                    }

                    vec
                };

                let ty = self.ctx.alloc(Type::Fn(FnType {
                    parameters,
                    return_ty: returnty,
                }));

                self.callstack.push(Frame {
                    return_type: Some(returnty),
                });

                let func_ref = self.builder.func_ref();
                self.scopes.push(ident, Def::Fn(func_ref, ty));

                let mut cfg = CFGBuilder::new();
                self.eval(&mut cfg, block)?;
                self.builder.append_func(ident, ty, cfg.build());
            }
            Node::Type { ident, args, body } => {
                let ident = self.get_ident(ident)?;
                let ty = self.term_to_ty(body, Some(args))?;

                let type_ref = self.builder.type_ref();
                self.scopes.push(ident, Def::Type(type_ref, ty));
                self.builder.append_type(ident, ty);
            }
            _ => {}
        }

        Ok(())
    }

    pub fn run(mut self) -> (TIR<'ast>, Vec<SemaError<'ast>>) {
        let mut errors: Vec<SemaError> = vec![];

        match self.ast.root {
            Some(node) => match self.ast.get(node) {
                Node::TopLevelScope(lst) => {
                    for node in lst {
                        if let Err(err) = self.eval_toplevel(node) {
                            errors.push(err);
                        }
                    }
                }
                _ => errors.push(SemaError::InvalidRootNode(node)),
            },
            None => errors.push(SemaError::NoRootNode),
        }

        (self.builder.build(self.ctx), errors)
    }
}
