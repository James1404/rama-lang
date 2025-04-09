use std::{collections::HashMap, ffi::CString};

use crate::{
    lexer::TokenType,
    metadata::Metadata,
    tir::{CFG, FuncRef, Instruction, Ref, TIR, TypeDef},
    typed_ast::TypedAST,
    types::{ADTKind, FloatKind, FnType, IntSize, Type, TypeID},
    valuescope::ScopeArena,
};

extern crate llvm_sys as llvm;

use itertools::Itertools;
use llvm::{core::*, transforms::pass_builder::*, *};
use llvm_sys::target_machine::{
    LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple,
    LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetRef,
};
use typed_index_collections::{TiVec, ti_vec};

#[derive(Debug, Clone)]
pub enum Def {
    Local(*mut LLVMValue),
    Type(*mut LLVMType),
}

type Scope<'a> = ScopeArena<'a, Def>;

struct CFGMapping {
    data: TiVec<Ref, *mut LLVMValue>,
}

impl CFGMapping {
    fn new() -> Self {
        Self { data: ti_vec![] }
    }

    fn push(&mut self, value: *mut LLVMValue) {
        self.data.push(value);
    }

    fn get(&mut self, value: Ref) -> *mut LLVMValue {
        self.data[value]
    }
}

pub struct Codegen<'a> {
    tir: TIR<'a>,
    scopes: Scope<'a>,

    functions_mapping: TiVec<FuncRef, *mut LLVMValue>,

    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
}

impl<'a> Drop for Codegen<'a> {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.builder);
            core::LLVMDisposeModule(self.module);
            core::LLVMContextDispose(self.context);
        }
    }
}

impl<'a> Codegen<'a> {
    pub fn new(tir: TIR<'a>, metadata: Metadata<'a>) -> Self {
        let name = CString::new(metadata.filename.to_str().unwrap()).unwrap();

        let context = unsafe { core::LLVMContextCreate() };
        let module = unsafe { core::LLVMModuleCreateWithName(name.as_ptr() as *const _) };
        let builder = unsafe { core::LLVMCreateBuilderInContext(context) };

        Self {
            tir,
            scopes: Scope::new(),
            functions_mapping: ti_vec![],
            context,
            module,
            builder,
        }
    }

    fn eval_value(&self, builder: *mut LLVMBuilder, node_ref: Ref) -> *mut LLVMValue {
        let (node, og_ty) = self.ast.get(node_ref);
        let ty = self.type_to_llvm(og_ty);

        unsafe {
            match node {
                Node::Literal(Literal::Int(v)) => {
                    LLVMConstIntOfString(ty, CString::new(v).unwrap().as_ptr(), 10)
                }
                Node::Literal(Literal::Float(v)) => LLVMConstRealOfString(
                    LLVMFloatTypeInContext(self.context),
                    CString::new(v).unwrap().as_ptr(),
                ),
                Node::Literal(Literal::Bool(v)) => {
                    LLVMConstInt(LLVMFloatTypeInContext(self.context), v as u64, false as i32)
                }
                Node::Literal(Literal::String(v)) => {
                    let name = CString::new(format!("global_string_{}", node_ref.0)).unwrap();
                    let global = LLVMAddGlobal(self.module, ty, name.as_ptr());

                    LLVMSetLinkage(global, LLVMLinkage::LLVMInternalLinkage);
                    LLVMSetGlobalConstant(global, true as i32);

                    let text = CString::new(v).unwrap();
                    let value =
                        LLVMConstString(text.as_ptr(), text.count_bytes() as u32, false as i32);

                    LLVMSetInitializer(global, value);

                    global
                }
                Node::Literal(Literal::Struct { fields }) => {
                    let name = CString::new(format!("struct_{}", node_ref.0)).unwrap();
                    let alloca = LLVMBuildAlloca(builder, ty, name.as_ptr());

                    for (idx, field) in fields.iter().enumerate() {
                        let name = CString::new(format!("struct_{}_{}", node_ref.0, idx)).unwrap();
                        let alloca =
                            LLVMBuildStructGEP2(builder, ty, alloca, idx as u32, name.as_ptr());

                        let value = self.eval_expression(builder, field.value);
                        LLVMBuildStore(builder, value, alloca);
                    }

                    alloca
                }
                Node::Ident(token) => {
                    let ident = CString::new(token.text).unwrap();

                    let alloca = self.scopes.get_local(token.text.to_string()).expect(
                        format!("Variable \"{}\" does not exists in scope", token.text).as_str(),
                    );
                    LLVMBuildLoad2(builder, ty, alloca, ident.as_ptr())
                }

                Node::FieldAccess(value, field) => {
                    let ident = self.get_ident(value);
                    let type_id = self.ast.get_type_id(value);

                    let field = self.get_ident(field);

                    let idx = match self.ast.get_ty(value) {
                        Type::ADT(adt) => adt.fields.iter().position(|f| f.ident == field).unwrap(),
                        _ => panic!(),
                    };

                    let ty = self.type_to_llvm(type_id);

                    let alloca = self.scopes.get_local(ident.clone()).expect(
                        format!("Variable \"{}\" does not exists in scope", ident).as_str(),
                    );

                    let name = CString::new(format!("struct_{}_{}", node_ref.0, type_id)).unwrap();
                    let alloca =
                        LLVMBuildStructGEP2(builder, ty, alloca, idx as u32, name.as_ptr());
                    let name = CString::new(format!("struct_{}_{}", node_ref.0, ident)).unwrap();
                    LLVMBuildLoad2(builder, ty, alloca, name.as_ptr())
                }

                Node::FnCall {
                    func: func_ref,
                    args,
                } => {
                    let ty = self.type_to_llvm(self.ast.get_type_id(func_ref));

                    let name = self.get_ident(func_ref);
                    let func = self
                        .scope
                        .get_local(name.clone())
                        .expect(format!("Variable \"{}\" does not exists in scope", name).as_str());

                    let mut arguments = vec![];
                    for arg in args {
                        arguments.push(self.eval_expression(builder, arg));
                    }

                    let arguments = arguments.as_mut_slice();

                    LLVMBuildCall2(
                        builder,
                        ty,
                        func,
                        arguments.as_mut_ptr(),
                        arguments.len() as u32,
                        CString::new(format!("call{}", func_ref.0))
                            .unwrap()
                            .as_ptr(),
                    )
                }
                Node::Cast { value, ty } => {
                    let t1 = self.ast.get_ty(value);
                    let t2 = self.ast.get_ty(ty);
                    let value = self.eval_expression(builder, value);
                    let ty = self.type_to_llvm(self.ast.get_type_id(ty));

                    let name = CString::new(format!("cast{}", node_ref.0)).unwrap();

                    match (t1, t2) {
                        (Type::Float(_), Type::Int { size, signed }) => {
                            if signed {
                                LLVMBuildFPToSI(builder, value, ty, name.as_ptr())
                            } else {
                                LLVMBuildFPToUI(builder, value, ty, name.as_ptr())
                            }
                        }
                        (Type::Int { size, signed }, Type::Float(_)) => {
                            if signed {
                                LLVMBuildSIToFP(builder, value, ty, name.as_ptr())
                            } else {
                                LLVMBuildUIToFP(builder, value, ty, name.as_ptr())
                            }
                        }
                        (Type::Float(FloatKind::F32), Type::Float(FloatKind::F64)) => {
                            LLVMBuildFPExt(builder, value, ty, name.as_ptr())
                        }
                        (Type::Float(FloatKind::F64), Type::Float(FloatKind::F32)) => {
                            LLVMBuildFPTrunc(builder, value, ty, name.as_ptr())
                        }
                        (
                            Type::Int {
                                size: s1,
                                signed: true,
                            },
                            Type::Int {
                                size: s2,
                                signed: true,
                            },
                        ) => {
                            if s1 < s2 {
                                LLVMBuildTrunc(builder, value, ty, name.as_ptr())
                            } else {
                                LLVMBuildZExt(builder, value, ty, name.as_ptr())
                            }
                        }
                        _ => panic!(),
                    }
                }

                _ => panic!(),
            }
        }
    }

    fn eval_expression(&self, builder: *mut LLVMBuilder, node_ref: Ref) -> *mut LLVMValue {
        let node = self.ast.get_node(node_ref);
        unsafe {
            match node {
                Node::Binary { lhs, rhs, op } => {
                    let lhs = self.eval_expression(builder, lhs);
                    let rhs = self.eval_expression(builder, rhs);

                    match op.ty {
                        TokenType::Plus => LLVMBuildAdd(
                            builder,
                            lhs,
                            rhs,
                            CString::new(format!("addtmp{}", node_ref.0))
                                .unwrap()
                                .as_ptr(),
                        ),
                        TokenType::Minus => LLVMBuildSub(
                            builder,
                            lhs,
                            rhs,
                            CString::new(format!("subtmp{}", node_ref.0))
                                .unwrap()
                                .as_ptr(),
                        ),
                        TokenType::Asterix => LLVMBuildMul(
                            builder,
                            lhs,
                            rhs,
                            CString::new(format!("multmp{}", node_ref.0))
                                .unwrap()
                                .as_ptr(),
                        ),
                        TokenType::Slash => LLVMBuildFDiv(
                            builder,
                            lhs,
                            rhs,
                            CString::new(format!("divtmp{}", node_ref.0))
                                .unwrap()
                                .as_ptr(),
                        ),

                        _ => panic!(),
                    }
                }
                _ => self.eval_value(builder, node_ref),
            }
        }
    }

    fn eval_function_stmt(
        &mut self,
        builder: *mut LLVMBuilder,
        function: *mut LLVMValue,
        node: Ref,
    ) {
        unsafe {
            match self.ast.get_node(node) {
                Node::VarDecl { ident, ty, value } => {
                    let ty = self.type_to_llvm(self.ast.get_type_id(node));

                    let node_ident = self.get_ident(ident);
                    let ident = CString::new(node_ident.clone()).unwrap();

                    let entry = LLVMGetEntryBasicBlock(function);
                    let builder = LLVMCreateBuilderInContext(self.context);
                    LLVMPositionBuilderAtEnd(builder, entry);
                    let alloca = LLVMBuildAlloca(builder, ty, ident.as_ptr());

                    let value = self.eval_expression(builder, value);
                    LLVMBuildStore(builder, value, alloca);

                    self.scopes.push(node_ident, Def::Local(alloca));
                }
                Node::ConstDecl { ident, ty, value } => {
                    let ty = self.type_to_llvm(self.ast.get_type_id(node));

                    let node_ident = self.get_ident(ident);
                    let ident = CString::new(node_ident.clone()).unwrap();

                    let entry = LLVMGetEntryBasicBlock(function);
                    let builder = LLVMCreateBuilderInContext(self.context);
                    LLVMPositionBuilderAtEnd(builder, entry);
                    let alloca = LLVMBuildAlloca(builder, ty, ident.as_ptr());

                    let value = self.eval_expression(builder, value);
                    LLVMBuildStore(builder, value, alloca);

                    self.scopes.push(node_ident, Def::Local(alloca));
                }

                Node::Scope(lst) => {
                    for node in lst {
                        self.eval_function_stmt(builder, function, node);
                    }
                }
                Node::Assignment { ident, value } => {
                    let alloca = self.eval_value(builder, ident);
                    let value = self.eval_expression(builder, value);

                    LLVMBuildStore(builder, value, alloca);
                }

                Node::Return(value) => {
                    LLVMBuildRet(builder, self.eval_expression(builder, value));
                }
                _ => {
                    self.eval_expression(builder, node);
                }
            }
        }
    }

    fn type_to_llvm(&self, ty: TypeID) -> *mut LLVMType {
        unsafe {
            match self.ast.context.get(ty) {
                Type::Unit => todo!(),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Bool => LLVMInt8TypeInContext(self.context),
                Type::Int { size, signed } => match (signed, size) {
                    (true, IntSize::Bits8) => LLVMInt8TypeInContext(self.context),
                    (true, IntSize::Bits16) => LLVMInt16TypeInContext(self.context),
                    (true, IntSize::Bits32) => LLVMInt32TypeInContext(self.context),
                    (true, IntSize::Bits64) => LLVMInt64TypeInContext(self.context),
                    (true, IntSize::BitsPtr) => LLVMInt64TypeInContext(self.context),
                    (false, IntSize::Bits8) => LLVMInt8TypeInContext(self.context),
                    (false, IntSize::Bits16) => LLVMInt16TypeInContext(self.context),
                    (false, IntSize::Bits32) => LLVMInt32TypeInContext(self.context),
                    (false, IntSize::Bits64) => LLVMInt64TypeInContext(self.context),
                    (false, IntSize::BitsPtr) => LLVMInt64TypeInContext(self.context),
                },
                Type::Float(FloatKind::F32) => LLVMFloatTypeInContext(self.context),
                Type::Float(FloatKind::F64) => LLVMDoubleTypeInContext(self.context),
                Type::Slice(inner) => {
                    let ty = self.type_to_llvm(inner);
                    let mut elements = [ty, LLVMInt64TypeInContext(self.context)];

                    LLVMStructTypeInContext(
                        self.context,
                        elements.as_mut_ptr(),
                        elements.len() as u32,
                        false as i32,
                    )
                }
                Type::Array { inner, len } => LLVMArrayType2(self.type_to_llvm(inner), len as u64),
                Type::ADT(adt) => match adt.kind {
                    ADTKind::Struct => {
                        let mut elements = adt
                            .fields
                            .iter()
                            .map(|field| self.type_to_llvm(field.ty.unwrap()))
                            .collect_vec();

                        let elements = elements.as_mut_slice();

                        LLVMStructTypeInContext(
                            self.context,
                            elements.as_mut_ptr(),
                            elements.len() as u32,
                            false as i32,
                        )
                    }
                    _ => todo!(),
                },
                Type::Ptr(_) => LLVMPointerTypeInContext(self.context, 0),
                Type::Fn(FnType {
                    parameters,
                    return_ty,
                }) => {
                    let mut parameters = parameters
                        .iter()
                        .map(|ty| self.type_to_llvm(*ty))
                        .collect_vec()
                        .into_boxed_slice();

                    let return_ty = self.type_to_llvm(return_ty);

                    LLVMFunctionType(
                        return_ty,
                        parameters.as_mut_ptr(),
                        parameters.len() as u32,
                        0,
                    )
                }
                Type::Ref(type_id) => todo!(),
            }
        }
    }

    fn eval_instruction(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping,
        inst: &Instruction<'a>,
    ) {
        unsafe {
            let value = match inst {
                Instruction::Add { lhs, rhs, .. } => {
                    let lhs = mapping.get(*lhs);
                    let rhs = mapping.get(*rhs);

                    LLVMBuildAdd(builder, lhs, rhs, CString::new("addtmp").unwrap().as_ptr())
                }
                Instruction::Sub { lhs, rhs, .. } => {
                    let lhs = mapping.get(*lhs);
                    let rhs = mapping.get(*rhs);

                    LLVMBuildSub(builder, lhs, rhs, CString::new("subtmp").unwrap().as_ptr())
                }
                Instruction::Mul { lhs, rhs, .. } => {
                    let lhs = mapping.get(*lhs);
                    let rhs = mapping.get(*rhs);

                    LLVMBuildMul(builder, lhs, rhs, CString::new("multmp").unwrap().as_ptr())
                }
                Instruction::Div { lhs, rhs, .. } => {
                    let lhs = mapping.get(*lhs);
                    let rhs = mapping.get(*rhs);

                    LLVMBuildFDiv(builder, lhs, rhs, CString::new("divtmp").unwrap().as_ptr())
                }
                Instruction::CmpGt { lhs, rhs, .. } => todo!(),
                Instruction::CmpLt { lhs, rhs, .. } => todo!(),
                Instruction::CmpGe { lhs, rhs, .. } => todo!(),
                Instruction::CmpLe { lhs, rhs, .. } => todo!(),
                Instruction::CmpEq { lhs, rhs, .. } => todo!(),
                Instruction::CmpNq { lhs, rhs, .. } => todo!(),
                Instruction::Negate { value, .. } => todo!(),
                Instruction::Not { value, .. } => todo!(),
                Instruction::Load { reg, .. } => todo!(),
                Instruction::Store { reg, value, .. } => todo!(),
                Instruction::Ref { value, .. } => todo!(),
                Instruction::Deref { value, .. } => todo!(),
                Instruction::Cast {
                    value, from, to, ..
                } => {
                    let value = mapping.get(*value);
                    let ty = self.type_to_llvm(*to);
                    let name = CString::new("").unwrap();

                    match (self.tir.ctx.get(*from), self.tir.ctx.get(*to)) {
                        (Type::Float(_), Type::Int { size, signed }) => {
                            if signed {
                                LLVMBuildFPToSI(builder, value, ty, name.as_ptr())
                            } else {
                                LLVMBuildFPToUI(builder, value, ty, name.as_ptr())
                            }
                        }
                        (Type::Int { size, signed }, Type::Float(_)) => {
                            if signed {
                                LLVMBuildSIToFP(builder, value, ty, name.as_ptr())
                            } else {
                                LLVMBuildUIToFP(builder, value, ty, name.as_ptr())
                            }
                        }
                        (Type::Float(FloatKind::F32), Type::Float(FloatKind::F64)) => {
                            LLVMBuildFPExt(builder, value, ty, name.as_ptr())
                        }
                        (Type::Float(FloatKind::F64), Type::Float(FloatKind::F32)) => {
                            LLVMBuildFPTrunc(builder, value, ty, name.as_ptr())
                        }
                        (
                            Type::Int {
                                size: s1,
                                signed: true,
                            },
                            Type::Int {
                                size: s2,
                                signed: true,
                            },
                        ) => {
                            if s1 < s2 {
                                LLVMBuildTrunc(builder, value, ty, name.as_ptr())
                            } else {
                                LLVMBuildZExt(builder, value, ty, name.as_ptr())
                            }
                        }
                        _ => panic!(),
                    }
                }
                Instruction::CreateStruct { ty, fields } => {
                    let name = CString::new("").unwrap();
                    let alloca = LLVMBuildAlloca(builder, self.type_to_llvm(*ty), name.as_ptr());

                    for (idx, (field, ty)) in fields.iter().enumerate() {
                        let name = CString::new("").unwrap();
                        let alloca = LLVMBuildStructGEP2(
                            builder,
                            self.type_to_llvm(*ty),
                            alloca,
                            idx as u32,
                            name.as_ptr(),
                        );

                        LLVMBuildStore(builder, mapping.get(*field), alloca);
                    }

                    alloca
                }
                Instruction::StructStore { field, value, .. } => {
                    LLVMBuildStore(builder, mapping.get(*value), mapping.get(*field))
                }
                Instruction::StructLoad {
                    r#struct, idx, ty, ..
                } => {
                    let name = CString::new("").unwrap();
                    LLVMBuildStructGEP2(
                        builder,
                        self.type_to_llvm(*ty),
                        mapping.get(*r#struct),
                        *idx as u32,
                        name.as_ptr(),
                    )
                }
                Instruction::Call { func, args, .. } => {
                    let ty = self.type_to_llvm(self.tir.get_func(*func).ty.return_ty);

                    let func = self.functions_mapping[*func];
                    let mut args = args.iter().map(|arg| mapping.get(*arg)).collect_vec();

                    LLVMBuildCall2(
                        builder,
                        ty,
                        func,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        CString::new("").unwrap().as_ptr(),
                    )
                }
                Instruction::Integer(value, ty) => LLVMConstIntOfString(
                    self.type_to_llvm(*ty),
                    CString::new(*value).unwrap().as_ptr(),
                    10,
                ),
                Instruction::Float(value, ty) => LLVMConstRealOfString(
                    self.type_to_llvm(*ty),
                    CString::new(*value).unwrap().as_ptr(),
                ),
                Instruction::Bool(value) => LLVMConstInt(
                    LLVMInt8TypeInContext(self.context),
                    *value as u64,
                    false as i32,
                ),
                Instruction::String(value) => {
                    let ty =
                        LLVMArrayType2(LLVMInt8TypeInContext(self.context), value.len() as u64);

                    let name = CString::new(format!("global_string_{}", 0)).unwrap();
                    let global = LLVMAddGlobal(self.module, ty, name.as_ptr());

                    LLVMSetLinkage(global, LLVMLinkage::LLVMInternalLinkage);
                    LLVMSetGlobalConstant(global, true as i32);

                    let text = CString::new(*value).unwrap();
                    let value =
                        LLVMConstString(text.as_ptr(), text.count_bytes() as u32, false as i32);

                    LLVMSetInitializer(global, value);

                    global
                }
            };

            mapping.push(value);
        };
    }

    fn eval_cfg(&mut self, function: *mut LLVMValue, cfg: &CFG<'a>) {
        let mut mapping = CFGMapping::new();

        for (idx, bb) in cfg.blocks.iter().enumerate() {
            let llvmName = CString::new(format!("bb{}", idx)).unwrap();
            let llvmBB =
                unsafe { LLVMAppendBasicBlockInContext(self.context, function, llvmName.as_ptr()) };
            unsafe { LLVMPositionBuilderAtEnd(self.builder, llvmBB) };

            bb.instructions
                .iter()
                .for_each(|inst| self.eval_instruction(self.builder, &mut mapping, inst));
        }
    }

    fn eval_toplevel(&mut self, node: Ref) {
        match self.ast.get_node(node) {
            Node::VarDecl { ident, ty, value } => {}
            Node::ConstDecl { ident, ty, value } => {}
            Node::FnDecl {
                ident,
                params,
                ret,
                block,
            } => {
                let ident_og = self.get_ident(ident);
                let ident = CString::new(ident_og.clone()).unwrap();

                let function = unsafe {
                    let return_ty = self.type_to_llvm(self.ast.get_type_id(ret));
                    let function_type = LLVMFunctionType(return_ty, std::ptr::null_mut(), 0, 0);

                    LLVMAddFunction(self.module, ident.as_ptr(), function_type)
                };

                self.scopes.push(ident_og, Def::Local(function));

                unsafe {
                    let bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        b"entry\0".as_ptr() as *const _,
                    );
                    LLVMPositionBuilderAtEnd(self.builder, bb);

                    self.scope = Scope::new_with_parent(self.scopes.clone());

                    self.eval_function_stmt(self.builder, function, block);

                    if self.ast.get_ty(ret) == Type::Void {
                        LLVMBuildRetVoid(self.builder);
                    }
                }
            }
            Node::Type { ident, args, body } => {}
            _ => {}
        }
    }

    pub fn run(mut self) {
        for &TypeDef { ty, .. } in self.tir.type_iter() {
            self.type_to_llvm(ty);
        }

        for func in self.tir.func_iter() {
            let ident = CString::new(func.name).unwrap();
            let function = unsafe {
                let return_ty = self.type_to_llvm(func.ty.return_ty);
                let function_type = LLVMFunctionType(return_ty, std::ptr::null_mut(), 0, 0);
                LLVMAddFunction(self.module, ident.as_ptr(), function_type)
            };

            self.functions_mapping.push(function);

            unsafe { self.eval_cfg(function, &func.block) };
        }

        unsafe {
            let target_machine = {
                use target::{
                    LLVMInitializeX86AsmParser, LLVMInitializeX86AsmPrinter,
                    LLVMInitializeX86Disassembler, LLVMInitializeX86Target,
                    LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC,
                };

                LLVMInitializeX86Target();
                LLVMInitializeX86TargetInfo();
                LLVMInitializeX86AsmPrinter();
                LLVMInitializeX86AsmParser();
                LLVMInitializeX86Disassembler();
                LLVMInitializeX86TargetMC();

                let mut error: *mut i8 = std::ptr::null_mut();

                let target_triple = LLVMGetDefaultTargetTriple();

                let mut target_ref: LLVMTargetRef = std::ptr::null_mut();
                LLVMGetTargetFromTriple(target_triple, &mut target_ref, &mut error);

                LLVMCreateTargetMachine(
                    target_ref,
                    target_triple,
                    c"".as_ptr(),
                    c"".as_ptr(),
                    LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                    LLVMRelocMode::LLVMRelocDefault,
                    LLVMCodeModel::LLVMCodeModelDefault,
                )
            };

            let builder = LLVMCreatePassBuilderOptions();
            LLVMPassBuilderOptionsSetVerifyEach(builder, true as i32);
            LLVMPassBuilderOptionsSetDebugLogging(builder, true as i32);

            let passes = CString::new("mem2reg").unwrap();

            LLVMRunPasses(self.module, passes.as_ptr(), target_machine, builder);

            LLVMDumpModule(self.module);

            LLVMDisposePassBuilderOptions(builder);
        };
    }
}
