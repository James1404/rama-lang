use std::{collections::HashMap, ffi::CString};

use crate::{
    ast::{Literal, Node, Ref},
    lexer::TokenType,
    metadata::Metadata,
    typed_ast::TypedAST,
    types::{ADTKind, FloatKind, IntSize, Type, TypeID}, valuescope::ScopeArena,
};

extern crate llvm_sys as llvm;

use itertools::Itertools;
use llvm::{core::*, transforms::pass_builder::*, *};
use llvm_sys::target_machine::{
    LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple,
    LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetRef,
};

#[derive(Debug, Clone)]
pub enum Def {
    Local(*mut LLVMValue),
    Type(*mut LLVMType),
}

type Scope<'a> = ScopeArena<'a, Def>;

pub struct Codegen<'a> {
    ast: TypedAST<'a>,

    scopes: Scope<'a>,

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
    pub fn new(ast: TypedAST<'a>, metadata: Metadata<'a>) -> Self {
        let name = CString::new(metadata.filename.to_str().unwrap()).unwrap();

        let context = unsafe { core::LLVMContextCreate() };
        let module = unsafe { core::LLVMModuleCreateWithName(name.as_ptr() as *const _) };
        let builder = unsafe { core::LLVMCreateBuilderInContext(context) };

        Self {
            ast,
            scopes: Scope::new(),
            context,
            module,
            builder,
        }
    }

    fn get_ident(&'a self, node: Ref) -> &'a str {
        match self.ast.get_node(node) {
            Node::Ident(token) => token.text,
            _ => panic!(),
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
                Type::Fn {
                    parameters,
                    return_ty,
                } => {
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
            Node::Type { ident, args, body } => {
                let ty = self.type_to_llvm(self.ast.get_type_id(body));
                let ident = self.get_ident(ident);
                self.scopes.push(ident, Def::Type(ty));
            }
            _ => {}
        }
    }

    pub fn run(mut self) {
        match self.ast.root {
            Some(node) => match self.ast.get_node(node) {
                Node::TopLevelScope(lst) => {
                    for node in lst {
                        self.eval_toplevel(node);
                    }
                }
                _ => {}
            },
            None => {}
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
