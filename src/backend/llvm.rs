use std::{ffi::CString, ptr};

use crate::{
    ast::{Literal, Node, Ref},
    lexer::{Token, TokenType},
    typed_ast::TypedAST,
    types::{FloatKind, IntSize, Type, TypeID},
};

extern crate llvm_sys as llvm;

use llvm::{core::*, transforms::pass_builder::*, *};
use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction};

pub struct Codegen<'a> {
    ast: TypedAST<'a>,

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
    pub fn new(ast: TypedAST<'a>) -> Self {
        let context = unsafe { core::LLVMContextCreate() };
        let module = unsafe { core::LLVMModuleCreateWithName(b"test\0".as_ptr() as *const _) };
        let builder = unsafe { core::LLVMCreateBuilderInContext(context) };

        Self {
            ast,
            context,
            module,
            builder,
        }
    }

    fn get_ident(&self, node: Ref) -> Token {
        match self.ast.get_node(node) {
            Node::Ident(token) => token,
            _ => panic!(),
        }
    }

    fn eval_value(&self, build: *mut LLVMBuilder, node_ref: Ref) -> *mut LLVMValue {
        let (node, ty) = self.ast.get(node_ref);
        let ty = self.type_to_llvm(ty);
        unsafe {
            match node {
                Node::Literal(Literal::Int(v)) => {
                    LLVMConstIntOfString(ty, CString::new(v).unwrap().as_ptr(), 10)
                }
                Node::Literal(Literal::Float(v)) => {
                    LLVMConstRealOfString(LLVMFloatType(), CString::new(v).unwrap().as_ptr())
                }
                Node::Literal(Literal::Bool(v)) => {
                    LLVMConstInt(LLVMFloatType(), v as u64, false as i32)
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
                Node::Literal(_) => self.eval_value(builder, node_ref),
                Node::Ident(token) => {
                    let ty = self.type_to_llvm(self.ast.get_type_id(node_ref));

                    let ident = CString::new(token.text).unwrap();

                    let alloca = LLVMBuildAlloca(builder, ty, ident.as_ptr());
                    LLVMBuildLoad2(builder, ty, alloca, ident.as_ptr())
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

                _ => panic!("{}", Into::<&'static str>::into(node)),
            }
        }
    }

    fn eval_function_stmt(&self, builder: *mut LLVMBuilder, function: *mut LLVMValue, node: Ref) {
        unsafe {
            match self.ast.get_node(node) {
                Node::VarDecl { ident, ty, value } => {
                    let ty = self.type_to_llvm(self.ast.get_type_id(node));

                    let ident = self.get_ident(ident);
                    let ident = CString::new(ident.text).unwrap();

                    let entry = LLVMGetEntryBasicBlock(function);
                    let builder = LLVMCreateBuilderInContext(self.context);
                    LLVMPositionBuilder(builder, entry, ptr::null_mut());
                    let alloca = LLVMBuildAlloca(builder, ty, ident.as_ptr());

                    let value = self.eval_expression(builder, value);

                    let load = LLVMBuildLoad2(builder, ty, alloca, ident.as_ptr());
                    LLVMBuildStore(builder, load, value);
                }
                Node::ConstDecl { ident, ty, value } => {
                    let ty = self.type_to_llvm(self.ast.get_type_id(node));

                    let ident = self.get_ident(ident);
                    let ident = CString::new(ident.text).unwrap();

                    let entry = LLVMGetEntryBasicBlock(function);
                    let builder = LLVMCreateBuilderInContext(self.context);
                    LLVMPositionBuilder(builder, entry, ptr::null_mut());
                    let alloca = LLVMBuildAlloca(builder, ty, ident.as_ptr());

                    let value = self.eval_expression(builder, value);

                    let load = LLVMBuildLoad2(builder, ty, alloca, ident.as_ptr());
                    LLVMBuildStore(builder, load, value);
                }

                Node::Scope(lst) => {
                    for node in lst {
                        self.eval_function_stmt(builder, function, node);
                    }
                }
                Node::Assignment { ident, value } => {}

                Node::Return(value) => {
                    LLVMBuildRet(builder, self.eval_expression(builder, value));
                }
                _ => {}
            }
        }
    }

    fn type_to_llvm(&self, ty: TypeID) -> *mut LLVMType {
        unsafe {
            match self.ast.context.get(ty) {
                Type::Unit => todo!(),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Bool => LLVMInt8Type(),
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
                Type::Slice(type_id) => todo!(),
                Type::Array { inner, len } => LLVMArrayType2(self.type_to_llvm(inner), len as u64),
                Type::ADT(adt) => todo!(),
                Type::Ptr(inner) => LLVMPointerType(self.type_to_llvm(inner), 0),
                Type::Fn {
                    parameters,
                    return_ty,
                } => {
                    let mut parameters = parameters
                        .iter()
                        .map(|ty| self.type_to_llvm(*ty))
                        .collect::<Vec<*mut LLVMType>>()
                        .into_boxed_slice();

                    let return_ty = self.type_to_llvm(return_ty);

                    core::LLVMFunctionType(
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
                let ident = self.get_ident(ident);
                let ident = CString::new(ident.text).unwrap();

                let function = unsafe {
                    let return_ty = self.type_to_llvm(self.ast.get_type_id(ret));
                    let function_type =
                        core::LLVMFunctionType(return_ty, std::ptr::null_mut(), 0, 0);

                    core::LLVMAddFunction(self.module, ident.as_ptr(), function_type)
                };

                unsafe {
                    let bb = llvm::core::LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        b"entry\0".as_ptr() as *const _,
                    );
                    LLVMPositionBuilderAtEnd(self.builder, bb);

                    self.eval_function_stmt(self.builder, function, block);

                    LLVMVerifyFunction(function, LLVMVerifierFailureAction::LLVMPrintMessageAction);
                }
            }
            Node::Type { ident, args, body } => {}
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
            //let pass_manager = LLVMCreateFunctionPassManagerForModule(self.module);

            //let builder = LLVMCreatePassBuilderOptions();

            //LLVMAddPromoteMemoryToRegisterPass(pass_manager);

            LLVMDumpModule(self.module)
        };
    }
}
