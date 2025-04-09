use std::ffi::CString;

use crate::{
    metadata::Metadata,
    tir::{CFG, FuncRef, Instruction, Loc, Ref, TIR, Terminator, TypeDef},
    types::{ADTKind, FloatKind, FnType, IntSize, Type, TypeID},
};

extern crate llvm_sys as llvm;

use itertools::Itertools;
use llvm::{core::*, transforms::pass_builder::*, *};
use llvm_sys::target_machine::{
    LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple,
    LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetRef,
};
use typed_index_collections::{TiVec, ti_vec};

struct CFGMapping {
    data: TiVec<Ref, *mut LLVMValue>,
    basic_blocks: TiVec<Loc, *mut LLVMBasicBlock>,
}

impl CFGMapping {
    fn new() -> Self {
        Self {
            data: ti_vec![],
            basic_blocks: ti_vec![],
        }
    }

    fn push(&mut self, value: *mut LLVMValue) {
        self.data.push(value);
    }

    fn push_bb(&mut self, bb: *mut LLVMBasicBlock) {
        self.basic_blocks.push(bb);
    }

    fn get(&mut self, value: Ref) -> *mut LLVMValue {
        self.data[value]
    }

    fn get_bb(&mut self, loc: Loc) -> *mut LLVMBasicBlock {
        self.basic_blocks[loc]
    }
}

pub struct Codegen<'a> {
    tir: TIR<'a>,

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
            functions_mapping: ti_vec![],
            context,
            module,
            builder,
        }
    }

    fn type_to_llvm(&self, ty: TypeID) -> *mut LLVMType {
        unsafe {
            match self.tir.ctx.get(ty) {
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
                Type::Ref(_) => todo!(),
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
                Instruction::CmpGt { .. } => todo!(),
                Instruction::CmpLt { .. } => todo!(),
                Instruction::CmpGe { .. } => todo!(),
                Instruction::CmpLe { .. } => todo!(),
                Instruction::CmpEq { .. } => todo!(),
                Instruction::CmpNq { .. } => todo!(),
                Instruction::Negate { .. } => todo!(),
                Instruction::Not { .. } => todo!(),
                Instruction::Load { .. } => todo!(),
                Instruction::Store { reg, value } => {
                    mapping.get(*value)
                }
                Instruction::Ref { .. } => todo!(),
                Instruction::Deref { .. } => todo!(),
                Instruction::Cast {
                    value, from, to, ..
                } => {
                    let value = mapping.get(*value);
                    let ty = self.type_to_llvm(*to);
                    let name = CString::new("").unwrap();

                    match (self.tir.ctx.get(*from), self.tir.ctx.get(*to)) {
                        (Type::Float(_), Type::Int { signed, .. }) => {
                            if signed {
                                LLVMBuildFPToSI(builder, value, ty, name.as_ptr())
                            } else {
                                LLVMBuildFPToUI(builder, value, ty, name.as_ptr())
                            }
                        }
                        (Type::Int { signed, .. }, Type::Float(_)) => {
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
                Instruction::CreateStruct { ty, fields, .. } => {
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

                Instruction::FuncRef { index, .. } => self.functions_mapping[*index],
                Instruction::TypeRef { .. } => todo!(),

                Instruction::Call { func, args, ty, .. } => {
                    let ty = self.type_to_llvm(*ty);

                    let func = mapping.get(*func);
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
                Instruction::Integer { value, ty, .. } => LLVMConstIntOfString(
                    self.type_to_llvm(*ty),
                    CString::new(*value).unwrap().as_ptr(),
                    10,
                ),
                Instruction::Float { value, ty, .. } => LLVMConstRealOfString(
                    self.type_to_llvm(*ty),
                    CString::new(*value).unwrap().as_ptr(),
                ),
                Instruction::Bool { value, .. } => LLVMConstInt(
                    LLVMInt8TypeInContext(self.context),
                    *value as u64,
                    false as i32,
                ),
                Instruction::String { value, .. } => {
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

    fn eval_terminator(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping,
        terminator: &Terminator,
    ) {
        unsafe {
            match terminator {
                Terminator::Goto(loc) => {
                    LLVMBuildBr(builder, mapping.get_bb(*loc));
                }
                Terminator::If { .. } => todo!(),
                Terminator::Return(value) => {
                    LLVMBuildRet(builder, mapping.get(*value));
                }
                Terminator::ImplicitReturn(_) => todo!(),
            }
        }
    }

    fn eval_cfg(&mut self, function: *mut LLVMValue, cfg: &CFG<'a>) {
        let mut mapping = CFGMapping::new();

        for (idx, bb) in cfg.blocks.iter().enumerate() {
            let llvm_name = CString::new(format!("bb{}", idx)).unwrap();
            let llvm_bb =
                unsafe { LLVMAppendBasicBlockInContext(self.context, function, llvm_name.as_ptr()) };

            mapping.push_bb(llvm_bb);
            unsafe { LLVMPositionBuilderAtEnd(self.builder, llvm_bb) };

            bb.instructions
                .iter()
                .for_each(|inst| self.eval_instruction(self.builder, &mut mapping, inst));

            self.eval_terminator(self.builder, &mut mapping, &bb.terminator);
        }
    }

    pub fn run(mut self) {
        for TypeDef { ty, .. } in self.tir.type_iter() {
            self.type_to_llvm(ty);
        }

        for func in self.tir.clone().func_iter() {
            let ty = match self.tir.ctx.get(func.ty) {
                Type::Fn(ty) => ty,
                _ => panic!(),
            };

            let ident = CString::new(func.name).unwrap();
            let function = unsafe {
                let return_ty = self.type_to_llvm(ty.return_ty);
                let function_type = LLVMFunctionType(return_ty, std::ptr::null_mut(), 0, 0);
                LLVMAddFunction(self.module, ident.as_ptr(), function_type)
            };

            self.functions_mapping.push(function);
            self.eval_cfg(function, &func.cfg);
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
            LLVMPassBuilderOptionsSetDebugLogging(builder, true as i32);

            let passes = CString::new("mem2reg").unwrap();

            LLVMRunPasses(self.module, passes.as_ptr(), target_machine, builder);

            LLVMDumpModule(self.module);

            LLVMDisposePassBuilderOptions(builder);
        };
    }
}
