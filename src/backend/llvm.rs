use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::{CStr, CString},
    fs::File,
    mem::MaybeUninit,
};

use crate::{
    metadata::Metadata,
    rair::{
        self, BinOp, CFG, FuncIdx, Loc, Operand, Place, RIL, RValue, Statement, Terminator,
        TypeDef, UnOp,
    },
    types::{AdtKind, FloatKind, FnType, IntSize, Type, TypeID},
};

extern crate llvm_sys as llvm;

use itertools::{Itertools, izip};
use llvm::{core::*, transforms::pass_builder::*, *};
use llvm_sys::{
    target::LLVMSetModuleDataLayout,
    target_machine::{
        LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetDataLayout,
        LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple,
        LLVMRelocMode, LLVMTargetMachineEmitToFile, LLVMTargetRef,
    },
};
use typed_index_collections::{TiVec, ti_vec};
use log::{error, info};

trait IntoC<'a>: Clone
where
    Self::Item: Clone,
{
    type Item;

    fn into_c(self) -> Self::Item;
}

impl<'a> IntoC<'a> for &str {
    type Item = Cow<'a, CStr>;
    fn into_c(mut self) -> Self::Item {
        if self.is_empty() {
            self = "\0";
        }

        // Start from the end of the string as it's the most likely place to find a null byte
        if !self.chars().rev().any(|ch| ch == '\0') {
            return Cow::from(
                CString::new(self).expect("unreachable since null bytes are checked"),
            );
        }

        unsafe { Cow::from(CStr::from_ptr(self.as_ptr() as *const _)) }
    }
}

struct CFGMapping {
    data: HashMap<Place, *mut LLVMValue>,
    args: Vec<*mut LLVMValue>,
    basic_blocks: TiVec<Loc, *mut LLVMBasicBlock>,
}

impl CFGMapping {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
            args: vec![],
            basic_blocks: ti_vec![],
        }
    }

    fn push(&mut self, place: Place, value: *mut LLVMValue) {
        self.data.insert(place, value);
    }

    fn push_bb(&mut self, bb: *mut LLVMBasicBlock) {
        self.basic_blocks.push(bb);
    }

    fn get(&mut self, place: Place) -> *mut LLVMValue {
        *self.data.get(&place).unwrap()
    }

    fn get_bb(&mut self, loc: Loc) -> *mut LLVMBasicBlock {
        self.basic_blocks[loc]
    }
}

pub struct CodeBuilder<'a> {
    ril: RIL<'a>,

    functions_mapping: TiVec<FuncIdx, *mut LLVMValue>,

    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,

    metadata: Metadata<'a>,
}

impl<'a> Drop for CodeBuilder<'a> {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.builder);
            core::LLVMDisposeModule(self.module);
            core::LLVMContextDispose(self.context);
        }
    }
}

impl<'a> CodeBuilder<'a> {
    pub fn new(ril: RIL<'a>, metadata: Metadata<'a>) -> Self {
        let name = format!("{}.rama", metadata.name).into_c();

        let context = unsafe { core::LLVMContextCreate() };
        let module = unsafe { core::LLVMModuleCreateWithName(name.as_ptr() as *const _) };
        let builder = unsafe { core::LLVMCreateBuilderInContext(context) };

        Self {
            ril,
            functions_mapping: ti_vec![],
            context,
            module,
            builder,
            metadata,
        }
    }

    fn type_to_llvm(&self, ty: TypeID) -> *mut LLVMType {
        unsafe {
            match self.ril.ctx.get(ty) {
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Unit => LLVMArrayType2(LLVMInt8TypeInContext(self.context), 0),
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
                        elements.len() as _,
                        false as _,
                    )
                }
                Type::Array { inner, len } => LLVMArrayType2(self.type_to_llvm(inner), len as _),
                Type::Adt(adt) => match adt.kind {
                    AdtKind::Struct => {
                        let mut elements = adt
                            .fields
                            .iter()
                            .map(|field| self.type_to_llvm(field.ty.unwrap()))
                            .collect_vec();

                        let elements = elements.as_mut_slice();

                        LLVMStructTypeInContext(
                            self.context,
                            elements.as_mut_ptr(),
                            elements.len() as _,
                            false as _,
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
                        .map(|param| self.type_to_llvm(param.1))
                        .collect_vec()
                        .into_boxed_slice();

                    let return_ty = self.type_to_llvm(return_ty);

                    LLVMFunctionType(return_ty, parameters.as_mut_ptr(), parameters.len() as _, 0)
                }
                Type::Ref(_) => todo!(),
            }
        }
    }

    // fn eval_stmt(
    //     &mut self,
    //     builder: *mut LLVMBuilder,
    //     mapping: &mut CFGMapping,
    //     stmt: &Statement<'a>,
    // ) {
    //     unsafe {
    //         let value = match stmt {
    //             Instruction::Nop => panic!(),

    //             Instruction::Add { lhs, rhs, .. } => {
    //                 let lhs = mapping.get(*lhs);
    //                 let rhs = mapping.get(*rhs);

    //                 LLVMBuildAdd(builder, lhs, rhs, CString::new("addtmp").unwrap().as_ptr())
    //             }
    //             Instruction::Sub { lhs, rhs, .. } => {
    //                 let lhs = mapping.get(*lhs);
    //                 let rhs = mapping.get(*rhs);

    //                 LLVMBuildSub(builder, lhs, rhs, CString::new("subtmp").unwrap().as_ptr())
    //             }
    //             Instruction::Mul { lhs, rhs, .. } => {
    //                 let lhs = mapping.get(*lhs);
    //                 let rhs = mapping.get(*rhs);

    //                 LLVMBuildMul(builder, lhs, rhs, CString::new("multmp").unwrap().as_ptr())
    //             }
    //             Instruction::Div { lhs, rhs, .. } => {
    //                 let lhs = mapping.get(*lhs);
    //                 let rhs = mapping.get(*rhs);

    //                 LLVMBuildFDiv(builder, lhs, rhs, CString::new("divtmp").unwrap().as_ptr())
    //             }

    //             Instruction::Cmp {
    //                 lhs, rhs, ty, kind, ..
    //             } => {
    //                 let l = mapping.get(*lhs);
    //                 let r = mapping.get(*rhs);

    //                 match self.ril.ctx.get(*ty) {
    //                     Type::Int { signed: true, .. } => LLVMBuildICmp(
    //                         builder,
    //                         match kind {
    //                             CmpKind::Equal => LLVMIntPredicate::LLVMIntEQ,
    //                             CmpKind::NotEqual => LLVMIntPredicate::LLVMIntNE,
    //                             CmpKind::LessThan => LLVMIntPredicate::LLVMIntSLT,
    //                             CmpKind::LessEqual => LLVMIntPredicate::LLVMIntSLE,
    //                             CmpKind::GreaterThan => LLVMIntPredicate::LLVMIntSGT,
    //                             CmpKind::GreaterEqual => LLVMIntPredicate::LLVMIntSGE,
    //                         },
    //                         l,
    //                         r,
    //                         CString::new("intcmp").unwrap().as_ptr(),
    //                     ),
    //                     Type::Int { signed: false, .. } => LLVMBuildICmp(
    //                         builder,
    //                         match kind {
    //                             CmpKind::Equal => LLVMIntPredicate::LLVMIntEQ,
    //                             CmpKind::NotEqual => LLVMIntPredicate::LLVMIntNE,
    //                             CmpKind::LessThan => LLVMIntPredicate::LLVMIntULT,
    //                             CmpKind::LessEqual => LLVMIntPredicate::LLVMIntULE,
    //                             CmpKind::GreaterThan => LLVMIntPredicate::LLVMIntUGT,
    //                             CmpKind::GreaterEqual => LLVMIntPredicate::LLVMIntUGE,
    //                         },
    //                         l,
    //                         r,
    //                         CString::new("intcmp").unwrap().as_ptr(),
    //                     ),

    //                     _ => panic!(),
    //                 }
    //             }
    //             Instruction::Negate { dest: _, value } => LLVMBuildNeg(
    //                 builder,
    //                 mapping.get(*value),
    //                 CString::new("negate").unwrap().as_ptr(),
    //             ),
    //             Instruction::Not { .. } => todo!(),
    //             Instruction::MakeVar { dest: _, value, ty } => {
    //                 let name = CString::new("").unwrap();
    //                 let alloca = LLVMBuildAlloca(builder, self.type_to_llvm(*ty), name.as_ptr());
    //                 let value = mapping.get(*value);
    //                 LLVMBuildStore(builder, value, alloca);

    //                 alloca

    //                 //mapping.get(*value)
    //             }
    //             Instruction::ReadVar { dest: _, var } => mapping.get(*var),
    //             Instruction::WriteVar { var, value } => {
    //                 let var = mapping.get(*var);
    //                 let value = mapping.get(*value);
    //                 LLVMBuildStore(builder, value, var)
    //             }
    //             Instruction::Ref { .. } => todo!(),
    //             Instruction::Deref { .. } => todo!(),
    //             Instruction::Cast {
    //                 value, from, to, ..
    //             } => {
    //                 let value = mapping.get(*value);
    //                 let ty = self.type_to_llvm(*to);
    //                 let name = CString::new("").unwrap();

    //                 match (self.ril.ctx.get(*from), self.ril.ctx.get(*to)) {
    //                     (Type::Float(_), Type::Int { signed, .. }) => {
    //                         if signed {
    //                             LLVMBuildFPToSI(builder, value, ty, name.as_ptr())
    //                         } else {
    //                             LLVMBuildFPToUI(builder, value, ty, name.as_ptr())
    //                         }
    //                     }
    //                     (Type::Int { signed, .. }, Type::Float(_)) => {
    //                         if signed {
    //                             LLVMBuildSIToFP(builder, value, ty, name.as_ptr())
    //                         } else {
    //                             LLVMBuildUIToFP(builder, value, ty, name.as_ptr())
    //                         }
    //                     }
    //                     (Type::Float(FloatKind::F32), Type::Float(FloatKind::F64)) => {
    //                         LLVMBuildFPExt(builder, value, ty, name.as_ptr())
    //                     }
    //                     (Type::Float(FloatKind::F64), Type::Float(FloatKind::F32)) => {
    //                         LLVMBuildFPTrunc(builder, value, ty, name.as_ptr())
    //                     }
    //                     (
    //                         Type::Int {
    //                             size: s1,
    //                             signed: true,
    //                         },
    //                         Type::Int {
    //                             size: s2,
    //                             signed: true,
    //                         },
    //                     ) => {
    //                         if s1 < s2 {
    //                             LLVMBuildTrunc(builder, value, ty, name.as_ptr())
    //                         } else {
    //                             LLVMBuildZExt(builder, value, ty, name.as_ptr())
    //                         }
    //                     }
    //                     _ => panic!(),
    //                 }
    //             }
    //             Instruction::MakeStruct { dest: _, ty } => {
    //                 let name = CString::new("").unwrap();
    //                 LLVMBuildAlloca(builder, self.type_to_llvm(*ty), name.as_ptr())
    //             }
    //             Instruction::WriteField {
    //                 r#struct,
    //                 field,
    //                 value,
    //                 ty,
    //             } => {
    //                 let alloca = mapping.get(*r#struct);

    //                 let name = CString::new("").unwrap();
    //                 let alloca = LLVMBuildStructGEP2(
    //                     builder,
    //                     self.type_to_llvm(*ty),
    //                     alloca,
    //                     *field as _,
    //                     name.as_ptr(),
    //                 );

    //                 LLVMBuildStore(builder, mapping.get(*value), alloca);

    //                 alloca
    //             }
    //             Instruction::ReadField {
    //                 r#struct,
    //                 field,
    //                 ty,
    //                 ..
    //             } => {
    //                 let name = CString::new("").unwrap();
    //                 LLVMBuildStructGEP2(
    //                     builder,
    //                     self.type_to_llvm(*ty),
    //                     mapping.get(*r#struct),
    //                     *field as _,
    //                     name.as_ptr(),
    //                 )
    //             }

    //             Instruction::ReadArg { dest: _, index } => mapping.args[*index],

    //             Instruction::Call { func, args, .. } => {
    //                 let ty = self.type_to_llvm(self.ril.get_func(*func).ty);
    //                 let func = self.functions_mapping[*func];
    //                 let mut args = args.iter().map(|arg| mapping.get(*arg)).collect_vec();

    //                 LLVMBuildCall2(
    //                     builder,
    //                     ty,
    //                     func,
    //                     args.as_mut_ptr(),
    //                     args.len() as _,
    //                     CString::new("").unwrap().as_ptr(),
    //                 )
    //             }
    //             Instruction::Integer { value, ty, .. } => LLVMConstIntOfString(
    //                 self.type_to_llvm(*ty),
    //                 CString::new(*value).unwrap().as_ptr(),
    //                 10,
    //             ),
    //             Instruction::Float { value, ty, .. } => LLVMConstRealOfString(
    //                 self.type_to_llvm(*ty),
    //                 CString::new(*value).unwrap().as_ptr(),
    //             ),
    //             Instruction::Bool { value, .. } => LLVMConstInt(
    //                 LLVMInt8TypeInContext(self.context),
    //                 *value as _,
    //                 false as _,
    //             ),
    //             Instruction::String { value, .. } => {
    //                 let ty =
    //                     LLVMArrayType2(LLVMInt8TypeInContext(self.context), value.len() as u64);

    //                 let name = CString::new(format!("global_string_{}", 0)).unwrap();
    //                 let global = LLVMAddGlobal(self.module, ty, name.as_ptr());

    //                 LLVMSetLinkage(global, LLVMLinkage::LLVMInternalLinkage);
    //                 LLVMSetGlobalConstant(global, true as i32);

    //                 let text = CString::new(*value).unwrap();
    //                 let value =
    //                     LLVMConstString(text.as_ptr(), text.count_bytes() as u32, false as i32);

    //                 LLVMSetInitializer(global, value);

    //                 global
    //             }
    //             Instruction::Unit { dest: _ } => {
    //                 let ty = self.ril.ctx.alloc(Type::Unit);

    //                 let name = CString::new("").unwrap();
    //                 LLVMBuildAlloca(builder, self.type_to_llvm(ty), name.as_ptr())
    //             }

    //             Instruction::Goto(loc) => LLVMBuildBr(builder, mapping.get_bb(*loc)),
    //             Instruction::Goto_if { cond, loc } => todo!(),
    //             Instruction::Goto_if_not { cond, loc } => todo!(),

    //             Instruction::ReturnNone => LLVMBuildRetVoid(builder),
    //             Instruction::Return(value) => LLVMBuildRet(builder, mapping.get(*value)),
    //         };

    //         mapping.push(value);
    //     };
    // }

    fn eval_operand(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping,
        operand: &Operand<'a>,
    ) -> *mut LLVMValue {
        unsafe {
            match operand {
                Operand::Const(kind) => match kind {
                    rair::ConstKind::Float(value, ty) => {
                        LLVMConstRealOfString(self.type_to_llvm(*ty), value.into_c().as_ptr())
                    }
                    rair::ConstKind::Integer(value, ty) => {
                        LLVMConstIntOfString(self.type_to_llvm(*ty), value.into_c().as_ptr(), 10)
                    }
                    rair::ConstKind::String(value) => {
                        let ty =
                            LLVMArrayType2(LLVMInt8TypeInContext(self.context), value.len() as _);

                        let name = CString::new(format!("global_string_{}", 0)).unwrap();
                        let global = LLVMAddGlobal(self.module, ty, name.as_ptr());

                        LLVMSetLinkage(global, LLVMLinkage::LLVMInternalLinkage);
                        LLVMSetGlobalConstant(global, true as _);

                        let text = value.into_c();
                        let value =
                            LLVMConstString(text.as_ptr(), text.count_bytes() as _, false as _);

                        LLVMSetInitializer(global, value);

                        global
                    }
                    rair::ConstKind::True => {
                        LLVMConstInt(LLVMInt8TypeInContext(self.context), true as _, false as _)
                    }

                    rair::ConstKind::False => {
                        LLVMConstInt(LLVMInt8TypeInContext(self.context), false as _, false as _)
                    }
                    rair::ConstKind::Unit => todo!(),
                },
                Operand::Copy(place) => mapping.get(*place),
            }
        }
    }

    fn eval_rvalue(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping,
        rvalue: &RValue<'a>,
    ) -> *mut LLVMValue {
        unsafe {
            match rvalue {
                RValue::Use(operand) => self.eval_operand(builder, mapping, operand),
                RValue::BinaryOp(op, lhs, rhs) => {
                    let lhs = self.eval_operand(builder, mapping, lhs);
                    let rhs = self.eval_operand(builder, mapping, rhs);

                    match op {
                        BinOp::Add => LLVMBuildAdd(builder, lhs, rhs, "addtmp".into_c().as_ptr()),
                        BinOp::Sub => LLVMBuildSub(builder, lhs, rhs, "subtmp".into_c().as_ptr()),
                        BinOp::Mul => LLVMBuildMul(builder, lhs, rhs, "multmp".into_c().as_ptr()),
                        BinOp::Div => LLVMBuildFDiv(builder, lhs, rhs, "divtmp".into_c().as_ptr()),
                        _ => panic!(),
                    }
                }
                RValue::UnaryOp(op, value) => match op {
                    UnOp::Not => LLVMBuildNot(
                        builder,
                        self.eval_operand(builder, mapping, value),
                        "nottmp".into_c().as_ptr(),
                    ),
                    UnOp::Negate => LLVMBuildNeg(
                        builder,
                        self.eval_operand(builder, mapping, value),
                        "negatetmp".into_c().as_ptr(),
                    ),
                },
                RValue::Cast(operand, type_id) => todo!(),
                RValue::Ref(place) => todo!(),
                RValue::Call(func_idx, operands) => todo!(),
                RValue::Aggregate(aggregate_kind) => todo!(),
                RValue::Len(place) => todo!(),
            }
        }
    }

    fn eval_terminator(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping,
        terminator: &Terminator<'a>,
    ) -> *mut LLVMValue {
        unsafe {
            match terminator {
                Terminator::Goto(loc) => LLVMBuildBr(builder, mapping.get_bb(*loc)),
                Terminator::If { cond, t, f } => LLVMBuildCondBr(
                    builder,
                    mapping.get(*cond),
                    mapping.get_bb(*t),
                    mapping.get_bb(*f),
                ),
                Terminator::ReturnNone => LLVMBuildRetVoid(builder),
                Terminator::Return(value) => {
                    LLVMBuildRet(builder, self.eval_operand(builder, mapping, value))
                }
            }
        }
    }

    fn eval_cfg(&mut self, mapping: &mut CFGMapping, function: *mut LLVMValue, cfg: &CFG<'a>) {
        let entry = unsafe {
            LLVMAppendBasicBlockInContext(self.context, function, "entry".into_c().as_ptr())
        };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, entry) };

        for (place, ty) in cfg.mapping.iter_enumerated() {
            let name = "".into_c();
            mapping.push(place, unsafe {
                LLVMBuildAlloca(self.builder, self.type_to_llvm(*ty), name.as_ptr())
            });
        }

        for idx in 0..cfg.blocks.len() {
            let llvm_name = format!("bb{}", idx).into_c();
            let llvm_bb = unsafe {
                LLVMAppendBasicBlockInContext(self.context, function, llvm_name.as_ptr())
            };

            mapping.push_bb(llvm_bb);
        }

        for (bb, llvm_bb) in izip!(cfg.blocks.iter(), mapping.basic_blocks.clone().iter()) {
            unsafe { LLVMPositionBuilderAtEnd(self.builder, *llvm_bb) };

            for stmt in &bb.statements {
                match stmt {
                    Statement::Assign(place, rvalue) => {
                        let alloca = mapping.get(*place);
                        let value = self.eval_rvalue(self.builder, mapping, rvalue);
                        unsafe { LLVMBuildStore(self.builder, value, alloca) };
                    }
                }
            }

            self.eval_terminator(self.builder, mapping, &bb.terminator);
        }

        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, entry);
            LLVMBuildBr(self.builder, *mapping.basic_blocks.first().unwrap());
        };
    }

    pub fn build(mut self, file_type: FileType) {
        for TypeDef { ty, .. } in &self.ril.types {
            self.type_to_llvm(*ty);
        }

        for func in self.ril.clone().funcs {
            let mut mapping = CFGMapping::new();

            match func {
                rair::Func::Decl {
                    name,
                    cfg,
                    return_ty,
                    params,
                } => {
                    let ident = name.into_c();
                    let function = unsafe {
                        let mut parameters = params
                            .iter()
                            .map(|param| self.type_to_llvm(param.ty))
                            .collect_vec();

                        let return_ty = self.type_to_llvm(return_ty);
                        let function_type = LLVMFunctionType(
                            return_ty,
                            parameters.as_mut_ptr(),
                            parameters.len() as _,
                            false as _,
                        );

                        LLVMAddFunction(self.module, ident.as_ptr(), function_type)
                    };

                    for idx in 0..params.len() {
                        mapping
                            .args
                            .push(unsafe { LLVMGetParam(function, idx as _) });
                    }

                    self.functions_mapping.push(function);
                    self.eval_cfg(&mut mapping, function, &cfg);
                }

                rair::Func::Extern {
                    name,
                    return_ty,
                    params,
                } => todo!(),
            }
        }

        let target_machine = unsafe {
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
            LLVMSetTarget(self.module, target_triple);

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

        unsafe {
            let data_layout = LLVMCreateTargetDataLayout(target_machine);
            LLVMSetModuleDataLayout(self.module, data_layout);
        }

        let builder = unsafe {
            let builder = LLVMCreatePassBuilderOptions();
            LLVMPassBuilderOptionsSetVerifyEach(builder, true as _);
            LLVMPassBuilderOptionsSetDebugLogging(builder, true as _);
            builder
        };

        let passes = [
            "instcombine",
            "reassociate",
            "gvn",
            "simplifycfg",
            "mem2reg",
        ]
        .join(", ");

        let passes = passes.into_c();

        unsafe { LLVMRunPasses(self.module, passes.as_ptr(), target_machine, builder) };

        unsafe { LLVMDumpModule(self.module) };

        let filename =
            self.metadata
                .outdir
                .join(format!("{}{}", self.metadata.name, file_type.extension()));

        drop(
            File::create(&filename)
                .unwrap_or_else(|_| panic!("Failed to create file {}", filename.to_str().unwrap())),
        );

        let filename = filename.to_str().unwrap();
        let filename_c = filename.into_c();

        let mut error_message = MaybeUninit::uninit();

        unsafe {
            LLVMTargetMachineEmitToFile(
                target_machine,
                dbg!(self.module),
                filename_c.as_ptr() as *mut _,
                file_type.llvm_file_type(),
                error_message.as_mut_ptr(),
            )
        };

        unsafe {
            LLVMDisposePassBuilderOptions(builder);
            LLVMDisposeModule(self.module);
        }

        match std::process::Command::new("ld")
            .arg(format!("-o build/{}", self.metadata.name))
            .arg(filename)
            .spawn()
        {
            Ok(_) => info!("linked successfully"),
            Err(_) => error!("Failed to link executable"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum FileType {
    Assembly,
    Object,
}

impl FileType {
    fn llvm_file_type(self) -> LLVMCodeGenFileType {
        match self {
            FileType::Assembly => LLVMCodeGenFileType::LLVMAssemblyFile,
            FileType::Object => LLVMCodeGenFileType::LLVMObjectFile,
        }
    }

    fn extension(self) -> String {
        match self {
            FileType::Assembly => ".s",
            FileType::Object => ".o",
        }
        .to_string()
    }
}

pub fn compile<'a>(rair: RIL<'a>, metadata: Metadata<'a>) {
    let code = CodeBuilder::new(rair, metadata);
    let filetype = FileType::Object;

    code.build(filetype);
}
