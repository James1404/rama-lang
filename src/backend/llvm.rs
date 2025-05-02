use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::{CStr, CString},
    fs::File,
    mem::MaybeUninit,
};

use crate::{
    metadata::Metadata,
    ril::{
        BinOp, ConstKind, Func, FuncIdx, Loc, Operand, Place, RValue, Statement, Terminator, TypeDef, UnOp, CFG, RIL
    },
    ty::{AdtKind, FloatKind, FnType, IntSize, Type, TypeID},
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
use log::{error, info};
use typed_index_collections::{TiVec, ti_vec};

trait IntoC<'a> {
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

trait IntoLLVM<'a> {
    type Item;
    type Extra;

    fn into_llvm(self, extra: Self::Extra) -> Self::Item;
}

impl<'a> IntoLLVM<'a> for TypeID {
    type Item = *mut LLVMType;
    type Extra = &'a CodeBuilder<'a>;

    fn into_llvm(self, extra: Self::Extra) -> Self::Item {
        unsafe {
            match extra.ril.ctx.get(self) {
                Type::Void => LLVMVoidTypeInContext(extra.context),
                Type::Unit => LLVMArrayType2(LLVMInt8TypeInContext(extra.context), 0),
                Type::Bool => LLVMInt8TypeInContext(extra.context),
                Type::Int { size, signed } => match (signed, size) {
                    (true, IntSize::Bits8) => LLVMInt8TypeInContext(extra.context),
                    (true, IntSize::Bits16) => LLVMInt16TypeInContext(extra.context),
                    (true, IntSize::Bits32) => LLVMInt32TypeInContext(extra.context),
                    (true, IntSize::Bits64) => LLVMInt64TypeInContext(extra.context),
                    (true, IntSize::BitsPtr) => LLVMInt64TypeInContext(extra.context),
                    (false, IntSize::Bits8) => LLVMInt8TypeInContext(extra.context),
                    (false, IntSize::Bits16) => LLVMInt16TypeInContext(extra.context),
                    (false, IntSize::Bits32) => LLVMInt32TypeInContext(extra.context),
                    (false, IntSize::Bits64) => LLVMInt64TypeInContext(extra.context),
                    (false, IntSize::BitsPtr) => LLVMInt64TypeInContext(extra.context),
                },
                Type::Float(FloatKind::F32) => LLVMFloatTypeInContext(extra.context),
                Type::Float(FloatKind::F64) => LLVMDoubleTypeInContext(extra.context),
                Type::Slice(inner) => {
                    let ty = inner.into_llvm(extra);
                    let mut elements = [ty, LLVMInt64TypeInContext(extra.context)];

                    LLVMStructTypeInContext(
                        extra.context,
                        elements.as_mut_ptr(),
                        elements.len() as _,
                        false as _,
                    )
                }
                Type::Array { inner, len } => LLVMArrayType2(inner.into_llvm(extra), len as _),
                Type::Adt(adt) => match adt.kind {
                    AdtKind::Struct => {
                        let mut elements = adt
                            .fields
                            .iter()
                            .map(|field| field.ty.unwrap().into_llvm(extra))
                            .collect_vec();

                        let elements = elements.as_mut_slice();

                        LLVMStructTypeInContext(
                            extra.context,
                            elements.as_mut_ptr(),
                            elements.len() as _,
                            false as _,
                        )
                    }
                    _ => todo!(),
                },
                Type::Ptr(_) => LLVMPointerTypeInContext(extra.context, 0),
                Type::Fn(FnType {
                    parameters,
                    return_ty,
                }) => {
                    let mut parameters = parameters
                        .iter()
                        .map(|param| param.1.into_llvm(extra))
                        .collect_vec()
                        .into_boxed_slice();

                    let return_ty = return_ty.into_llvm(extra);

                    LLVMFunctionType(return_ty, parameters.as_mut_ptr(), parameters.len() as _, 0)
                }
                Type::Ref(_) => todo!(),
            }
        }
    }
}

struct CFGMapping<'a> {
    data: HashMap<Place<'a>, *mut LLVMValue>,
    args: Vec<*mut LLVMValue>,
    basic_blocks: TiVec<Loc, *mut LLVMBasicBlock>,
    types: HashMap<Place<'a>, TypeID>,
}

impl<'a> CFGMapping<'a> {
    fn new(types: HashMap<Place<'a>, TypeID>) -> Self {
        Self {
            data: HashMap::new(),
            args: vec![],
            basic_blocks: ti_vec![],
            types,
        }
    }

    fn push(&mut self, place: Place<'a>, value: *mut LLVMValue) {
        self.data.insert(place, value);
    }

    fn push_bb(&mut self, bb: *mut LLVMBasicBlock) {
        self.basic_blocks.push(bb);
    }

    fn get(&mut self, place: Place<'a>) -> *mut LLVMValue {
        *self.data.get(&place).unwrap()
    }

    fn get_bb(&mut self, loc: Loc) -> *mut LLVMBasicBlock {
        self.basic_blocks[loc]
    }
}

pub struct CodeBuilder<'ctx> {
    ril: RIL<'ctx, 'ctx>,

    functions_mapping: TiVec<FuncIdx, *mut LLVMValue>,

    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,

    metadata: Metadata<'ctx>,
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
    pub fn new(ril: RIL<'a, 'a>, metadata: Metadata<'a>) -> Self {
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

    fn eval_operand(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping<'a>,
        operand: &Operand<'a>,
    ) -> *mut LLVMValue {
        unsafe {
            match operand {
                Operand::Const(kind) => match kind {
                    ConstKind::Float(value, ty) => {
                        LLVMConstRealOfString(ty.into_llvm(self), value.into_c().as_ptr())
                    }
                    ConstKind::Integer(value, ty) => {
                        LLVMConstIntOfString(ty.into_llvm(self), value.into_c().as_ptr(), 10)
                    }
                    ConstKind::String(value) => {
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
                    ConstKind::True => {
                        LLVMConstInt(LLVMInt8TypeInContext(self.context), true as _, false as _)
                    }

                    ConstKind::False => {
                        LLVMConstInt(LLVMInt8TypeInContext(self.context), false as _, false as _)
                    }
                    ConstKind::Unit => todo!(),
                },
                Operand::Copy(place) => mapping.get(*place),
            }
        }
    }

    fn eval_rvalue(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping<'a>,
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
                RValue::BuildAdt(ty, type_args, fields) => todo!(),
                RValue::BuildArray(inner, data) => {
                    let inner = inner.into_llvm(self);
                    let arr_ty = LLVMArrayType2(inner, data.len() as _);

                    let mut elements = [LLVMInt64TypeInContext(self.context), arr_ty];
                    let ty = LLVMStructTypeInContext(
                        self.context,
                        elements.as_mut_ptr(),
                        elements.len() as _,
                        false as _,
                    );

                    let name = "array".into_c();
                    let alloca = LLVMBuildAlloca(builder, ty, name.as_ptr());

                    let name = "len".into_c();
                    let len = LLVMBuildStructGEP2(builder, arr_ty, alloca, 0 as _, name.as_ptr());
                    let val = LLVMConstInt(inner, data.len() as _, 10);
                    LLVMBuildStore(builder, val, len);

                    let name = "array_inner".into_c();
                    let array = LLVMBuildStructGEP2(builder, arr_ty, alloca, 1 as _, name.as_ptr());

                    for (idx, elem) in data.iter().enumerate() {
                        let name = format!("array_{idx}").into_c();
                        let elem_ptr =
                            LLVMBuildStructGEP2(builder, arr_ty, array, idx as _, name.as_ptr());
                        let elem_val = self.eval_operand(builder, mapping, elem);
                        LLVMBuildStore(builder, elem_val, elem_ptr);
                    }

                    alloca
                }
                RValue::BuildSlice(ty, array) => todo!(),
                RValue::Len(place) => {
                    let ty = mapping.types[place];
                    match self.ril.ctx.get(ty) {
                        Type::Slice(_) => todo!(),
                        Type::Array { inner, .. } => {
                            let alloca = mapping.get(*place);

                            let ptr = LLVMBuildStructGEP2(
                                builder,
                                ty.into_llvm(self),
                                alloca,
                                0 as _,
                                "len_ptr".into_c().as_ptr(),
                            );

                            LLVMBuildLoad2(
                                builder,
                                inner.into_llvm(self),
                                ptr,
                                "len".into_c().as_ptr(),
                            )
                        }
                        _ => panic!(),
                    }
                }
            }
        }
    }

    fn eval_terminator(
        &mut self,
        builder: *mut LLVMBuilder,
        mapping: &mut CFGMapping<'a>,
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

    fn eval_cfg(&mut self, mapping: &mut CFGMapping<'a>, function: *mut LLVMValue, cfg: &CFG<'a>) {
        let entry = unsafe {
            LLVMAppendBasicBlockInContext(self.context, function, "entry".into_c().as_ptr())
        };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, entry) };

        for (place, ty) in cfg.mapping.iter() {
            let name = "".into_c();
            mapping.push(place.clone(), unsafe {
                LLVMBuildAlloca(self.builder, ty.into_llvm(self), name.as_ptr())
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
                        let alloca = mapping.get(place.clone());
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

    pub fn emit_program_entrypoint(&mut self) {
        let ident = "_start".into_c();
        let function = unsafe {
            let parameters = std::ptr::null_mut();

            let return_ty = LLVMVoidTypeInContext(self.context);
            let function_type = LLVMFunctionType(return_ty, parameters, 0 as _, false as _);

            LLVMAddFunction(self.module, ident.as_ptr(), function_type)
        };

        let entry = unsafe {
            LLVMAppendBasicBlockInContext(self.context, function, "entry".into_c().as_ptr())
        };
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, entry);

            let ty = match *self.ril.funcs.last().unwrap() {
                Func::Extern { return_ty, .. } => return_ty,
                Func::Decl { return_ty, .. } => return_ty,
            }
            .into_llvm(self);
            let func = *self.functions_mapping.last().unwrap();

            let args = std::ptr::null_mut();

            let name = "".into_c();

            LLVMBuildCall2(self.builder, ty, func, args, 0 as _, name.as_ptr());
            LLVMBuildRetVoid(self.builder);
        };
    }

    pub fn build(mut self, file_type: FileType) {
        for TypeDef { ty, .. } in &self.ril.types {
            ty.into_llvm(&self);
        }

        for func in self.ril.funcs.clone() {
            match func {
                Func::Decl {
                    name,
                    cfg,
                    return_ty,
                    params,
                } => {
                    let mut mapping = CFGMapping::new(cfg.mapping.clone());

                    let ident = name.into_c();
                    let function = unsafe {
                        let mut parameters = params
                            .iter()
                            .map(|param| param.ty.into_llvm(&self))
                            .collect_vec();

                        let return_ty = return_ty.into_llvm(&self);
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

                Func::Extern {
                    name: _,
                    return_ty: _,
                    params: _,
                } => todo!(),
            }
        }

        self.emit_program_entrypoint();

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
            .arg("-e main")
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

pub fn compile<'a>(rair: RIL<'a, 'a>, metadata: Metadata<'a>) {
    let code = CodeBuilder::new(rair, metadata);
    let filetype = FileType::Object;

    code.build(filetype);
}
