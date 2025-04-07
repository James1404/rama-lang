use std::fmt::{Display, Write};

extern crate llvm_sys as llvm;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeID(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Field<'a> {
    pub ident: &'a str,
    pub ty: Option<TypeID>,
}

#[derive(Debug, Clone, Copy, PartialEq, strum_macros::Display)]
pub enum ADTKind {
    Struct,
    Enum,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericArg {
    Type(TypeID),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ADT<'a> {
    pub kind: ADTKind,
    pub fields: Vec<Field<'a>>,
    pub generic_args: Vec<GenericArg>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum IntSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
    BitsPtr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Unit,

    Void,

    Bool,
    Int {
        size: IntSize,
        signed: bool,
    },
    Float(FloatKind),

    Slice(TypeID),
    Array {
        inner: TypeID,
        len: usize,
    },

    ADT(ADT<'a>),

    Ptr(TypeID),

    Fn {
        parameters: Vec<TypeID>,
        return_ty: TypeID,
    },

    Ref(TypeID),
}

impl<'a> Type<'a> {
    pub fn uint(size: IntSize) -> Type<'a> {
        Type::Int { size, signed: false }
    }

    pub fn int(size: IntSize) -> Type<'a> {
        Type::Int { size, signed: true }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeContext<'a> {
    pub data: Vec<Type<'a>>,
}

impl<'a> TypeContext<'a> {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    pub fn alloc(&mut self, ty: Type<'a>) -> TypeID {
        let index = self.data.len();
        self.data.push(ty);
        return TypeID(index);
    }

    pub fn alloc_slice(&mut self, inner: Type<'a>) -> TypeID {
        let inner = self.alloc(inner);
        self.alloc(Type::Slice(inner))
    }

    pub fn alloc_array(&mut self, inner: Type<'a>, len: usize) -> TypeID {
        let inner = self.alloc(inner);
        self.alloc(Type::Array { inner, len })
    }

    pub fn get(&self, id: TypeID) -> Type {
        self.data[id.0].clone()
    }

    pub fn get_mut(&mut self, id: TypeID) -> &mut Type<'a> {
        &mut self.data[id.0]
    }

    pub fn display(&'a self, ty: TypeID) -> TypeFmt<'a> {
        TypeFmt { ctx: self, ty }
    }

    pub fn to_llvm(&'a self, context: *mut llvm::LLVMContext, ty: TypeID) -> *mut llvm::LLVMType {
        match self.get(ty) {
            Type::Unit => todo!(),
            Type::Void => unsafe { llvm::core::LLVMVoidTypeInContext(context) },
            Type::Bool => todo!(),
            Type::Int { size, signed } => unsafe {
                match (signed, size) {
                    (true, IntSize::Bits8) => llvm::core::LLVMInt8TypeInContext(context),
                    (true, IntSize::Bits16) => llvm::core::LLVMInt16TypeInContext(context),
                    (true, IntSize::Bits32) => llvm::core::LLVMInt32TypeInContext(context),
                    (true, IntSize::Bits64) => llvm::core::LLVMInt64TypeInContext(context),
                    (true, IntSize::BitsPtr) => llvm::core::LLVMInt64TypeInContext(context),
                    (false, IntSize::Bits8) => llvm::core::LLVMInt8TypeInContext(context),
                    (false, IntSize::Bits16) => llvm::core::LLVMInt16TypeInContext(context),
                    (false, IntSize::Bits32) => llvm::core::LLVMInt32TypeInContext(context),
                    (false, IntSize::Bits64) => llvm::core::LLVMInt64TypeInContext(context),
                    (false, IntSize::BitsPtr) => llvm::core::LLVMInt64TypeInContext(context),
                }
            },
            Type::Float(kind) => unsafe {
                match kind {
                    FloatKind::F32 => llvm::core::LLVMFloatTypeInContext(context),
                    FloatKind::F64 => llvm::core::LLVMDoubleTypeInContext(context),
                }
            },
            Type::Slice(type_id) => todo!(),
            Type::Array { inner, len } => unsafe {
                llvm::core::LLVMArrayType2(self.to_llvm(context, inner), len as u64)
            },
            Type::ADT(adt) => todo!(),
            Type::Ptr(inner) => unsafe {
                llvm::core::LLVMPointerTypeInContext(
                    context,
                    llvm::core::LLVMGetPointerAddressSpace(self.to_llvm(context, inner)),
                )
            },
            Type::Fn {
                parameters,
                return_ty,
            } => todo!(),
            Type::Ref(ty) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeFmt<'a> {
    ctx: &'a TypeContext<'a>,
    ty: TypeID,
}

impl<'a> Display for TypeFmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ctx.get(self.ty) {
            Type::Unit => write!(f, "()"),

            Type::Void => write!(f, "void"),
            Type::Bool => write!(f, "bool"),
            Type::Int { size, signed } => write!(
                f,
                "{}{}",
                if signed { "i" } else { "u" },
                match size {
                    IntSize::Bits8 => "i8",
                    IntSize::Bits16 => "i16",
                    IntSize::Bits32 => "i32",
                    IntSize::Bits64 => "i64",
                    IntSize::BitsPtr => "isize",
                }
            ),
            Type::Float(float_kind) => write!(
                f,
                "{}",
                match float_kind {
                    FloatKind::F32 => "f32",
                    FloatKind::F64 => "f64",
                }
            ),
            Type::Slice(inner) => write!(f, "[{}]", self.ctx.display(inner)),
            Type::Array { inner, len } => write!(f, "[{}; {}]", self.ctx.display(inner), len),
            Type::ADT(adt) => {
                write!(f, "{} {{", adt.kind)?;
                for field in adt.fields {
                    write!(f, "{}", field.ident)?;
                    if let Some(ty) = field.ty {
                        write!(f, ": {}", self.ctx.display(ty))?;
                    }
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
            Type::Ptr(inner) => write!(f, "*{}", self.ctx.display(inner)),
            Type::Fn {
                parameters,
                return_ty,
            } => {
                f.write_str("fn(")?;
                let mut iter = parameters.iter().peekable();
                while let Some(param) = iter.next() {
                    write!(f, "{}", self.ctx.display(*param))?;

                    if !iter.peek().is_none() {
                        f.write_str(", ")?;
                    }
                }

                write!(f, ") {}", self.ctx.display(return_ty))
            }
            Type::Ref(ty) => write!(f, "Ref({}, {})", ty.0, self.ctx.display(ty)),
        }
    }
}
