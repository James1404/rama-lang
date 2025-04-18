use std::fmt::Display;

use derive_more::Display;

extern crate llvm_sys as llvm;

#[derive(Debug, Clone, Copy, PartialEq, Display, Hash, Eq, PartialOrd, Ord)]
pub struct TypeID(pub usize);

#[derive(Debug, Clone, PartialEq)]
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
pub enum TypeVariable {
    Type(TypeID),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ADT<'a> {
    pub kind: ADTKind,
    pub fields: Vec<Field<'a>>,
    pub typevariables: Vec<TypeVariable>,
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
pub struct FnType<'a> {
    pub parameters: Vec<(&'a str, TypeID)>,
    pub return_ty: TypeID,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Void,

    Unit,

    Bool,
    Int { size: IntSize, signed: bool },
    Float(FloatKind),

    Slice(TypeID),
    Array { inner: TypeID, len: usize },

    ADT(ADT<'a>),

    Ptr(TypeID),

    Fn(FnType<'a>),

    Ref(TypeID),
}

impl<'a> Type<'a> {
    pub fn uint(size: IntSize) -> Type<'a> {
        Type::Int {
            size,
            signed: false,
        }
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

    pub fn get(&self, id: TypeID) -> Type<'a> {
        match self.data[id.0].clone() {
            Type::Ref(ty) => self.get(ty),
            ty => ty,
        }
    }

    pub fn display(&self, ty: TypeID) -> TypeFmt {
        TypeFmt { ctx: self, ty }
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
            Type::Void => write!(f, "void"),
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Int { size, signed } => write!(
                f,
                "{}{}",
                if signed { "i" } else { "u" },
                match size {
                    IntSize::Bits8 => "8",
                    IntSize::Bits16 => "16",
                    IntSize::Bits32 => "32",
                    IntSize::Bits64 => "64",
                    IntSize::BitsPtr => "size",
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
            Type::Fn(FnType {
                parameters,
                return_ty,
            }) => {
                f.write_str("fn(")?;
                let mut iter = parameters.iter().peekable();
                while let Some(param) = iter.next() {
                    write!(f, "{}: {}", param.0, self.ctx.display(param.1))?;

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
