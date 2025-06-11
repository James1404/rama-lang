use std::fmt::Display;

use bumpalo::Bump;
use derive_more::Display;
use itertools::izip;

#[derive(Debug, Clone, Copy, PartialEq, Display, Hash, Eq, PartialOrd, Ord)]
pub struct TypeID(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: TypeID,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeVariable(pub TypeID);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant<'a> {
    pub name: &'a str,
    pub ty: Option<TypeID>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum<'a> {
    pub tags: Vec<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sum<'a> {
    pub variants: Vec<Variant<'a>>,
    pub typevariables: Vec<TypeVariable>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct<'a> {
    pub fields: Vec<Field<'a>>,
    pub typevariables: Vec<TypeVariable>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
    BitsPtr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType<'a> {
    pub parameters: Vec<(&'a str, TypeID)>,
    pub return_ty: Option<TypeID>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'a> {
    Void,

    Unit,

    Bool,
    Int { size: IntSize, signed: bool },
    Float(FloatKind),
    Str,

    Slice(TypeID),
    Array { inner: TypeID, len: usize },

    Enum(Enum<'a>),
    Struct(Struct<'a>),
    Sum(Sum<'a>),

    Ptr(TypeID),

    Fn(FnType<'a>),

    Ref(TypeID),

    Existential,
    Universal,
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

    pub fn from_str(ident: &str) -> Option<Type<'a>> {
        match ident {
            "void" => Some(Type::Void),

            "i8" => Some(Type::int(IntSize::Bits8)),
            "i16" => Some(Type::int(IntSize::Bits16)),
            "i32" => Some(Type::int(IntSize::Bits32)),
            "i64" => Some(Type::int(IntSize::Bits64)),
            "isize" => Some(Type::int(IntSize::BitsPtr)),

            "u8" => Some(Type::uint(IntSize::Bits8)),
            "u16" => Some(Type::uint(IntSize::Bits16)),
            "u32" => Some(Type::uint(IntSize::Bits32)),
            "u64" => Some(Type::uint(IntSize::Bits64)),
            "usize" => Some(Type::uint(IntSize::BitsPtr)),

            "f32" => Some(Type::Float(FloatKind::F32)),
            "f64" => Some(Type::Float(FloatKind::F64)),

            "bool" => Some(Type::Bool),
            "str" => Some(Type::Str),

            _ => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeContext<'a> {
    pub data: Vec<Type<'a>>,
    pub arena: Bump,
}

impl<'a> TypeContext<'a> {
    pub fn new() -> Self {
        Self {
            data: vec![],
            arena: Bump::new(),
        }
    }

    pub fn alloc(&mut self, ty: Type<'a>) -> TypeID {
        let index = self.data.len();
        self.data.push(ty);
        TypeID(index)
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

    pub fn eq(&self, lhs: TypeID, rhs: TypeID) -> bool {
        match (self.get(lhs), self.get(rhs)) {
            (Type::Void, Type::Void) => true,
            (Type::Unit, Type::Unit) => true,

            (Type::Bool, Type::Bool) => true,
            (
                Type::Int {
                    signed: lsign,
                    size: lsize,
                },
                Type::Int {
                    signed: rsign,
                    size: rsize,
                },
            ) => lsign == rsign && lsize == rsize,
            (Type::Float(lhs), Type::Float(rhs)) => lhs == rhs,
            (Type::Str, Type::Str) => true,

            (Type::Slice(lhs), Type::Slice(rhs)) => self.eq(lhs, rhs),
            (
                Type::Array {
                    inner: lhs,
                    len: llen,
                },
                Type::Array {
                    inner: rhs,
                    len: rlen,
                },
            ) => self.eq(lhs, rhs) && llen == rlen,

            (Type::Enum(lhs), Type::Enum(rhs)) => lhs == rhs,
            (Type::Struct(lhs), Type::Struct(rhs)) => 'outer: {
                for (l, r) in izip!(lhs.fields, rhs.fields) {
                    if l.name != r.name || !self.eq(l.ty, r.ty) {
                        break 'outer false;
                    }
                }

                for (l, r) in izip!(lhs.typevariables, rhs.typevariables) {
                    if !self.eq(l.0, r.0) {
                        break 'outer false;
                    }
                }

                true
            }
            (Type::Sum(lhs), Type::Sum(rhs)) => 'outer: {
                for (l, r) in izip!(lhs.variants, rhs.variants) {
                    if l.name != r.name {
                        break 'outer false;
                    }

                    match (l.ty, r.ty) {
                        (Some(l), Some(r)) => {
                            if !self.eq(l, r) {
                                break 'outer false;
                            }
                        }
                        (None, None) => {}
                        _ => break 'outer false,
                    }
                }

                for (l, r) in izip!(lhs.typevariables, rhs.typevariables) {
                    if !self.eq(l.0, r.0) {
                        break 'outer false;
                    }
                }

                true
            }

            (Type::Ptr(lhs), Type::Ptr(rhs)) => self.eq(lhs, rhs),

            (Type::Fn(lhs), Type::Fn(rhs)) => 'outer: {
                for (l, r) in izip!(lhs.parameters, rhs.parameters) {
                    if l.0 != r.0 || !self.eq(l.1, r.1) {
                        break 'outer false;
                    }
                }

                match (lhs.return_ty, rhs.return_ty) {
                    (Some(lhs), Some(rhs)) if !self.eq(lhs, rhs) => false,
                    _ => true,
                }
            }

            _ => false,
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
            Type::Str => write!(f, "str"),
            Type::Slice(inner) => write!(f, "[{}]", self.ctx.display(inner)),
            Type::Array { inner, len } => write!(f, "[{}; {}]", self.ctx.display(inner), len),
            Type::Enum(v) => {
                write!(f, "enum {{")?;
                for tag in v.tags {
                    write!(f, "{}", tag)?;
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
            Type::Sum(v) => {
                write!(f, "sum {{")?;
                for variant in v.variants {
                    write!(f, "{}", variant.name)?;
                    if let Some(ty) = variant.ty {
                        write!(f, ": {}", self.ctx.display(ty))?;
                    }
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
            Type::Struct(v) => {
                write!(f, "struct {{")?;
                for field in v.fields {
                    write!(f, "{}", field.name)?;
                    write!(f, ": {}", self.ctx.display(field.ty))?;
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

                    if iter.peek().is_some() {
                        f.write_str(", ")?;
                    }
                }

                write!(f, ")")?;

                if let Some(ret) = return_ty {
                    write!(f, " -> {}", self.ctx.display(ret))?;
                }

                Ok(())
            }
            Type::Ref(ty) => write!(f, "Ref({}, {})", ty.0, self.ctx.display(ty)),

            Type::Existential => write!(f, "Existential"),
            Type::Universal => write!(f, "Universal"),
        }
    }
}
