use std::{fmt::Display, rc::Rc};
use itertools::izip;

pub type TypeRef<'a> = Rc<Type<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: Rc<Type<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant<'a> {
    pub name: &'a str,
    pub ty: Option<Rc<Type<'a>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum<'a> {
    pub tags: Vec<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sum<'a> {
    pub variants: Vec<Variant<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record<'a> {
    pub fields: Vec<Field<'a>>,
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
pub struct Param<'a> {
    pub name: &'a str,
    pub xsty: Rc<Type<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType<'a> {
    pub parameters: Vec<Param<'a>>,
    pub return_ty: Rc<Type<'a>>,
}

#[derive(Debug, Clone, Eq)]
pub enum Type<'a> {
    Unit,

    Bool,
    Int { size: IntSize, signed: bool },
    Float(FloatKind),
    Str,

    Slice(Rc<Type<'a>>),
    Array { inner: Rc<Type<'a>>, len: usize },

    Enum(Enum<'a>),
    Record(Record<'a>),
    Sum(Sum<'a>),

    Ptr(Rc<Type<'a>>),

    Fn(FnType<'a>),

    Ref(Rc<Type<'a>>),

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

impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
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

            (Type::Slice(lhs), Type::Slice(rhs)) => lhs == rhs,
            (
                Type::Array {
                    inner: lhs,
                    len: llen,
                },
                Type::Array {
                    inner: rhs,
                    len: rlen,
                },
            ) => lhs == rhs && llen == rlen,

            (Type::Enum(lhs), Type::Enum(rhs)) => lhs == rhs,
            (Type::Record(lhs), Type::Record(rhs)) => 'outer: {
                for (l, r) in izip!(&lhs.fields, &rhs.fields) {
                    if l.name != r.name || l.ty != r.ty {
                        break 'outer false;
                    }
                }

                true
            }
            (Type::Sum(lhs), Type::Sum(rhs)) => 'outer: {
                for (l, r) in izip!(&lhs.variants, &rhs.variants) {
                    if l.name != r.name {
                        break 'outer false;
                    }

                    match (&l.ty, &r.ty) {
                        (Some(l), Some(r)) => {
                            if l != r {
                                break 'outer false;
                            }
                        }
                        (None, None) => {}
                        _ => break 'outer false,
                    }
                }

                true
            }

            (Type::Ptr(lhs), Type::Ptr(rhs)) => lhs == rhs,

            (Type::Fn(lhs), Type::Fn(rhs)) => 'outer: {
                for (l, r) in izip!(&lhs.parameters, &rhs.parameters) {
                    if l.name != r.name || l.ty != r.ty {
                        break 'outer false;
                    }
                }

                lhs.return_ty == rhs.return_ty
            }

            _ => false,
        }
    }
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Int { size, signed } => write!(
                f,
                "{}{}",
                if *signed { "i" } else { "u" },
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
            Type::Slice(inner) => write!(f, "[{inner}]"),
            Type::Array { inner, len } => write!(f, "[{inner}; {len}]"),
            Type::Enum(v) => {
                write!(f, "enum {{")?;
                for tag in v.tags.iter() {
                    write!(f, "{tag}")?;
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
            Type::Sum(v) => {
                write!(f, "sum {{")?;
                for variant in v.variants.iter() {
                    write!(f, "{}", variant.name)?;
                    if let Some(ref ty) = variant.ty {
                        write!(f, ": {ty}")?;
                    }
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
            Type::Record(v) => {
                write!(f, "struct {{")?;
                for field in v.fields.iter() {
                    write!(f, "{}", field.name)?;
                    write!(f, ": {}", field.ty)?;
                    write!(f, ",")?;
                }
                write!(f, "}}")
            }
            Type::Ptr(inner) => write!(f, "*{inner}"),
            Type::Fn(FnType {
                parameters,
                return_ty,
            }) => {
                f.write_str("fn(")?;
                let mut iter = parameters.iter().peekable();
                while let Some(param) = iter.next() {
                    write!(f, "{}: {}", param.name, param.ty)?;

                    if iter.peek().is_some() {
                        f.write_str(", ")?;
                    }
                }

                write!(f, ")")?;
                write!(f, " -> {}", return_ty)?;

                Ok(())
            }
            Type::Ref(ty) => write!(f, "Ref({ty})"),

            Type::Existential => write!(f, "Existential"),
            Type::Universal => write!(f, "Universal"),
        }
    }
}
