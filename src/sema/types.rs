#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeID(usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Field<'a> {
    pub ident: &'a str,
    pub ty: Option<TypeID>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntKind {
    I8,
    I16,
    I32,
    I64,
    ISize,

    U8,
    U16,
    U32,
    U64,
    USize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Void,
    
    Bool,
    Int(IntKind),
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

    Existential,
    Universal,
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
}
