use crate::tokens::Token;

pub type Ref = usize;

pub enum Node<'a> {
    Error {
        msg: String,
        token: Token<'a>,
    },

    Binary {
        lhs: Ref,
        rhs: Ref,
        op: Token<'a>,
    },
    Unary {
        value: Ref,
        op: Token<'a>,
    },

    Float(&'a str),
    Int(&'a str),
    String(&'a str),
    Bool(bool),
    Ident(Token<'a>),

    TopLevelScope(Vec<Ref>),
    Scope(Vec<Ref>),

    ConstDecl {
        ident: Ref,
        ty: Option<Ref>,
        value: Ref,
    },
    VarDecl {
        ident: Ref,
        ty: Option<Ref>,
        value: Ref,
    },

    Assignment {
        ident: Ref,
        value: Ref,
    },

    ParameterList(Vec<Ref>),
    Paramater {
        ident: Ref,
        ty: Ref,
    },

    FnDecl {
        params: Ref,
        ret: Ref,
        block: Ref,
    },
    FnCall {
        func: Ref,
        args: Vec<Ref>,
    },

    Return(Ref),
    ImplicitReturn(Ref),

    Dot {
        lhs: Ref,
        ident: Ref,
    },

    If {
        cond: Ref,
        t: Ref,
        f: Ref,
    },

    Match {
        value: Ref,
        branches: Vec<Ref>,
    },
    MatchBranch {
        pattern: Ref,
        value: Ref,
    },

    Comptime(Ref),
    Extern(Ref),

    FnType {
        params: Ref,
        ret: Ref,
    },

    Field {
        ident: Ref,
        ty: Option<Ref>,
        default: Option<Ref>,
    },

    Struct {
        fields: Vec<Ref>,
    },
    Distinct(Ref),
    PtrType(Ref),

    Defer(Ref),

    Cast {
        value: Ref,
        ty: Ref,
    },
}

pub struct AST<'a> {
    data: Vec<Node<'a>>,
    root: Option<Ref>,
}

impl<'a> AST<'a> {
    pub fn new() -> Self {
        Self {
            data: vec![],
            root: None,
        }
    }

    pub fn alloc(&'a mut self, node: Node<'a>) -> Ref {
        let index = self.data.len();
        self.data.push(node);
        return index;
    }
}
