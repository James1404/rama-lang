use derive_more::Display;
use std::{fmt::Display, rc::Rc};

use crate::lexer::{TokenType};

#[derive(Debug, Clone, Copy, Display)]
pub enum UnOp {
    #[display("!")]
    Not,
    #[display("-")]
    Negate,
}

impl From<TokenType> for UnOp {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Not => Self::Not,
            TokenType::Minus => Self::Negate,

            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy, Display)]
pub enum BinOp {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,

    #[display("<")]
    Less,
    #[display("<=")]
    LessEq,
    #[display(">")]
    Greater,
    #[display(">=")]
    GreaterEq,
    #[display("==")]
    Eq,
    #[display("!=")]
    NotEq,
}

impl From<TokenType> for BinOp {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Asterix => Self::Mul,
            TokenType::Slash => Self::Div,

            TokenType::LAngle => Self::Less,
            TokenType::LessEq => Self::LessEq,
            TokenType::RAngle => Self::Greater,
            TokenType::GreaterEq => Self::GreaterEq,

            TokenType::EqualEqual => Self::Eq,
            TokenType::NotEqual => Self::NotEq,

            _ => panic!(),
        }
    }
}

impl BinOp {
    pub fn precedence(self) -> i32 {
        use BinOp::*;

        match self {
            Add => 1,
            Sub => 2,
            Mul => 3,
            Div => 4,
            Less | LessEq | Greater | GreaterEq | Eq | NotEq => 4,
        }
    }
}

#[derive(Debug, Display, Clone, Copy, Hash, PartialEq, Eq)]
#[display("{_0}")]
pub struct Ident<'a>(pub &'a str);

#[derive(Debug, Display)]
#[display("{ident}: {value}")]
pub struct LiteralRecordField<'a> {
    pub ident: Ident<'a>,
    pub value: Rc<Expr<'a>>,
}

#[derive(Debug, Display)]
#[display("{ident}: {ty}")]
pub struct RecordField<'a> {
    pub ident: Ident<'a>,
    pub ty: Rc<Type<'a>>,
}

#[derive(Debug)]
pub struct EnumVariant<'a> {
    pub ident: Ident<'a>,
    pub ty: Option<Rc<Type<'a>>>,
}

#[derive(Debug)]
pub enum Value<'a> {
    Ident(Ident<'a>),
    Float(&'a str),
    Int(&'a str),
    String(&'a str),
    Bool(bool),
    Record {
        fields: Vec<LiteralRecordField<'a>>,
    },

    Call {
        func: Rc<Expr<'a>>,
        args: Vec<Rc<Expr<'a>>>,
    },

    FieldAccess {
        value: Rc<Expr<'a>>,
        field: Ident<'a>,
    },
    Index {
        value: Rc<Expr<'a>>,
        index: usize,
    },

    Ref(Rc<Expr<'a>>),
    Deref(Rc<Expr<'a>>),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Value(Value<'a>),
    Binary {
        lhs: Rc<Expr<'a>>,
        op: BinOp,
        rhs: Rc<Expr<'a>>,
    },
    Unary {
        op: UnOp,
        value: Rc<Expr<'a>>,
    },
    Assign {
        lhs: Rc<Expr<'a>>,
        value: Rc<Expr<'a>>,
    },

    Cast {
        value: Rc<Expr<'a>>,
        ty: Rc<Type<'a>>,
    },

    If(If<'a>),
    Match(Match<'a>),
    Block(Block<'a>),
}

#[derive(Debug)]
pub enum Type<'a> {
    Ident(Ident<'a>),
    Record(Vec<RecordField<'a>>),
    Enum(Vec<EnumVariant<'a>>),
    Fn {
        params: Vec<Rc<Type<'a>>>,
        ret: Option<Rc<Type<'a>>>,
        external_linkage: bool,
    },
    Ptr(Rc<Type<'a>>),
    Slice(Rc<Type<'a>>),
    Array(Rc<Type<'a>>, usize),
}

#[derive(Debug)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
    pub result: Option<Rc<Expr<'a>>>,
}

#[derive(Debug)]
pub struct ConstDecl<'a> {
    pub ident: Ident<'a>,
    pub ty: Option<Rc<Type<'a>>>,
    pub value: Rc<Expr<'a>>,
}

#[derive(Debug)]
pub struct LetDecl<'a> {
    pub ident: Ident<'a>,
    pub ty: Option<Rc<Type<'a>>>,
    pub value: Rc<Expr<'a>>,
}

#[derive(Debug, Display)]
#[display("{ident}: {ty}")]
pub struct Param<'a> {
    pub ident: Ident<'a>,
    pub ty: Rc<Type<'a>>,
}

#[derive(Debug)]
pub struct ExternFn<'a> {
    pub ident: Ident<'a>,
    pub params: Vec<Param<'a>>,
    pub ret: Option<Rc<Type<'a>>>,
}

#[derive(Debug)]
pub struct Fn<'a> {
    pub ident: Ident<'a>,
    pub params: Vec<Param<'a>>,
    pub ret: Option<Rc<Type<'a>>>,
    pub block: Block<'a>,
}

#[derive(Debug)]
pub struct If<'a> {
    pub cond: Rc<Expr<'a>>,
    pub then: Block<'a>,
    pub otherwise: Option<Block<'a>>,
}

#[derive(Debug)]
pub struct MatchBranch<'a> {
    pub pattern: Rc<Expr<'a>>,
    pub value: Rc<Expr<'a>>,
}

#[derive(Debug)]
pub struct Match<'a> {
    pub value: Rc<Expr<'a>>,
    pub branches: Vec<MatchBranch<'a>>,
}

#[derive(Debug, Display)]
#[display("import \"{_0}\"")]
pub struct Import<'a>(pub &'a str);

#[derive(Debug)]
pub enum Statement<'a> {
    ConstDecl(ConstDecl<'a>),
    LetDecl(LetDecl<'a>),

    Expr(Rc<Expr<'a>>),

    Return(Rc<Expr<'a>>),
    ReturnNone,
}

#[derive(Debug)]
pub enum TopLevelStatement<'a> {
    ConstDecl(ConstDecl<'a>),
    LetDecl(LetDecl<'a>),

    Type {
        ident: Ident<'a>,
        inner: Rc<Type<'a>>,
    },

    ExternFn(ExternFn<'a>),
    Fn(Fn<'a>),

    Import(Import<'a>),
}

#[derive(Debug)]
pub struct AST<'a> {
    pub statements: Vec<TopLevelStatement<'a>>,
}

impl Display for EnumVariant<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(ty) = &self.ty {
            write!(f, ": {ty}")?;
        }

        Ok(())
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Ident(ident) => write!(f, "{ident}"),
            Float(v) => write!(f, "{v}"),
            Int(v) => write!(f, "{v}"),
            String(v) => write!(f, "\"{v}\""),
            Bool(v) => write!(f, "{v}"),
            Record { fields } => {
                writeln!(f, "{{")?;

                for field in fields {
                    writeln!(f, "{field};")?;
                }

                write!(f, "}}")
            }

            Call { func, args } => {
                write!(f, "{func}")?;
                write!(f, "(")?;

                let mut iter = args.iter().peekable();
                while let Some(arg) = iter.next() {
                    write!(f, "{arg}")?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            }

            FieldAccess { value, field } => write!(f, "{value}.{field}"),
            Index { value, index } => write!(f, "{value}[{index}]"),

            Ref(value) => write!(f, "&{value}"),
            Deref(value) => write!(f, "*{value}"),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expr::*;

        match self {
            Value(value) => write!(f, "{value}"),
            Binary { lhs, op, rhs } => write!(f, "{lhs} {op} {rhs}"),
            Unary { op, value } => write!(f, "{op}{value}"),
            Assign { lhs, value } => write!(f, "{lhs} = {value}"),
            Cast { value, ty } => write!(f, "{value} as {ty}"),
            If(v) => write!(f, "{v}"),
            Match(v) => write!(f, "{v}"),
            Block(block) => write!(f, "{block}"),
        }
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;

        match self {
            Ident(ident) => write!(f, "{ident}"),
            Record(fields) => {
                writeln!(f, "record")?;

                for field in fields {
                    writeln!(f, "\t{field}")?;
                }

                write!(f, "end")
            }
            Enum(variants) => {
                writeln!(f, "enum")?;

                for v in variants {
                    writeln!(f, "\t{v}")?;
                }

                write!(f, "end")
            }
            Fn {
                params,
                ret,
                external_linkage,
            } => {
                if *external_linkage {
                    write!(f, "extern ")?;
                }

                write!(f, "fn(")?;

                let mut iter = params.iter().peekable();
                while let Some(param) = iter.next() {
                    write!(f, "{param}")?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")?;

                if let Some(ret) = ret {
                    write!(f, " -> {ret}")?;
                }

                Ok(())
            }
            Ptr(ty) => write!(f, "*{ty}"),
            Slice(ty) => write!(f, "[{ty}]"),
            Array(ty, len) => write!(f, "[{ty}; {len}]"),
        }
    }
}

impl Display for Block<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            writeln!(f, "\t{s}")?;
        }

        if let Some(result) = &self.result {
            writeln!(f, "\t{}", result)?;
        }

        Ok(())
    }
}

impl Display for ConstDecl<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {}", self.ident)?;

        if let Some(ty) = &self.ty {
            write!(f, ": {ty}")?;
        }

        write!(f, " = {};", self.value)
    }
}

impl Display for LetDecl<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {}", self.ident)?;

        if let Some(ty) = &self.ty {
            write!(f, ": {ty}")?;
        }

        write!(f, " = {};", self.value)
    }
}

impl Display for ExternFn<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern fn {}(", self.ident)?;

        let mut iter = self.params.iter().peekable();
        while let Some(param) = iter.next() {
            write!(f, "{param}")?;

            if iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        write!(f, ")")?;

        if let Some(ret) = &self.ret {
            write!(f, " -> {ret}")?;
        }

        Ok(())
    }
}

impl Display for Fn<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.ident)?;

        let mut iter = self.params.iter().peekable();
        while let Some(param) = iter.next() {
            write!(f, "{param}")?;

            if iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        write!(f, ")")?;

        if let Some(ret) = &self.ret {
            write!(f, " -> {ret}")?;
        }

        writeln!(f, "")?;

        write!(f, "{}", self.block)?;

        write!(f, "end")
    }
}

impl Display for If<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "if {} then", self.cond)?;

        write!(f, "{}", self.then)?;

        if let Some(otherwise) = &self.otherwise {
            writeln!(f, "else")?;
            write!(f, "{}", otherwise)?;
        }

        writeln!(f, "end")
    }
}

impl Display for Match<'_> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for MatchBranch<'_> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Statement::*;

        match self {
            ConstDecl(v) => write!(f, "{v}"),
            LetDecl(v) => write!(f, "{v}"),
            Expr(expr) => write!(f, "{expr};"),
            Return(expr) => write!(f, "return {expr};"),
            ReturnNone => write!(f, "return;"),
        }
    }
}

impl Display for TopLevelStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TopLevelStatement::*;

        match self {
            ConstDecl(v) => write!(f, "{v}"),
            LetDecl(v) => write!(f, "{v}"),
            Type { ident, inner } => write!(f, "type {ident} is {inner}"),
            ExternFn(v) => write!(f, "{v}"),
            Fn(v) => write!(f, "{v}"),
            Import(v) => write!(f, "{v}"),
        }
    }
}

impl Display for AST<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{stmt}")?;
        }

        Ok(())
    }
}
