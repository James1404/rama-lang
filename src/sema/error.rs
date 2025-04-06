use std::result;

use crate::{
    ast,
    sema::types::TypeID,
    tokens::{Token, TokenType},
};

#[derive(Debug, Clone, Copy)]
pub enum SemaError<'a> {
    InvalidType,
    InvalidTerm(ast::Ref),
    InvalidCast {
        from: TypeID,
        into: TypeID,
    },
    InvalidBinaryTypes {
        lhs: TypeID,
        op: TokenType,
        rhs: TypeID,
    },
    NotDefined(Token<'a>),

    InvalidRootNode(ast::Ref),
    NoRootNode,

    CannotAssignToConst(Token<'a>),

    CannotReturnOutsideOfFunction,
    FunctionDoesNotHaveReturnType,

    Err(&'a str),
}

pub struct Error<'a> {
    pub msg: String,
    pub data: SemaError<'a>,
}

impl<'a> Error<'a> {
    pub fn new(msg: String, data: SemaError<'a>) -> Self {
        Self { data, msg }
    }
}

pub type Result<'a, T> = result::Result<T, SemaError<'a>>;
