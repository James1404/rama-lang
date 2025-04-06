use std::{fmt::Display, result};

use crate::{
    ast,
    sema::types::TypeID,
    lexer::{Token, TokenType},
};

#[derive(Debug, Clone, )]
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

    Err(String),
}

impl<'a> Display for SemaError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemaError::InvalidType => todo!(),
            SemaError::InvalidTerm(_) => todo!(),
            SemaError::InvalidCast { from, into } => todo!(),
            SemaError::InvalidBinaryTypes { lhs, op, rhs } => todo!(),
            SemaError::NotDefined(token) => todo!(),
            SemaError::InvalidRootNode(_) => todo!(),
            SemaError::NoRootNode => todo!(),
            SemaError::CannotAssignToConst(token) => todo!(),
            SemaError::CannotReturnOutsideOfFunction => todo!(),
            SemaError::FunctionDoesNotHaveReturnType => todo!(),
            SemaError::Err(msg) => write!(f, "Err \"{}\"", msg),
        }
    }
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
