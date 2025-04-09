use std::{fmt::Display, result};

use crate::{
    ast::{self, Ref}, lexer::{Token, TokenType}, types::TypeID
};

#[derive(Debug, Clone, )]
pub enum SemaError<'a> {
    InvalidType,
    InvalidTerm(Ref),
    InvalidCast {
        from: TypeID,
        into: TypeID,
    },
    InvalidBinaryTypes {
        lhs: TypeID,
        op: TokenType,
        rhs: TypeID,
    },
    NotDefined(&'a str),

    InvalidRootNode(Ref),
    NoRootNode,

    CannotAssignToConst(String),

    CannotReturnOutsideOfFunction,
    FunctionDoesNotHaveReturnType,

    Err(String),
}

impl<'a> Display for SemaError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemaError::InvalidType => write!(f, "Invalid Type"),
            SemaError::InvalidTerm(term) => write!(f, "Invalid term {}", term),
            SemaError::InvalidCast { from, into } => todo!(),
            SemaError::InvalidBinaryTypes { lhs, op, rhs } => todo!(),
            SemaError::NotDefined(ident) => write!(f, "Variable \"{}\" not defined", ident),
            SemaError::InvalidRootNode(_) => todo!(),
            SemaError::NoRootNode => todo!(),
            SemaError::CannotAssignToConst(token) => todo!(),
            SemaError::CannotReturnOutsideOfFunction => todo!(),
            SemaError::FunctionDoesNotHaveReturnType => todo!(),
            SemaError::Err(msg) => write!(f, "{}", msg),
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
