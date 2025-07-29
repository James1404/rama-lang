use std::result;

use thiserror::Error;

use crate::{lexer::TokenType, ty::TypeRef};

#[derive(Debug, Clone, Error)]
pub enum SemaError<'a> {
    #[error("Invalid type")]
    InvalidType,
    #[error("Cannot cast {from} into {into}")]
    InvalidCast { from: TypeRef<'a>, into: TypeRef<'a> },
    #[error("Invalid binary types {lhs} and {rhs}")]
    InvalidBinaryTypes {
        lhs: TypeRef<'a>,
        op: TokenType,
        rhs: TypeRef<'a>,
    },
    #[error("Cannot assign {value} to {var}")]
    InvalidAssignment { var: TypeRef<'a>, value: TypeRef<'a> },

    #[error("Variable \"{_0}\" not defined")]
    NotDefined(&'a str),

    #[error("Cannot assign to const \"{_0}\"")]
    CannotAssignToConst(String),

    #[error("Cannot return outside of function")]
    CannotReturnOutsideOfFunction,
    #[error("Function does not have return type")]
    FunctionDoesNotHaveReturnType,

    #[error("{_0}")]
    Err(String),
}

pub type Result<'a, T> = result::Result<T, SemaError<'a>>;
