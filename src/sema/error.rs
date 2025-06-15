use std::result;

use thiserror::Error;

use crate::{ast::Ref, lexer::TokenType, ty::TypeID};

#[derive(Debug, Clone, Error)]
pub enum SemaError<'a> {
    #[error("Invalid type")]
    InvalidType,
    #[error("Invalid term {_0}")]
    InvalidTerm(Ref),
    #[error("Cannot cast {from} into {into}")]
    InvalidCast { from: TypeID, into: TypeID },
    #[error("Invalid binary types {lhs} and {rhs}")]
    InvalidBinaryTypes {
        lhs: TypeID,
        op: TokenType,
        rhs: TypeID,
    },
    #[error("Cannot assign {value} to {var}")]
    InvalidAssignment { var: TypeID, value: TypeID },

    #[error("Variable \"{_0}\" not defined")]
    NotDefined(&'a str),

    #[error("Invalid root node")]
    InvalidRootNode(Ref),
    #[error("No root node")]
    NoRootNode,

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
