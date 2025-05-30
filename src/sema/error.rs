use std::{fmt::Display, result};

use crate::{ast::Ref, lexer::TokenType, ty::TypeID};

#[derive(Debug, Clone)]
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
            SemaError::InvalidCast { .. } => todo!(),
            SemaError::InvalidBinaryTypes { .. } => todo!(),
            SemaError::NotDefined(ident) => write!(f, "Variable \"{}\" not defined", ident),
            SemaError::InvalidRootNode(_) => todo!(),
            SemaError::NoRootNode => todo!(),
            SemaError::CannotAssignToConst(_) => todo!(),
            SemaError::CannotReturnOutsideOfFunction => todo!(),
            SemaError::FunctionDoesNotHaveReturnType => todo!(),
            SemaError::Err(msg) => write!(f, "{}", msg),
        }
    }
}

pub type Result<'a, T> = result::Result<T, SemaError<'a>>;
