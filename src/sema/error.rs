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
    NotDefined {
        token: Token<'a>,
    },

    InvalidRootNode(ast::Ref),
    NoRootNode,

    CannotReturnOutsideOfFunction,
    FunctionDoesNotHaveReturnType,
}

pub type Result<'a, T> = result::Result<T, SemaError<'a>>;
