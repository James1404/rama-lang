#pragma once

#include "common.hpp"

enum class TokenType {
    None,

    // Values
    Ident,
    Float,
    Int,
    String,

    // Symbols
    Plus,
    Minus,
    Multiply,
    Divide,

    PlusEq,
    MinusEq,
    MultiplyEq,
    DivideEq,

    Less,
    Greater,
    LessEq,
    GreaterEq,
    EqualEq,
    NotEq,

    Equal,
    Not,

    SemiColon, Colon,
    Comma, Dot,

    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,

    // Keywords
    True, False,

    If,
    Else,

    For,
    While,
    Loop,

    Continue,
    Break,
    Return,

    And,
    Or,

    Comptime,
    Extern,
    
    Type,
    Enum,
    Struct,
    Distinct,

    Const,
    Var,

    Import,
    
    Fn,

    Defer,
    As,
};

struct Token {
    TokenType type;
    i32 location;
    i32 line;
    i32 line_offset;
    string_view text;
};

TokenType string_to_tokentype(string_view str);
