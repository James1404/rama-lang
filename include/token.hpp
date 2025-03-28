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

    Type,
    Enum,
    Struct,

    Const,
    Var,

    Import,
    
    Fn,

    For,
    While,
    Loop,

    Continue,
    Break,
    Return,
};

struct Token {
    TokenType type;
    i32 location;
    i32 line;
    i32 line_offset;
    string_view text;
};

TokenType string_to_tokentype(string_view str);
