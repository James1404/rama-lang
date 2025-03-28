#pragma once
#include <string_view>

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
    int location;
    int line;
    int line_offset;
    std::string_view text;
};

TokenType string_to_tokentype(std::string_view str);
