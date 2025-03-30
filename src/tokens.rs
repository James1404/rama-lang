use phf::phf_map;
use strum_macros::IntoStaticStr;

#[derive(Debug, Clone, PartialEq, IntoStaticStr)]
pub enum TokenType {
    // Values
    String,
    Int,
    Float,
    Ident,

    // Symbols
    Dot,
    Comma,
    Colon,
    Semicolon,

    Plus,
    Asterix,
    Minus,
    Slash,
    Percent,

    PlusEq,
    MulEq,
    MinusEq,
    DivEq,
    ModEq,

    Less,
    Greater,
    LessEq,
    GreaterEq,

    Equal,
    Not,

    NotEqual,
    EqualEqual,

    LParen,
    RParen,

    LBracket,
    RBracket,

    LBrace,
    RBrace,

    Arrow,

    // Keywords
    True,
    False,

    If,
    Else,

    For,
    While,
    Loop,

    Return,

    Const,
    Var,

    And,
    Or,

    Fn,
    Extern,

    Type,
    Struct,
    Enum,
    Distinct,

    Defer,
    Comptime,

    As,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub text: &'a str,
    pub line: usize,
    pub location: usize,
    pub ty: TokenType,
}

pub static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "true" => TokenType::True,
    "false" => TokenType::False,

    "if" => TokenType::If,
    "else" => TokenType::Else,
    
    "for" => TokenType::For,
    "while" => TokenType::While,
    "loop" => TokenType::Loop,

    "return" => TokenType::Return,

    "const" => TokenType::Const,
    "var" => TokenType::Var,

    "and" => TokenType::And,
    "or" => TokenType::Or,

    "fn" => TokenType::Fn,
    "extern" => TokenType::Extern,
    
    "type" => TokenType::Type,
    "struct" => TokenType::Struct,
    "enum" => TokenType::Enum,
    "distinct" => TokenType::Distinct,

    "defer" => TokenType::Defer,
    "comptime" => TokenType::Comptime,

    "as" => TokenType::As,
};
