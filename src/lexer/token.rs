use crate::lexer::position::Position;
use phf::phf_map;
use strum_macros::IntoStaticStr;

#[derive(Debug, Clone, Copy, PartialEq, IntoStaticStr)]
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

    LAngle,
    RAngle,

    StructConstructor, // .{
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
    Interface,

    Defer,
    Comptime,

    Import,

    As,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub text: &'a str,
    pub ty: TokenType,
    pub pos: Position,
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
    "interface" => TokenType::Interface,

    "defer" => TokenType::Defer,
    "comptime" => TokenType::Comptime,

    "import" => TokenType::Import,

    "as" => TokenType::As,
};

pub fn precedence(token: Token<'_>) -> i32 {
    match token.ty {
        TokenType::Plus => 1,
        TokenType::Minus => 2,
        TokenType::Asterix => 3,
        TokenType::Slash => 4,

        TokenType::LAngle
        | TokenType::LessEq
        | TokenType::RAngle
        | TokenType::GreaterEq
        | TokenType::EqualEqual
        | TokenType::NotEqual => 4,

        _ => -1,
    }
}
