use std::fmt::Display;

use crate::lexer::position::Position;
use phf::phf_map;
use strum_macros::IntoStaticStr;
use derive_more::Display;

#[derive(Debug, Clone, Copy, PartialEq, IntoStaticStr, Display)]
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

    RecordConstructor, // .{
    LBrace,
    RBrace,

    Arrow,

    // Keywords
    Begin,
    End,
    Is,

    If,
    Then,
    Else,        

    True,
    False,

    For,
    While,
    Loop,

    Return,

    Const,
    Var,
    Let,

    And,
    Or,

    Fn,
    Extern,

    Type,
    Record,
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

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

pub static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "begin" => TokenType::Begin,
    "end" => TokenType::End,
    "is" => TokenType::Is,

    "if" => TokenType::If,
    "then" => TokenType::Then,
    "else" => TokenType::Else,

    "true" => TokenType::True,
    "false" => TokenType::False,

    "for" => TokenType::For,
    "while" => TokenType::While,
    "loop" => TokenType::Loop,

    "return" => TokenType::Return,

    "const" => TokenType::Const,
    "var" => TokenType::Var,
    "let" => TokenType::Let,

    "and" => TokenType::And,
    "or" => TokenType::Or,

    "fn" => TokenType::Fn,
    "extern" => TokenType::Extern,

    "type" => TokenType::Type,
    "record" => TokenType::Record,
    "enum" => TokenType::Enum,
    "distinct" => TokenType::Distinct,
    "interface" => TokenType::Interface,

    "defer" => TokenType::Defer,
    "comptime" => TokenType::Comptime,

    "import" => TokenType::Import,

    "as" => TokenType::As,
};
