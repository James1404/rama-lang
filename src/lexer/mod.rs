mod position;
pub mod token;

use position::Position;

pub use crate::lexer::token::{Token, TokenType, precedence};

pub struct Lexer<'a> {
    position: Position,
    src: &'a [u8],
    out: Vec<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            position: Position {
                current: 0,
                start: 0,
                line: 1,
                offset: 0,
            },
            src: src.as_bytes(),

            out: vec![],
        }
    }

    fn current(&self) -> u8 {
        self.src[self.position.current]
    }

    fn peek(&self) -> Option<u8> {
        if self.position.current + 1 < self.src.len() {
            Some(self.src[self.position.current + 1])
        } else {
            None
        }
    }

    fn append(&mut self, ty: TokenType) {
        let text = match std::str::from_utf8(&self.src[self.position.start..self.position.current])
        {
            Ok(text) => text,
            Err(err) => panic!("{:?}", err),
        };

        let ty = if ty == TokenType::Ident {
            match token::KEYWORDS.get(text).cloned() {
                Some(ty) => ty,
                None => TokenType::Ident,
            }
        } else {
            ty
        };

        let token = Token {
            text,
            ty,
            pos: self.position,
        };

        self.out.push(token);
    }

    fn append_single(&mut self, ty: TokenType) {
        self.advance();
        self.append(ty);
    }

    fn append_single_or_next(&mut self, cond: u8, t: TokenType, f: TokenType) {
        self.advance();
        let tok = if self.advance_if(cond) { t } else { f };
        self.append(tok);
    }

    fn matches(&self, c: u8) -> bool {
        self.current() == c
    }

    fn advance(&mut self) {
        self.position.current += 1;
    }

    fn advance_if(&mut self, cond: u8) -> bool {
        if self.matches(cond) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn run(mut self) -> Vec<Token<'a>> {
        while self.position.current < self.src.len() {
            self.position.start = self.position.current;

            match self.current() {
                b'(' => self.append_single(TokenType::LParen),
                b')' => self.append_single(TokenType::RParen),
                b'{' => self.append_single(TokenType::LBrace),
                b'}' => self.append_single(TokenType::RBrace),
                b'[' => self.append_single(TokenType::LBracket),
                b']' => self.append_single(TokenType::RBracket),

                b'.' => {
                    self.append_single_or_next(b'{', TokenType::StructConstructor, TokenType::Dot)
                }
                b',' => self.append_single(TokenType::Comma),

                b':' => self.append_single(TokenType::Colon),
                b';' => self.append_single(TokenType::Semicolon),

                b'+' => self.append_single_or_next(b'=', TokenType::PlusEq, TokenType::Plus),
                b'-' => match self.peek() {
                    Some(b'=') => {
                        self.advance();
                        self.append_single(TokenType::MinusEq)
                    }
                    Some(b'>') => {
                        self.advance();
                        self.append_single(TokenType::Arrow)
                    }
                    _ => self.append_single(TokenType::Minus),
                },
                b'*' => self.append_single_or_next(b'=', TokenType::MulEq, TokenType::Asterix),

                b'/' => {
                    self.advance();

                    match self.current() {
                        b'/' => {
                            while !self.matches(b'\n') {
                                self.advance();
                            }
                        }
                        b'*' => todo!("Add multiline comments"),
                        b'=' => self.append_single(TokenType::DivEq),
                        _ => self.append(TokenType::Slash),
                    }
                }

                b'>' => self.append_single_or_next(b'=', TokenType::LessEq, TokenType::RAngle),
                b'<' => self.append_single_or_next(b'=', TokenType::GreaterEq, TokenType::LAngle),

                b'=' => self.append_single_or_next(b'=', TokenType::EqualEqual, TokenType::Equal),
                b'!' => self.append_single_or_next(b'=', TokenType::NotEqual, TokenType::Not),

                b'"' => {
                    self.advance();

                    while self.current() != b'"' {
                        self.advance();
                    }

                    self.position.start += 1;
                    self.append(TokenType::String);

                    self.advance();
                }

                b' ' | b'\t' => self.advance(),
                b'\n' | b'\r' => {
                    self.advance();
                    self.position.line += 1;
                }

                c if c.is_ascii_digit() => {
                    self.advance();

                    let mut ty = TokenType::Int;

                    loop {
                        match self.current() {
                            b'.' => ty = TokenType::Float,
                            c if c.is_ascii_digit() => {}
                            _ => break,
                        }

                        self.advance();
                    }

                    self.append(ty);
                }
                c if c.is_ascii_alphabetic() || c == b'_' => {
                    self.advance();

                    loop {
                        match self.current() {
                            b'_' => {}
                            c if c.is_ascii_alphanumeric() => {}
                            _ => break,
                        }

                        self.advance();
                    }

                    self.append(TokenType::Ident);
                }

                c => panic!(
                    "Not implemented for character '{}'",
                    std::str::from_utf8(std::slice::from_ref(&c)).unwrap()
                ),
            }
        }

        self.out
    }
}
