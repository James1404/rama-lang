use crate::{
    ast::{self, AST},
    tokens::{Token, TokenType},
};

struct Cursor<'a> {
    position: usize,
    tokens: Vec<Token<'a>>,
}

impl<'a> Cursor<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            position: 0,
            tokens,
        }
    }

    fn eof(&self) -> bool {
        self.position >= self.tokens.len()
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn current(&self) -> Token {
        self.tokens[self.position].clone()
    }

    fn peek(&self) -> Option<Token> {
        let index = self.position + 1;

        if index > self.tokens.len() {
            None
        } else {
            Some(self.tokens[index].clone())
        }
    }

    fn matches(&self, expected: &TokenType) -> bool {
        self.current().ty == *expected
    }

    fn matches_next(&self, expected: &TokenType) -> bool {
        let next = self.peek();

        match next {
            None => false,
            Some(tok) => tok.ty == *expected,
        }
    }

    fn advance_if(&'a mut self, expected: TokenType) -> Option<Token<'a>> {
        match self.tokens.get(self.position) {
            Some(tok) => {
                if tok.ty == expected {
                    self.position += 1;
                    Some(tok.clone())
                } else {
                    None
                }
            }
            None => None,
        }
    }
}

pub struct Parser<'a> {
    ast: AST<'a>,
    cursor: Cursor<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            cursor: Cursor::new(tokens),
            ast: AST::new(),
        }
    }

    fn parse_ident(&'a mut self) -> ast::Ref {
        let tok = self.cursor.current();

        match tok.ty {
            TokenType::Ident => self.ast.alloc(ast::Node::Ident(tok)),
            _ => self.ast.alloc(ast::Node::Error {
                msg: "Unable to parse identifier".to_string(),
                token: tok,
            }),
        }
    }

    fn run(self) -> AST<'a> {
        self.ast
    }
}
