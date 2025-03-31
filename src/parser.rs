use crate::{
    ast::{self, AST},
    tokens::{Token, TokenType},
};

#[derive(Debug, Clone, Copy)]
struct Cursor<'a> {
    position: usize,
    tokens: &'a [Token<'a>],
}

impl<'a> Cursor<'a> {
    fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            position: 0,
            tokens: tokens.as_slice(),
        }
    }

    fn eof(self) -> bool {
        self.position >= self.tokens.len()
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn current(self) -> Token<'a> {
        self.tokens[self.position].clone()
    }

    fn peek(self) -> Option<Token<'a>> {
        let index = self.position + 1;

        if index > self.tokens.len() {
            None
        } else {
            Some(self.tokens[index].clone())
        }
    }

    fn matches(self, expected: &TokenType) -> bool {
        self.current().ty == *expected
    }

    fn matches_next(self, expected: &TokenType) -> bool {
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

#[derive(Clone)]
pub struct Parser<'a> {
    ast: AST<'a>,
    cursor: Cursor<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            cursor: Cursor::new(tokens),
            ast: AST::new(),
        }
    }

    fn advance_alloc(&'a mut self, node: ast::Node<'a>) -> ast::Ref {
        self.cursor.advance();
        self.ast.alloc(node)
    }

    fn parse_ident(&'a mut self) -> ast::Ref {
        let tok = self.cursor.current();

        match tok.ty {
            TokenType::Ident => self.ast.alloc(ast::Node::Ident(tok)),
            _ => self.ast.alloc(ast::Node::Error {
                msg: "Unable to parse identifier",
                token: tok,
            }),
        }
    }

    fn parse_if(&'a mut self) -> ast::Ref {
        todo!()
    }

    fn parse_expr(&'a mut self) -> ast::Ref {
        todo!()
    }

    fn parse_value(&'a mut self) -> ast::Ref {
        let tok = self.cursor.current();

        let value = match tok.ty {
            TokenType::String => self.advance_alloc(ast::Node::String(tok.text)),
            TokenType::Float => self.advance_alloc(ast::Node::Float(tok.text)),
            TokenType::Int => self.advance_alloc(ast::Node::Int(tok.text)),
            TokenType::Ident => self.advance_alloc(ast::Node::Ident(tok)),

            TokenType::True => self.advance_alloc(ast::Node::Bool(true)),
            TokenType::False => self.advance_alloc(ast::Node::Bool(false)),

            TokenType::If => self.parse_if(),

            TokenType::Comptime => {
                self.cursor.advance();
                let expr = self.parse_expr();
                self.ast.alloc(ast::Node::Comptime(expr))
            }

            _ => {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Unable to parse value",
                    token: tok,
                });
            }
        };

        value
    }

    fn parse_toplevel_stmt(&'a mut self) -> ast::Ref {
        let list = vec![];
        self.ast.alloc(ast::Node::TopLevelScope(list))
    }

    fn parse_toplevel(&'a mut self) -> ast::Ref {
        let mut list: Vec<ast::Ref> = vec![];

        loop {
            if self.cursor.eof() {
                break;
            }

            list.push(self.parse_toplevel_stmt());
        }

        self.ast.alloc(ast::Node::TopLevelScope(list))
    }

    pub fn run(mut self) -> AST<'a> {
        let toplevel = self.parse_toplevel();
        self.ast.root = Some(toplevel);

        self.ast
    }
}
