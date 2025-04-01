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

    fn matches(self, expected: TokenType) -> bool {
        if self.eof() {
            return false;
        }

        self.current().ty == expected
    }

    fn matches_next(self, expected: TokenType) -> bool {
        let next = self.peek();

        match next {
            None => false,
            Some(tok) => tok.ty == expected,
        }
    }

    fn advance_if(&mut self, expected: TokenType) -> bool {
        match self.tokens.get(self.position) {
            Some(tok) => {
                if tok.ty == expected {
                    self.position += 1;
                    true
                } else {
                    false
                }
            }
            None => false,
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

    fn alloc(&mut self, node: ast::Node<'a>) -> ast::Ref {
        self.ast.alloc(node)
    }

    fn advance_alloc(&mut self, node: ast::Node<'a>) -> ast::Ref {
        self.cursor.advance();
        self.alloc(node)
    }

    fn parse_ident(&'a mut self) -> ast::Ref {
        let tok = self.cursor.current();

        match tok.ty {
            TokenType::Ident => self.alloc(ast::Node::Ident(tok)),
            _ => self.alloc(ast::Node::Error {
                msg: "Unable to parse identifier",
                token: tok,
            }),
        }
    }

    fn expression(&mut self, lhs: ast::Ref, min_precedence: i32) -> ast::Ref {
        todo!()
    }

    fn parse_expr(&mut self) -> ast::Ref {
        let lhs = self.parse_value();
        self.expression(lhs, 0)
    }

    fn parse_comptime(&mut self) -> ast::Ref {
        todo!()
    }

    fn parse_value(&mut self) -> ast::Ref {
        let tok = self.cursor.current();

        let value = match tok.ty {
            TokenType::String => self.advance_alloc(ast::Node::String(tok.text)),
            TokenType::Float => self.advance_alloc(ast::Node::Float(tok.text)),
            TokenType::Int => self.advance_alloc(ast::Node::Int(tok.text)),
            TokenType::Ident => self.advance_alloc(ast::Node::Ident(tok)),

            TokenType::True => self.advance_alloc(ast::Node::Bool(true)),
            TokenType::False => self.advance_alloc(ast::Node::Bool(false)),

            TokenType::If => self.parse_if(),

            TokenType::Comptime => self.parse_comptime(),

            _ => {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Unable to parse value",
                    token: tok,
                });
            }
        };

        value
    }

    fn parse_stmt(&mut self) -> ast::Ref {
        todo!()
    }

    fn parse_scope(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        if !self.cursor.advance_if(TokenType::LBrace) {
            return self.advance_alloc(ast::Node::Error {
                msg: "Block needs opening brace",
                token,
            });
        }

        let mut list = Vec::<ast::Ref>::new();

        while !self.cursor.matches(TokenType::RBrace) {
            let stmt = self.parse_stmt();

            if self.cursor.advance_if(TokenType::Semicolon) {
                list.push(stmt);
            }
            else {
                list.push(self.alloc(ast::Node::ImplicitReturn(stmt)));
                break;
            }
        }

        if !self.cursor.advance_if(TokenType::RBrace) {
            return self.alloc(ast::Node::Error {
                msg: "Expect closing brace at end of statement",
                token,
            });
        }


        self.alloc(ast::Node::Scope(list))
    }

    fn parse_if(&mut self) -> ast::Ref {
        let tok = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::If) {
                break 'outer;
            }

            let cond = self.parse_expr();
            let t = self.parse_scope();

            if !self.cursor.advance_if(TokenType::Else) {
                break 'outer;
            }

            let f = self.parse_scope();

            return self.alloc(ast::Node::If { cond, t, f });
        }

        return self.advance_alloc(ast::Node::Error {
            msg: "Failed to parse if statement",
            token: tok,
        });
    }

    fn parse_toplevel_stmt(&mut self) -> Option<ast::Ref> {
        if self.cursor.eof() {
            return None;
        }

        let list = vec![];
        Some(self.alloc(ast::Node::TopLevelScope(list)))
    }

    fn parse_toplevel(&mut self) -> ast::Ref {
        let mut list: Vec<ast::Ref> = vec![];

        while let Some(node) = self.parse_toplevel_stmt() {
            list.push(node);
        }

        self.alloc(ast::Node::TopLevelScope(list))
    }

    pub fn run(mut self) -> AST<'a> {
        let toplevel = self.parse_toplevel();
        self.ast.root = Some(toplevel);

        self.ast
    }
}
