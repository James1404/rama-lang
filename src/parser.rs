use log::error;

use crate::{
    ast::{self, AST},
    tokens::{Token, TokenType, precedence},
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
        if self.eof() {
            return false;
        }

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
        match node {
            ast::Node::Error { msg, token } => {
                error!("[{}; {}] {}", token.line, token.location, msg)
            }
            _ => {}
        }
        self.ast.alloc(node)
    }

    fn advance_alloc(&mut self, node: ast::Node<'a>) -> ast::Ref {
        self.cursor.advance();
        self.alloc(node)
    }

    fn parse_ident(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Ident => self.advance_alloc(ast::Node::Ident(token)),
            _ => self.advance_alloc(ast::Node::Error {
                msg: "Unable to parse identifier",
                token,
            }),
        }
    }

    fn expression(&mut self, lhs: ast::Ref, min_precedence: i32) -> ast::Ref {
        let mut last = lhs;
        let mut current = self.cursor.current();

        while precedence(current) >= min_precedence {
            let op = self.cursor.current();
            self.cursor.advance();

            let mut rhs = self.parse_value();

            current = self.cursor.current();

            while precedence(current) > precedence(op) {
                rhs = self.expression(
                    rhs,
                    precedence(op)
                        + if precedence(current) > precedence(op) {
                            1
                        } else {
                            0
                        },
                );
            }

            last = self.alloc(ast::Node::Binary { lhs: last, rhs, op });
        }

        last
    }

    fn parse_expr(&mut self) -> ast::Ref {
        let lhs = self.parse_value();
        self.expression(lhs, 0)
    }

    fn parse_comptime(&mut self) -> ast::Ref {
        self.cursor.advance();
        let expr = self.parse_expr();
        self.alloc(ast::Node::Comptime(expr))
    }

    fn parse_value(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        let value = match token.ty {
            TokenType::String => self.advance_alloc(ast::Node::String(token.text)),
            TokenType::Float => self.advance_alloc(ast::Node::Float(token.text)),
            TokenType::Int => self.advance_alloc(ast::Node::Int(token.text)),
            TokenType::Ident => self.advance_alloc(ast::Node::Ident(token)),

            TokenType::True => self.advance_alloc(ast::Node::Bool(true)),
            TokenType::False => self.advance_alloc(ast::Node::Bool(false)),

            TokenType::If => self.parse_if(),

            TokenType::Comptime => self.parse_comptime(),

            TokenType::LParen => {
                self.cursor.advance();
                let expr = self.parse_expr();

                if self.cursor.advance_if(TokenType::RParen) {
                    expr
                } else {
                    self.advance_alloc(ast::Node::Error {
                        msg: "Parenthesis requires closing brace",
                        token,
                    })
                }
            }

            _ => {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Unable to parse value",
                    token,
                });
            }
        };

        value
    }

    fn parse_enum_type(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::Enum) {
                break 'outer;
            }

            if !self.cursor.advance_if(TokenType::LBrace) {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Enum decleration requires a body",
                    token,
                });
            }

            let mut variants = Vec::<ast::Ref>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Enum body requires a closing brace",
                        token,
                    });
                }

                let ident = self.parse_ident();

                let ty = if self.cursor.advance_if(TokenType::Colon) {
                    Some(self.parse_type_expr())
                } else {
                    None
                };

                let variant = self.alloc(ast::Node::EnumVariant { ident, ty });
                variants.push(variant);

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Enum variant must end with a semicolon",
                        token,
                    });
                }
            }

            return self.alloc(ast::Node::EnumType { variants });
        }

        self.advance_alloc(ast::Node::Error {
            msg: "Failed to parse enum",
            token,
        })
    }

    fn parse_struct_type(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::Struct) {
                break 'outer;
            }

            if !self.cursor.advance_if(TokenType::LBrace) {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Struct decleration requires a body",
                    token,
                });
            }

            let mut fields = Vec::<ast::Ref>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Struct body requires a closing brace",
                        token,
                    });
                }

                let ident = self.parse_ident();

                if !self.cursor.advance_if(TokenType::Colon) {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Struct field requires a type",
                        token,
                    });
                }

                let ty = self.parse_type_expr();

                let field = self.alloc(ast::Node::StructField { ident, ty });
                fields.push(field);

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Struct field must end with a semicolon",
                        token,
                    });
                }
            }

            return self.alloc(ast::Node::StructType { fields });
        }

        self.advance_alloc(ast::Node::Error {
            msg: "Failed to parse struct",
            token,
        })
    }

    fn parse_type_expr(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        let value = match token.ty {
            TokenType::Struct => self.parse_struct_type(),
            TokenType::Enum => self.parse_enum_type(),
            TokenType::Ident => self.parse_ident(),
            _ => {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Invalid type",
                    token,
                });
            }
        };

        if self.cursor.advance_if(TokenType::LParen) {
            let mut args = Vec::<ast::Ref>::new();

            loop {
                if self.cursor.matches(TokenType::RParen) {
                    break;
                }

                if self.cursor.eof() {
                    return self.alloc(ast::Node::Error { msg: "", token });
                }

                let arg = self.parse_type_expr();
                args.push(arg);

                if !self.cursor.advance_if(TokenType::Comma) {
                    break;
                }
            }

            if !self.cursor.advance_if(TokenType::RParen) {
                return self.alloc(ast::Node::Error {
                    msg: "Type constructor requires a closing parenthesis",
                    token,
                });
            }

            return self.alloc(ast::Node::TypeConstructor { ty: value, args });
        }

        value
    }

    fn parse_const(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Const) {
            let ident = self.parse_ident();

            let ty = if self.cursor.advance_if(TokenType::Colon) {
                Some(self.parse_type_expr())
            } else {
                None
            };

            if self.cursor.advance_if(TokenType::Equal) {
                let value = self.parse_expr();

                return if self.cursor.advance_if(TokenType::Semicolon) {
                    self.alloc(ast::Node::ConstDecl { ident, ty, value })
                } else {
                    self.advance_alloc(ast::Node::Error {
                        msg: "Expected semicolon after const statement",
                        token,
                    })
                };
            }
        }

        self.advance_alloc(ast::Node::Error {
            msg: "Failed to parse const statement",
            token,
        })
    }

    fn parse_var(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Var) {
            let ident = self.parse_ident();

            let ty = if self.cursor.advance_if(TokenType::Colon) {
                Some(self.parse_type_expr())
            } else {
                None
            };

            if self.cursor.advance_if(TokenType::Equal) {
                let value = self.parse_expr();

                return if self.cursor.advance_if(TokenType::Semicolon) {
                    self.alloc(ast::Node::VarDecl { ident, ty, value })
                } else {
                    self.advance_alloc(ast::Node::Error {
                        msg: "Expected semicolon after var statement",
                        token,
                    })
                };
            }
        }

        self.advance_alloc(ast::Node::Error {
            msg: "Failed to parse var statement",
            token,
        })
    }

    fn parse_type_arg(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Ident => self.parse_ident(),
            _ => self.advance_alloc(ast::Node::Error {
                msg: "Invalid generic",
                token,
            }),
        }
    }

    fn parse_type_stmt(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Type) {
            let ident = self.parse_ident();

            let mut args = Vec::<ast::Ref>::new();

            if self.cursor.advance_if(TokenType::LParen) {
                while !self.cursor.advance_if(TokenType::RParen) {
                    if self.cursor.eof() {
                        return self.advance_alloc(ast::Node::Error {
                            msg: "Type paramaters require a closing brace",
                            token,
                        });
                    }
                    let arg = self.parse_type_arg();
                    args.push(arg);
                }
            }

            if !self.cursor.advance_if(TokenType::Equal) {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Type definition requires an actual tyoe",
                    token,
                });
            }

            let body = self.parse_type_expr();

            return if self.cursor.advance_if(TokenType::Semicolon) {
                self.alloc(ast::Node::Type { ident, args, body })
            } else {
                self.advance_alloc(ast::Node::Error {
                    msg: "Expected semicolon after type statement",
                    token,
                })
            };
        }

        self.advance_alloc(ast::Node::Error {
            msg: "failed to parse type statement",
            token,
        })
    }

    fn parse_interface(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::Interface) {
                break 'outer;
            }

            let ident = self.parse_ident();

            if !self.cursor.advance_if(TokenType::LBrace) {
                return self.alloc(ast::Node::Error {
                    msg: "Interface requires body",
                    token,
                });
            }

            let mut fields = Vec::<ast::Ref>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return self.alloc(ast::Node::Error {
                        msg: "Interface body requires closing brace",
                        token,
                    });
                }

                let ident = self.parse_ident();

                if !self.cursor.advance_if(TokenType::Colon) {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Struct field requires a type",
                        token,
                    });
                }

                let ty = self.parse_type_expr();

                let field = self.alloc(ast::Node::StructField { ident, ty });
                fields.push(field);

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Struct field must end with a semicolon",
                        token,
                    });
                }
            }

            return self.alloc(ast::Node::Interface { fields });
        }

        self.advance_alloc(ast::Node::Error {
            msg: "failed to parse interface",
            token,
        })
    }

    fn parse_stmt(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => self.parse_const(),
            TokenType::Var => self.parse_var(),
            TokenType::Return => {
                self.cursor.advance();
                let expr = self.parse_expr();
                self.alloc(ast::Node::Return(expr))
            }
            TokenType::Defer => {
                self.cursor.advance();
                let expr = self.parse_expr();
                self.alloc(ast::Node::Defer(expr))
            }
            TokenType::Import => self.parse_import(),
            TokenType::Type => self.parse_type_stmt(),
            TokenType::Interface => self.parse_interface(),
            TokenType::Fn => self.parse_fn_stmt(),
            _ if self.cursor.matches(TokenType::Ident)
                && self.cursor.matches_next(TokenType::Equal) =>
            {
                let ident = self.parse_ident();
                self.cursor.advance();

                let value = self.parse_expr();

                self.alloc(ast::Node::Assignment { ident, value })
            }
            _ => {
                let expr = self.parse_expr();

                if self.cursor.advance_if(TokenType::Semicolon) {
                    expr
                } else {
                    self.alloc(ast::Node::ImplicitReturn(expr))
                }
            }
        }
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
            list.push(stmt);
        }

        if !self.cursor.advance_if(TokenType::RBrace) {
            return self.advance_alloc(ast::Node::Error {
                msg: "Expect closing brace at end of statement",
                token,
            });
        }

        self.alloc(ast::Node::Scope(list))
    }

    fn parse_fn_stmt(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Fn) {
            let ident = self.parse_ident();

            if !self.cursor.advance_if(TokenType::LParen) {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Function definition requires arguments list",
                    token,
                });
            }

            let mut params = Vec::<ast::Ref>::new();

            loop {
               if self.cursor.matches(TokenType::RParen) {
                    break;
                }
                if self.cursor.eof() {
                    return self.alloc(ast::Node::Error {
                        msg: "Function paramater list requires closing brace",
                        token,
                    });
                }

                let ident = self.parse_ident();

                if !self.cursor.advance_if(TokenType::Colon) {
                    return self.advance_alloc(ast::Node::Error {
                        msg: "Argument needs a type with ':'",
                        token,
                    });
                }

                let ty = self.parse_type_expr();
                params.push(self.alloc(ast::Node::Paramater { ident, ty }));

                if !self.cursor.advance_if(TokenType::Comma) {
                    break;
                }
            }

            if !self.cursor.advance_if(TokenType::RParen) {
                return self.alloc(ast::Node::Error {
                    msg: "Fn paramaters requires a closing parenthesis",
                    token,
                });
            }

            let ret = self.parse_type_expr();

            if !self.cursor.matches(TokenType::LBrace) {
                return self.advance_alloc(ast::Node::Error {
                    msg: "Function decleration requires a code block",
                    token,
                });
            }

            let block = self.parse_scope();

            let params = self.alloc(ast::Node::ParameterList(params));
            return self.alloc(ast::Node::FnDecl {
                ident,
                params,
                ret,
                block,
            });
        }

        self.advance_alloc(ast::Node::Error {
            msg: "Failed to parse fn statement",
            token,
        })
    }

    fn parse_import(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Import) {
            let current = self.cursor.current();
            return self.advance_alloc(match current.ty {
                TokenType::String => ast::Node::Import(current.text),
                _ => ast::Node::Error {
                    msg: "Import expects a path",
                    token,
                },
            });
        }

        self.advance_alloc(ast::Node::Error {
            msg: "Unable to parse import statement",
            token,
        })
    }

    fn parse_if(&mut self) -> ast::Ref {
        let token = self.cursor.current();

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
            token,
        });
    }

    fn parse_toplevel_stmt(&mut self) -> ast::Ref {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => self.parse_const(),
            TokenType::Var => self.parse_var(),
            TokenType::Comptime => self.parse_comptime(),
            TokenType::Type => self.parse_type_stmt(),
            TokenType::Interface => self.parse_interface(),
            TokenType::Fn => self.parse_fn_stmt(),
            _ => self.advance_alloc(ast::Node::Error {
                msg: "Invalid toplevel statement",
                token,
            }),
        }
    }

    fn parse_toplevel(&mut self) -> ast::Ref {
        let mut list: Vec<ast::Ref> = vec![];

        while !self.cursor.eof() {
            list.push(self.parse_stmt());
        }

        self.alloc(ast::Node::TopLevelScope(list))
    }

    pub fn run(mut self) -> AST<'a> {
        let toplevel = self.parse_toplevel();
        self.ast.root = Some(toplevel);

        self.ast
    }
}
