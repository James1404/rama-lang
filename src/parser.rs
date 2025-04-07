use std::result;

use log::error;

use crate::{
    ast::{self, Node, Param, Ref, AST},
    lexer::{precedence, Token, TokenType},
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

#[derive(Debug, Clone)]
pub struct ParserError<'a> {
    pub msg: String,
    pub token: Token<'a>,
}

pub type Result<'a, T> = result::Result<T, ParserError<'a>>;

#[derive(Clone)]
pub struct Parser<'tokens: 'parser, 'parser> {
    ast: AST<'parser>,
    cursor: Cursor<'tokens>,
}

impl<'tokens, 'parser> Parser<'tokens, 'parser> {
    pub fn new(tokens: &'tokens Vec<Token<'tokens>>) -> Self {
        Self {
            cursor: Cursor::new(tokens),
            ast: AST::new(),
        }
    }

    fn alloc(&mut self, node: Node<'tokens>) -> Ref {
        self.ast.alloc(node)
    }

    fn advance_alloc(&mut self, node: Node<'tokens>) -> Ref {
        self.cursor.advance();
        self.alloc(node)
    }

    fn parse_ident(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Ident => Ok(self.advance_alloc(Node::Ident(token))),
            _ => Err(ParserError {
                msg: "Unable to parse identifier".to_owned(),
                token,
            }),
        }
    }

    fn expression(&mut self, lhs: Ref, min_precedence: i32) -> Result<'tokens, Ref> {
        let mut last = lhs;
        let mut current = self.cursor.current();

        while precedence(current) >= min_precedence {
            let op = self.cursor.current();
            self.cursor.advance();

            let mut rhs = self.parse_value()?;

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
                )?;
            }

            last = self.alloc(Node::Binary { lhs: last, rhs, op });
        }

        Ok(last)
    }

    fn parse_expr(&mut self) -> Result<'tokens, Ref> {
        let lhs = self.parse_value()?;
        self.expression(lhs, 0)
    }

    fn parse_comptime(&mut self) -> Result<'tokens, Ref> {
        self.cursor.advance();
        let expr = self.parse_expr()?;
        Ok(self.alloc(Node::Comptime(expr)))
    }

    fn parse_value(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        let value = match token.ty {
            TokenType::String => {
                self.advance_alloc(Node::Literal(ast::Literal::String(token.text)))
            }
            TokenType::Float => self.advance_alloc(Node::Literal(ast::Literal::Float(token.text))),
            TokenType::Int => self.advance_alloc(Node::Literal(ast::Literal::Int(token.text))),
            TokenType::Ident => self.advance_alloc(Node::Ident(token)),

            TokenType::True => self.advance_alloc(Node::Literal(ast::Literal::Bool(true))),
            TokenType::False => self.advance_alloc(Node::Literal(ast::Literal::Bool(false))),

            TokenType::If => self.parse_if()?,

            TokenType::Comptime => self.parse_comptime()?,

            TokenType::LParen => {
                self.cursor.advance();
                let expr = self.parse_expr()?;

                if self.cursor.advance_if(TokenType::RParen) {
                    expr
                } else {
                    return Err(ParserError {
                        msg: "Parenthesis requires closing brace".to_owned(),
                        token,
                    });
                }
            }

            _ => {
                return Err(ParserError {
                    msg: "Unable to parse value".to_owned(),
                    token,
                });
            }
        };

        match self.cursor.current().ty {
            TokenType::As => {
                self.cursor.advance();
                let ty = self.parse_type_expr()?;

                Ok(self.alloc(Node::Cast { value, ty }))
            }
            _ => Ok(value),
        }
    }

    fn parse_enum_type(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::Enum) {
                break 'outer;
            }

            if !self.cursor.advance_if(TokenType::LBrace) {
                return Err(ParserError {
                    msg: "Enum decleration requires a body".to_owned(),
                    token,
                });
            }

            let mut variants = Vec::<ast::EnumVariant>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return Err(ParserError {
                        msg: "Enum body requires a closing brace".to_owned(),
                        token,
                    });
                }

                let ident = self.parse_ident()?;

                let ty = if self.cursor.advance_if(TokenType::Colon) {
                    Some(self.parse_type_expr()?)
                } else {
                    None
                };

                variants.push(ast::EnumVariant { ident, ty });

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return Err(ParserError {
                        msg: "Enum variant must end with a semicolon".to_owned(),
                        token,
                    });
                }
            }

            return Ok(self.alloc(Node::EnumType(variants)));
        }

        Err(ParserError {
            msg: "Failed to parse enum".to_owned(),
            token,
        })
    }

    fn parse_struct_type(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::Struct) {
                break 'outer;
            }

            if !self.cursor.advance_if(TokenType::LBrace) {
                return Err(ParserError {
                    msg: "Struct decleration requires a body".to_owned(),
                    token,
                });
            }

            let mut fields = Vec::<ast::StructField>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return Err(ParserError {
                        msg: "Struct body requires a closing brace".to_owned(),
                        token,
                    });
                }

                let ident = self.parse_ident()?;

                if !self.cursor.advance_if(TokenType::Colon) {
                    return Err(ParserError {
                        msg: "Struct field requires a type".to_owned(),
                        token,
                    });
                }

                let ty = self.parse_type_expr()?;

                fields.push(ast::StructField { ident, ty });

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return Err(ParserError {
                        msg: "Struct field must end with a semicolon".to_owned(),
                        token,
                    });
                }
            }

            return Ok(self.alloc(Node::StructType(fields)));
        }

        Err(ParserError {
            msg: "Failed to parse struct".to_owned(),
            token,
        })
    }

    fn parse_type_expr(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        let value = match token.ty {
            TokenType::Asterix => {
                self.cursor.advance();

                let inner = self.parse_type_expr()?;
                return Ok(self.alloc(Node::PtrType(inner)));
            }
            TokenType::Struct => self.parse_struct_type()?,
            TokenType::Enum => self.parse_enum_type()?,
            TokenType::Ident => self.parse_ident()?,
            TokenType::LBracket => {
                self.cursor.advance();

                let inner = self.parse_type_expr()?;

                let node = if self.cursor.advance_if(TokenType::Semicolon) {
                    let token = self.cursor.current();
                    let len = match token.ty {
                        TokenType::Int => {
                            self.cursor.advance();
                            token.text.parse::<usize>().unwrap()
                        }
                        _ => todo!("Add error message for invalid array size"),
                    };

                    Node::ArrayType(inner, len)
                } else {
                    Node::SliceType(inner)
                };

                if !self.cursor.advance_if(TokenType::RBracket) {
                    todo!("Error")
                }

                self.alloc(node)
            }
            _ => {
                return Err(ParserError {
                    msg: "Invalid type".to_owned(),
                    token,
                });
            }
        };

        if self.cursor.advance_if(TokenType::LParen) {
            let mut args = Vec::<Ref>::new();

            loop {
                if self.cursor.matches(TokenType::RParen) {
                    break;
                }

                if self.cursor.eof() {
                    return Err(ParserError { msg: "".to_owned(), token });
                }

                let arg = self.parse_type_expr()?;
                args.push(arg);

                if !self.cursor.advance_if(TokenType::Comma) {
                    break;
                }
            }

            if !self.cursor.advance_if(TokenType::RParen) {
                return Err(ParserError {
                    msg: "Type constructor requires a closing parenthesis".to_owned(),
                    token,
                });
            }

            return Ok(self.alloc(Node::TypeConstructor { ty: value, args }));
        }

        Ok(value)
    }

    fn parse_const(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Const) {
            let ident = self.parse_ident()?;

            let ty = if self.cursor.advance_if(TokenType::Colon) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            if self.cursor.advance_if(TokenType::Equal) {
                let value = self.parse_expr()?;

                return if self.cursor.advance_if(TokenType::Semicolon) {
                    Ok(self.alloc(Node::ConstDecl { ident, ty, value }))
                } else {
                    Err(ParserError {
                        msg: "Expected semicolon after const statement".to_owned(),
                        token,
                    })
                };
            }
        }

        Err(ParserError {
            msg: "Failed to parse const statement".to_owned(),
            token,
        })
    }

    fn parse_var(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Var) {
            let ident = self.parse_ident()?;

            let ty = if self.cursor.advance_if(TokenType::Colon) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            if self.cursor.advance_if(TokenType::Equal) {
                let value = self.parse_expr()?;

                return if self.cursor.advance_if(TokenType::Semicolon) {
                    Ok(self.alloc(Node::VarDecl { ident, ty, value }))
                } else {
                    Err(ParserError {
                        msg: "Expected semicolon after var statement".to_owned(),
                        token,
                    })
                };
            }
        }

        Err(ParserError {
            msg: "Failed to parse var statement".to_owned(),
            token,
        })
    }

    fn parse_type_arg(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Ident => self.parse_ident(),
            _ => Err(ParserError {
                msg: "Invalid generic".to_owned(),
                token,
            }),
        }
    }

    fn parse_type_stmt(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Type) {
            let ident = self.parse_ident()?;

            let mut args = Vec::<Ref>::new();

            if self.cursor.advance_if(TokenType::LParen) {
                while !self.cursor.advance_if(TokenType::RParen) {
                    if self.cursor.eof() {
                        return Err(ParserError {
                            msg: "Type paramaters require a closing brace".to_owned(), 
                            token,
                        });
                    }
                    let arg = self.parse_type_arg()?;
                    args.push(arg);
                }
            }

            if !self.cursor.advance_if(TokenType::Equal) {
                return Err(ParserError {
                    msg: "Type definition requires an actual tyoe".to_owned(),
                    token,
                });
            }

            let body = self.parse_type_expr()?;

            return if self.cursor.advance_if(TokenType::Semicolon) {
                Ok(self.alloc(Node::Type { ident, args, body }))
            } else {
                Err(ParserError {
                    msg: "Expected semicolon after type statement".to_owned(),
                    token,
                })
            };
        }

        Err(ParserError {
            msg: "failed to parse type statement".to_owned(),
            token,
        })
    }

    fn parse_interface(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::Interface) {
                break 'outer;
            }

            let ident = self.parse_ident()?;

            if !self.cursor.advance_if(TokenType::LBrace) {
                return Err(ParserError {
                    msg: "Interface requires body".to_owned(),
                    token,
                });
            }

            let mut fields = Vec::<ast::StructField>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return Err(ParserError {
                        msg: "Interface body requires closing brace".to_owned(),
                        token,
                    });
                }

                let ident = self.parse_ident()?;

                if !self.cursor.advance_if(TokenType::Colon) {
                    return Err(ParserError {
                        msg: "Struct field requires a type".to_owned(),
                        token,
                    });
                }

                let ty = self.parse_type_expr()?;

                fields.push(ast::StructField { ident, ty });

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return Err(ParserError {
                        msg: "Struct field must end with a semicolon".to_owned(),
                        token,
                    });
                }
            }

            return Ok(self.alloc(Node::Interface { ident, fields }));
        }

        Err(ParserError {
            msg: "failed to parse interface".to_owned(),
            token,
        })
    }

    fn parse_stmt(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => self.parse_const(),
            TokenType::Var => self.parse_var(),
            TokenType::Return => {
                self.cursor.advance();
                let expr = self.parse_expr()?;
                Ok(self.alloc(Node::Return(expr)))
            }
            TokenType::Defer => {
                self.cursor.advance();
                let expr = self.parse_expr()?;
                Ok(self.alloc(Node::Defer(expr)))
            }
            TokenType::Import => self.parse_import(),
            TokenType::Type => self.parse_type_stmt(),
            TokenType::Interface => self.parse_interface(),
            TokenType::Fn => self.parse_fn_stmt(),
            _ if self.cursor.matches(TokenType::Ident)
                && self.cursor.matches_next(TokenType::Equal) =>
            {
                let ident = self.parse_ident()?;
                self.cursor.advance();

                let value = self.parse_expr()?;

                Ok(self.alloc(Node::Assignment { ident, value }))
            }
            _ => {
                let expr = self.parse_expr()?;

                if self.cursor.advance_if(TokenType::Semicolon) {
                    Ok(expr)
                } else {
                    Ok(self.alloc(Node::ImplicitReturn(expr)))
                }
            }
        }
    }

    fn parse_scope(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if !self.cursor.advance_if(TokenType::LBrace) {
            return Err(ParserError {
                msg: "Block needs opening brace".to_owned(),
                token,
            });
        }

        let mut list = Vec::<Ref>::new();

        while !self.cursor.matches(TokenType::RBrace) {
            let stmt = self.parse_stmt()?;
            list.push(stmt);
        }

        if !self.cursor.advance_if(TokenType::RBrace) {
            return Err(ParserError {
                msg: "Expect closing brace at end of statement".to_owned(),
                token,
            });
        }

        Ok(self.alloc(Node::Scope(list)))
    }

    fn parse_fn_stmt(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Fn) {
            let ident = self.parse_ident()?;

            if !self.cursor.advance_if(TokenType::LParen) {
                return Err(ParserError {
                    msg: "Function definition requires arguments list".to_owned(),
                    token,
                });
            }

            let mut params = Vec::<Param>::new();

            loop {
                if self.cursor.matches(TokenType::RParen) {
                    break;
                }
                if self.cursor.eof() {
                    return Err(ParserError {
                        msg: "Function paramater list requires closing brace".to_owned(),
                        token,
                    });
                }

                let ident = self.parse_ident()?;

                if !self.cursor.advance_if(TokenType::Colon) {
                    return Err(ParserError {
                        msg: "Argument needs a type with ':'".to_owned(),
                        token,
                    });
                }

                let ty = self.parse_type_expr()?;
                params.push(Param { ident, ty });

                if !self.cursor.advance_if(TokenType::Comma) {
                    break;
                }
            }

            if !self.cursor.advance_if(TokenType::RParen) {
                return Err(ParserError {
                    msg: "Fn paramaters requires a closing parenthesis".to_owned(),
                    token,
                });
            }

            let ret = self.parse_type_expr()?;

            if !self.cursor.matches(TokenType::LBrace) {
                return Err(ParserError {
                    msg: "Function decleration requires a code block".to_owned(),
                    token,
                });
            }

            let block = self.parse_scope()?;

            return Ok(self.alloc(Node::FnDecl {
                ident,
                params,
                ret,
                block,
            }));
        }

        Err(ParserError {
            msg: "Failed to parse fn statement".to_owned(),
            token,
        })
    }

    fn parse_import(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Import) {
            let current = self.cursor.current();
            return match current.ty {
                TokenType::String => Ok(self.advance_alloc(Node::Import(current.text))),
                _ => Err(ParserError {
                    msg: "Import expects a path".to_owned(),
                    token,
                }),
            };
        }

        Err(ParserError {
            msg: "Unable to parse import statement".to_owned(),
            token,
        })
    }

    fn parse_if(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::If) {
                break 'outer;
            }

            let cond = self.parse_expr()?;
            let t = self.parse_scope()?;

            if !self.cursor.advance_if(TokenType::Else) {
                break 'outer;
            }

            let f = self.parse_scope()?;

            return Ok(self.alloc(Node::If { cond, t, f }));
        }

        return Err(ParserError {
            msg: "Failed to parse if statement".to_owned(),
            token,
        });
    }

    fn parse_toplevel_stmt(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => self.parse_const(),
            TokenType::Var => self.parse_var(),
            TokenType::Import => self.parse_import(),
            TokenType::Type => self.parse_type_stmt(),
            TokenType::Interface => self.parse_interface(),
            TokenType::Fn => self.parse_fn_stmt(),
            _ => Err(ParserError {
                msg: "Invalid toplevel statement".to_owned(),
                token,
            }),
        }
    }

    fn parse_toplevel(&mut self) -> Result<'tokens, Ref> {
        let mut list: Vec<Ref> = vec![];

        while !self.cursor.eof() {
            list.push(self.parse_toplevel_stmt()?);
        }

        Ok(self.alloc(Node::TopLevelScope(list)))
    }

    pub fn run(mut self) -> Result<'tokens, AST<'parser>> {
        let toplevel = self.parse_toplevel()?;
        self.ast.root = Some(toplevel);

        Ok(self.ast)
    }
}
