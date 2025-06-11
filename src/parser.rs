use std::result;

use itertools::Itertools;

use crate::{
    ast::{self, AST, Literal, LiteralStructField, Node, Param, Ref},
    lexer::{Token, TokenType, precedence},
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
        self.tokens[self.position]
    }

    fn matches(self, expected: TokenType) -> bool {
        if self.eof() {
            return false;
        }

        self.current().ty == expected
    }

    fn matches_many(self, expected: &[TokenType]) -> bool {
        if self.eof() {
            return false;
        }

        expected.contains(&self.current().ty)
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

    fn advance_if_many(&mut self, expected: &[TokenType]) -> bool {
        if self.eof() {
            return false;
        }

        match self.tokens.get(self.position) {
            Some(tok) => {
                if expected.contains(&tok.ty) {
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
pub enum ParserError<'a> {
    Many(Vec<ParserError<'a>>),
    Msg { msg: String, token: Token<'a> },
}

impl<'a> ParserError<'a> {
    pub fn error(&self) {
        match self {
            ParserError::Many(errs) => errs.iter().for_each(|err| err.error()),
            ParserError::Msg { msg, token } => {
                eprintln!("[{}; {}] {}", token.pos.line, token.pos.offset, msg);
            }
        }
    }
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
            TokenType::Ident => {
                let ident = self.advance_alloc(Node::Ident(token));

                if self.cursor.advance_if(TokenType::Dot) {
                    let field = self.parse_ident()?;

                    Ok(self.alloc(Node::FieldAccess(ident, field)))
                } else {
                    Ok(ident)
                }
            }
            _ => Err(ParserError::Msg {
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

            last = self.alloc(Node::Binary {
                lhs: last,
                rhs,
                op: op.ty.into(),
            });
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

    fn parse_string(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();
        match token.ty {
            TokenType::String => {
                Ok(self.advance_alloc(Node::Literal(ast::Literal::String(token.text))))
            }
            _ => Err(ParserError::Msg {
                msg: "Unable to parse string".to_owned(),
                token,
            }),
        }
    }

    fn parse_value(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        let value = match token.ty {
            TokenType::String => self.parse_string(),
            TokenType::Float => {
                Ok(self.advance_alloc(Node::Literal(ast::Literal::Float(token.text))))
            }
            TokenType::Int => Ok(self.advance_alloc(Node::Literal(ast::Literal::Int(token.text)))),
            TokenType::Ident => self.parse_ident(),
            TokenType::True => Ok(self.advance_alloc(Node::Literal(ast::Literal::Bool(true)))),
            TokenType::False => Ok(self.advance_alloc(Node::Literal(ast::Literal::Bool(false)))),

            TokenType::LParen => {
                self.cursor.advance();
                let expr = self.parse_expr()?;

                if self.cursor.advance_if(TokenType::RParen) {
                    Ok(expr)
                } else {
                    Err(ParserError::Msg {
                        msg: "Parenthesis requires closing brace".to_owned(),
                        token,
                    })
                }
            }

            TokenType::StructConstructor => {
                self.cursor.advance();

                let mut fields = Vec::<LiteralStructField>::new();

                while !self.cursor.matches(TokenType::RBrace) {
                    if self.cursor.eof() {
                        return Err(ParserError::Msg {
                            msg: "Struct initialization requires a closing brace".to_owned(),
                            token,
                        });
                    }

                    let ident = self.parse_ident()?;

                    if !self.cursor.advance_if(TokenType::Colon) {
                        return Err(ParserError::Msg {
                            msg: "Struct field requires a type".to_owned(),
                            token,
                        });
                    }

                    let value = self.parse_expr()?;

                    fields.push(LiteralStructField { ident, value });

                    if !self.cursor.advance_if(TokenType::Comma) {
                        break;
                    }
                }

                if self.cursor.advance_if(TokenType::RBrace) {
                    // TODO: Allow alternative "Vec2 { x: 5, y: 16 }"
                    // syntax as opposed to .{ x: 5, y: 16 } inference
                    Ok(self.alloc(Node::Literal(Literal::Struct { fields })))
                } else {
                    Err(ParserError::Msg {
                        msg: "Struct initializer requires a closing brace".to_owned(),
                        token,
                    })
                }
            }

            _ => {
                return Err(ParserError::Msg {
                    msg: "Unable to parse value".to_owned(),
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
                    return Err(ParserError::Msg {
                        msg: "".to_owned(),
                        token,
                    });
                }

                let arg = self.parse_expr()?;
                args.push(arg);

                if !self.cursor.advance_if(TokenType::Comma) {
                    break;
                }
            }

            if !self.cursor.advance_if(TokenType::RParen) {
                return Err(ParserError::Msg {
                    msg: "Type constructor requires a closing parenthesis".to_owned(),
                    token,
                });
            }

            return Ok(self.alloc(Node::FnCall { func: value?, args }));
        }

        match self.cursor.current().ty {
            TokenType::As => {
                self.cursor.advance();
                let ty = self.parse_type_expr()?;

                Ok(self.alloc(Node::Cast { value: value?, ty }))
            }
            _ => value,
        }
    }

    fn parse_enum_type(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        'outer: {
            if !self.cursor.advance_if(TokenType::Enum) {
                break 'outer;
            }

            if !self.cursor.advance_if(TokenType::LBrace) {
                return Err(ParserError::Msg {
                    msg: "Enum decleration requires a body".to_owned(),
                    token,
                });
            }

            let mut variants = Vec::<ast::EnumVariant>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return Err(ParserError::Msg {
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
                    return Err(ParserError::Msg {
                        msg: "Enum variant must end with a semicolon".to_owned(),
                        token,
                    });
                }
            }

            return Ok(self.alloc(Node::EnumType(variants)));
        }

        Err(ParserError::Msg {
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
                return Err(ParserError::Msg {
                    msg: "Struct decleration requires a body".to_owned(),
                    token,
                });
            }

            let mut fields = Vec::<ast::StructField>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return Err(ParserError::Msg {
                        msg: "Struct body requires a closing brace".to_owned(),
                        token,
                    });
                }

                let ident = self.parse_ident()?;

                if !self.cursor.advance_if(TokenType::Colon) {
                    return Err(ParserError::Msg {
                        msg: "Struct field requires a type".to_owned(),
                        token,
                    });
                }

                let ty = self.parse_type_expr()?;

                fields.push(ast::StructField { ident, ty });

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return Err(ParserError::Msg {
                        msg: "Struct field must end with a semicolon".to_owned(),
                        token,
                    });
                }
            }

            return Ok(self.alloc(Node::StructType(fields)));
        }

        Err(ParserError::Msg {
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
                return Err(ParserError::Msg {
                    msg: "Invalid type".to_owned(),
                    token,
                });
            }
        };

        if self.cursor.advance_if(TokenType::LAngle) {
            let mut args = Vec::<Ref>::new();

            loop {
                if self.cursor.matches(TokenType::RAngle) {
                    break;
                }

                if self.cursor.eof() {
                    return Err(ParserError::Msg {
                        msg: "".to_owned(),
                        token,
                    });
                }

                let arg = self.parse_type_expr()?;
                args.push(arg);

                if !self.cursor.advance_if(TokenType::Comma) {
                    break;
                }
            }

            if !self.cursor.advance_if(TokenType::RAngle) {
                return Err(ParserError::Msg {
                    msg: "Type constructor requires a closing right angle".to_owned(),
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
                    Err(ParserError::Msg {
                        msg: "Expected semicolon after const statement".to_owned(),
                        token,
                    })
                };
            }
        }

        Err(ParserError::Msg {
            msg: "Failed to parse const statement".to_owned(),
            token,
        })
    }

    fn parse_let(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Let) {
            let ident = self.parse_ident()?;

            let ty = if self.cursor.advance_if(TokenType::Colon) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            if self.cursor.advance_if(TokenType::Equal) {
                let value = self.parse_expr()?;

                return if self.cursor.advance_if(TokenType::Semicolon) {
                    Ok(self.alloc(Node::LetDecl { ident, ty, value }))
                } else {
                    Err(ParserError::Msg {
                        msg: "Expected semicolon after let statement".to_owned(),
                        token,
                    })
                };
            }
        }

        Err(ParserError::Msg {
            msg: "Failed to parse let statement".to_owned(),
            token,
        })
    }

    fn parse_type_arg(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Ident => self.parse_ident(),
            _ => Err(ParserError::Msg {
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

            if self.cursor.advance_if(TokenType::LAngle) {
                while !self.cursor.advance_if(TokenType::RAngle) {
                    if self.cursor.eof() {
                        return Err(ParserError::Msg {
                            msg: "Type paramaters require a closing brace".to_owned(),
                            token,
                        });
                    }
                    let arg = self.parse_type_arg()?;
                    args.push(arg);
                }
            }

            if !self.cursor.advance_if(TokenType::Equal) {
                return Err(ParserError::Msg {
                    msg: "Type definition requires an actual tyoe".to_owned(),
                    token,
                });
            }

            let body = self.parse_type_expr()?;

            return if self.cursor.advance_if(TokenType::Semicolon) {
                Ok(self.alloc(Node::Type { ident, args, body }))
            } else {
                Err(ParserError::Msg {
                    msg: "Expected semicolon after type statement".to_owned(),
                    token,
                })
            };
        }

        Err(ParserError::Msg {
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
                return Err(ParserError::Msg {
                    msg: "Interface requires body".to_owned(),
                    token,
                });
            }

            let mut fields = Vec::<ast::StructField>::new();

            while !self.cursor.advance_if(TokenType::RBrace) {
                if self.cursor.eof() {
                    return Err(ParserError::Msg {
                        msg: "Interface body requires closing brace".to_owned(),
                        token,
                    });
                }

                let ident = self.parse_ident()?;

                if !self.cursor.advance_if(TokenType::Colon) {
                    return Err(ParserError::Msg {
                        msg: "Struct field requires a type".to_owned(),
                        token,
                    });
                }

                let ty = self.parse_type_expr()?;

                fields.push(ast::StructField { ident, ty });

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return Err(ParserError::Msg {
                        msg: "Struct field must end with a semicolon".to_owned(),
                        token,
                    });
                }
            }

            return Ok(self.alloc(Node::Interface { ident, fields }));
        }

        Err(ParserError::Msg {
            msg: "failed to parse interface".to_owned(),
            token,
        })
    }

    fn parse_stmt(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => self.parse_const(),
            TokenType::Let => self.parse_let(),
            TokenType::Return => {
                self.cursor.advance();

                if self.cursor.advance_if(TokenType::Semicolon) {
                    Ok(self.alloc(Node::ReturnNone))
                } else {
                    let expr = self.parse_expr()?;
                    if self.cursor.advance_if(TokenType::Semicolon) {
                        Ok(self.alloc(Node::Return(expr)))
                    } else {
                        Err(ParserError::Msg {
                            msg: "Expected semicolon after return statement".to_owned(),
                            token,
                        })
                    }
                }
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

            TokenType::If => self.parse_if(),
            TokenType::Begin => self.parse_block(),

            _ => {
                let expr = self.parse_expr()?;

                let expr = if self.cursor.advance_if(TokenType::Equal) {
                    let value = self.parse_expr()?;
                    self.alloc(Node::Assignment { ident: expr, value })
                } else {
                    expr
                };

                if !self.cursor.advance_if(TokenType::Semicolon) {
                    return Err(ParserError::Msg {
                        msg: "Statement must end with a semicolon".to_owned(),
                        token,
                    });
                }

                Ok(expr)
            }
        }
    }

    fn parse_block_end(&mut self, end: TokenType) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        let mut stmts = Vec::<Result<Ref>>::new();

        while !self.cursor.matches(end) {
            stmts.push(self.parse_stmt());
        }

        if !self.cursor.advance_if(end) {
            return Err(ParserError::Msg {
                msg: format!("Expected \"{end}\" keyword at end of block").to_owned(),
                token,
            });
        }

        let mut errors = vec![];
        let stmts = stmts
            .into_iter()
            .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
            .collect_vec();

        if errors.is_empty() {
            Ok(self.alloc(Node::Block(stmts)))
        } else {
            Err(ParserError::Many(errors))
        }
    }

    fn parse_block(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if !self.cursor.advance_if(TokenType::Begin) {
            return Err(ParserError::Msg {
                msg: "Block needs begin keyword".to_owned(),
                token,
            });
        }

        let mut stmts = Vec::<Result<Ref>>::new();

        while !self.cursor.matches(TokenType::End) {
            stmts.push(self.parse_stmt());
        }

        if !self.cursor.advance_if(TokenType::End) {
            return Err(ParserError::Msg {
                msg: "Expected \"end\" keyword at end of block".to_owned(),
                token,
            });
        }

        let mut errors = vec![];
        let stmts = stmts
            .into_iter()
            .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
            .collect_vec();

        if errors.is_empty() {
            Ok(self.alloc(Node::Block(stmts)))
        } else {
            Err(ParserError::Many(errors))
        }
    }

    fn parse_fn_stmt(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Fn) {
            let ident = self.parse_ident()?;

            if !self.cursor.advance_if(TokenType::LParen) {
                return Err(ParserError::Msg {
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
                    return Err(ParserError::Msg {
                        msg: "Function paramater list requires closing brace".to_owned(),
                        token,
                    });
                }

                let ident = self.parse_ident()?;

                if !self.cursor.advance_if(TokenType::Colon) {
                    return Err(ParserError::Msg {
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
                return Err(ParserError::Msg {
                    msg: "Fn paramaters requires a closing parenthesis".to_owned(),
                    token,
                });
            }

            let ret = if self.cursor.advance_if(TokenType::Arrow) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            let block = {
                let mut stmts = Vec::<Result<Ref>>::new();

                while !self.cursor.matches(TokenType::End) {
                    stmts.push(self.parse_stmt());
                }

                if !self.cursor.advance_if(TokenType::End) {
                    return Err(ParserError::Msg {
                        msg: "Function decleration must finish with the \"end\" keyword".to_owned(),
                        token,
                    });
                }

                let mut errors = vec![];
                let stmts = stmts
                    .into_iter()
                    .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
                    .collect_vec();

                if !errors.is_empty() {
                    return Err(ParserError::Many(errors));
                }
                self.alloc(Node::Block(stmts))
            };

            return Ok(self.alloc(Node::FnDecl {
                ident,
                params,
                ret,
                block,
            }));
        }

        Err(ParserError::Msg {
            msg: "Failed to parse fn statement".to_owned(),
            token,
        })
    }

    fn parse_extern_fn(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if !self.cursor.advance_if(TokenType::Extern) {
            return Err(ParserError::Msg {
                msg: "extern fn error".to_owned(),
                token,
            });
        }

        if !self.cursor.advance_if(TokenType::Fn) {
            return Err(ParserError::Msg {
                msg: "Extern function declarations requires 'fn' keyword".to_owned(),
                token,
            });
        }

        let ident = self.parse_ident()?;

        if !self.cursor.advance_if(TokenType::LParen) {
            return Err(ParserError::Msg {
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
                return Err(ParserError::Msg {
                    msg: "Function paramater list requires closing brace".to_owned(),
                    token,
                });
            }

            let ident = self.parse_ident()?;

            if !self.cursor.advance_if(TokenType::Colon) {
                return Err(ParserError::Msg {
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
            return Err(ParserError::Msg {
                msg: "Fn paramaters requires a closing parenthesis".to_owned(),
                token,
            });
        }

        let ret = self.parse_type_expr()?;

        if !self.cursor.advance_if(TokenType::Semicolon) {
            return Err(ParserError::Msg {
                msg: "Extern fn declaration must end with a semicolon".to_owned(),
                token,
            });
        }

        Ok(self.alloc(Node::ExternFnDecl { ident, params, ret }))
    }

    fn parse_import(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        if self.cursor.advance_if(TokenType::Import) {
            let current = self.cursor.current();
            let ty = match current.ty {
                TokenType::String => self.advance_alloc(Node::Import(current.text)),
                _ => {
                    return Err(ParserError::Msg {
                        msg: "Import expects a path".to_owned(),
                        token,
                    });
                }
            };

            if !self.cursor.advance_if(TokenType::Semicolon) {
                return Err(ParserError::Msg {
                    msg: "Import must end with a semicolon".to_owned(),
                    token,
                });
            }

            return Ok(ty);
        }

        Err(ParserError::Msg {
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

            if !self.cursor.advance_if(TokenType::Then) {
                return Err(ParserError::Msg {
                    msg: "If statement must have \"then\" after it's condition".to_owned(),
                    token,
                });
            }

            let t = {
                let mut stmts = Vec::<Result<Ref>>::new();

                let endings = &[TokenType::Else, TokenType::End];

                while !self.cursor.matches_many(endings) {
                    stmts.push(self.parse_stmt());
                }

                if !self.cursor.advance_if_many(endings) {
                    panic!();
                }

                let mut errors = vec![];
                let stmts = stmts
                    .into_iter()
                    .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
                    .collect_vec();

                if errors.is_empty() {
                    Ok(self.alloc(Node::Block(stmts)))
                } else {
                    Err(ParserError::Many(errors))
                }
            }?;

            match self.cursor.current().ty {
                TokenType::Else => {
                    let f = self.parse_block_end(TokenType::End)?;
                    Ok(self.alloc(Node::IfElse { cond, t, f }))
                }
                TokenType::End => Ok(self.alloc(Node::If { cond, block: t })),
                _ => Err(ParserError::Msg {
                    msg: "".to_owned(),
                    token,
                }),
            }
        }

        Err(ParserError::Msg {
            msg: "Failed to parse if statement".to_owned(),
            token,
        })
    }

    fn parse_toplevel_stmt(&mut self) -> Result<'tokens, Ref> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => self.parse_const(),
            TokenType::Let => self.parse_let(),
            TokenType::Import => self.parse_import(),
            TokenType::Type => self.parse_type_stmt(),
            TokenType::Interface => self.parse_interface(),
            TokenType::Fn => self.parse_fn_stmt(),
            TokenType::Extern => self.parse_extern_fn(),
            _ => Err(ParserError::Msg {
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
