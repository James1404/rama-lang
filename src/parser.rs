use std::{rc::Rc, result};

use itertools::Itertools;

use crate::{
    ast::{
        self, AST, BinOp, Block, ConstDecl, Expr, ExternFn, Fn, Ident, If, Import, LetDecl,
        LiteralRecordField, Param, Statement, TopLevelStatement, Type, Value,
    },
    lexer::{Token, TokenType},
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

    fn get_advance(&mut self) -> Token<'a> {
        let t = self.current();
        self.advance();
        t
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

    #[must_use]
    fn expects(&mut self, expected: TokenType) -> Result<'a, ()> {
        let token = self.current();

        if !self.advance_if(expected) {
            return Err(ParserError::Msg {
                msg: format!("Expected \"{expected}\" got \"{token}\""),
                token,
            });
        }

        Ok(())
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

enum Operator {
    Invalid,

    Binary(BinOp),
    Assign,
    Cast,
}

impl From<TokenType> for Operator {
    fn from(value: TokenType) -> Self {
        use Operator::*;
        match value {
            TokenType::Plus => Binary(BinOp::Add),
            TokenType::Minus => Binary(BinOp::Sub),
            TokenType::Asterix => Binary(BinOp::Mul),
            TokenType::Slash => Binary(BinOp::Div),

            TokenType::LAngle => Binary(BinOp::Less),
            TokenType::LessEq => Binary(BinOp::LessEq),
            TokenType::RAngle => Binary(BinOp::Greater),
            TokenType::GreaterEq => Binary(BinOp::GreaterEq),

            TokenType::EqualEqual => Binary(BinOp::Eq),
            TokenType::NotEqual => Binary(BinOp::NotEq),

            TokenType::Equal => Assign,
            TokenType::As => Cast,

            _ => Invalid,
        }
    }
}

impl Operator {
    fn precedence(&self) -> i32 {
        use Operator::*;

        match self {
            Binary(op) => op.precedence(),
            Assign | Cast => 0,
            Invalid => -1,
        }
    }
}

pub struct Parser<'a> {
    cursor: Cursor<'a>,
}

impl<'a, 'b> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token<'a>>) -> Self {
        Self {
            cursor: Cursor::new(tokens),
        }
    }

    fn advance_alloc_ok<T>(&mut self, t: T) -> Result<'a, Rc<T>> {
        self.cursor.advance();
        Ok(Rc::new(t))
    }

    fn parse_ident(&mut self) -> Result<'a, Ident<'a>> {
        let token = self.cursor.current();
        self.cursor.expects(TokenType::Ident)?;

        Ok(Ident(token.text))
    }

    fn expression(&mut self, lhs: Rc<Expr<'a>>, min_precedence: i32) -> Result<'a, Rc<Expr<'a>>> {
        let mut last = lhs;
        let mut current = Operator::from(self.cursor.current().ty);

        while current.precedence() >= min_precedence {
            let op = Operator::from(self.cursor.get_advance().ty);

            if let Operator::Cast = op {
                let rhs = self.parse_type_expr()?;
                last = Rc::new(Expr::Cast {
                    value: last,
                    ty: rhs,
                });
                break;
            }

            let mut rhs = self.parse_value()?;

            current = self.cursor.current().ty.into();

            while current.precedence() > op.precedence() {
                rhs = self.expression(
                    rhs,
                    op.precedence()
                        + if current.precedence() > op.precedence() {
                            1
                        } else {
                            0
                        },
                )?;
            }

            last = Rc::new(match op {
                Operator::Binary(op) => Expr::Binary { lhs: last, rhs, op },
                Operator::Assign => Expr::Assign {
                    lhs: last,
                    value: rhs,
                },
                _ => panic!(),
            });
        }

        Ok(last)
    }

    // https://en.wikipedia.org/wiki/Operator-precedence_parser
    fn parse_expr(&mut self) -> Result<'a, Rc<Expr<'a>>> {
        let lhs = self.parse_value()?;
        self.expression(lhs, 0)
    }

    fn parse_value(&mut self) -> Result<'a, Rc<Expr<'a>>> {
        let token = self.cursor.current();

        let value = match token.ty {
            TokenType::String => self.advance_alloc_ok(Expr::Value(Value::String(token.text))),
            TokenType::Float => self.advance_alloc_ok(Expr::Value(Value::Float(token.text))),
            TokenType::Int => self.advance_alloc_ok(Expr::Value(Value::Int(token.text))),
            TokenType::Ident => {
                let ident = self.parse_ident()?;
                Ok(Rc::new(Expr::Value(Value::Ident(ident))))
            }
            TokenType::True => self.advance_alloc_ok(Expr::Value(Value::Bool(true))),
            TokenType::False => self.advance_alloc_ok(Expr::Value(Value::Bool(false))),

            TokenType::LParen => {
                self.cursor.advance();

                let expr = self.parse_expr()?;

                self.cursor.expects(TokenType::RParen)?;

                Ok(expr)
            }

            TokenType::RecordConstructor => {
                self.cursor.advance();

                let mut fields = Vec::<LiteralRecordField>::new();

                while !self.cursor.matches(TokenType::RBrace) {
                    if self.cursor.eof() {
                        break;
                    }

                    let ident = self.parse_ident()?;
                    self.cursor.expects(TokenType::Colon)?;
                    let value = self.parse_expr()?;

                    fields.push(LiteralRecordField { ident, value });

                    if !self.cursor.advance_if(TokenType::Comma) {
                        break;
                    }
                }

                self.cursor.expects(TokenType::RBrace)?;

                // TODO: Allow alternative "Vec2 { x: 5, y: 16 }"
                // syntax as opposed to .{ x: 5, y: 16 } inference

                Ok(Rc::new(Expr::Value(Value::Record { fields })))
            }

            TokenType::If => Ok(Rc::new(Expr::If(self.parse_if()?))),
            TokenType::Begin => Ok(Rc::new(Expr::Block(self.parse_block()?))),

            _ => {
                self.cursor.advance();

                return Err(ParserError::Msg {
                    msg: format!("Expected value got \"{token}\""),
                    token,
                });
            }
        };

        if self.cursor.advance_if(TokenType::LParen) {
            let mut args: Vec<Rc<Expr>> = Vec::new();

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

            return Ok(Rc::new(Expr::Value(Value::Call { func: value?, args })));
        }

        value
    }

    fn parse_enum_type(&mut self) -> Result<'a, Rc<Type<'a>>> {
        let token = self.cursor.current();

        self.cursor.expects(TokenType::Enum)?;

        let mut variants = Vec::<ast::EnumVariant>::new();

        while !self.cursor.advance_if(TokenType::End) {
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

            self.cursor.expects(TokenType::Semicolon)?;
        }

        return Ok(Rc::new(Type::Enum(variants)));
    }

    fn parse_record_type(&mut self) -> Result<'a, Rc<Type<'a>>> {
        let token = self.cursor.current();

        self.cursor.expects(TokenType::Record)?;

        let mut fields = Vec::<ast::RecordField>::new();

        while !self.cursor.advance_if(TokenType::End) {
            if self.cursor.eof() {
                return Err(ParserError::Msg {
                    msg: "Record body requires \"end\" keywords".to_owned(),
                    token,
                });
            }

            let ident = self.parse_ident()?;

            if !self.cursor.advance_if(TokenType::Colon) {
                return Err(ParserError::Msg {
                    msg: "Record field requires a type".to_owned(),
                    token,
                });
            }

            let ty = self.parse_type_expr()?;

            fields.push(ast::RecordField { ident, ty });

            if !self.cursor.advance_if(TokenType::Semicolon) {
                return Err(ParserError::Msg {
                    msg: "Record field must end with a semicolon".to_owned(),
                    token,
                });
            }
        }

        Ok(Rc::new(Type::Record(fields)))
    }

    fn parse_type_expr(&mut self) -> Result<'a, Rc<Type<'a>>> {
        let token = self.cursor.current();

        let value = match token.ty {
            TokenType::Asterix => {
                self.cursor.advance();

                let inner = self.parse_type_expr()?;
                return Ok(Rc::new(Type::Ptr(inner)));
            }
            TokenType::Record => self.parse_record_type()?,
            TokenType::Enum => self.parse_enum_type()?,
            TokenType::Ident => Rc::new(Type::Ident(self.parse_ident()?)),
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

                    Rc::new(Type::Array(inner, len))
                } else {
                    Rc::new(Type::Slice(inner))
                };

                self.cursor.expects(TokenType::RBracket)?;

                node
            }
            _ => {
                return Err(ParserError::Msg {
                    msg: "Invalid type".to_owned(),
                    token,
                });
            }
        };

        Ok(value)
    }

    fn parse_type_stmt(&mut self) -> Result<'a, TopLevelStatement<'a>> {
        self.cursor.expects(TokenType::Type)?;

        let ident = self.parse_ident()?;

        self.cursor.expects(TokenType::Is)?;

        let inner = self.parse_type_expr()?;

        Ok(TopLevelStatement::Type { ident, inner })
    }

    fn parse_stmt(&mut self) -> Result<'a, Statement<'a>> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => Ok(Statement::ConstDecl(self.parse_const_decl()?)),
            TokenType::Let => Ok(Statement::LetDecl(self.parse_let_decl()?)),

            TokenType::Return => {
                self.cursor.advance();

                if self.cursor.advance_if(TokenType::Semicolon) {
                    Ok(Statement::ReturnNone)
                } else {
                    let expr = self.parse_expr()?;
                    self.cursor.expects(TokenType::Semicolon)?;
                    Ok(Statement::Return(expr))
                }
            }

            _ => {
                let expr = self.parse_expr()?;

                match *expr {
                    Expr::If(_) | Expr::Match(_) => {}
                    _ => self.cursor.expects(TokenType::Semicolon)?,
                }

                Ok(Statement::Expr(expr))
            }
        }
    }

    fn parse_block_end(&mut self, end: TokenType) -> Result<'a, Block<'a>> {
        let token = self.cursor.current();

        let mut stmts = Vec::<Result<Statement>>::new();

        while !self.cursor.matches(end) {
            stmts.push(self.parse_stmt());
        }

        if !self.cursor.advance_if(end) {
            return Err(ParserError::Msg {
                msg: format!("Expected \"{end}\" keyword at end of block"),
                token,
            });
        }

        let mut errors = vec![];
        let statements = stmts
            .into_iter()
            .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
            .collect_vec();

        if errors.is_empty() {
            Ok(Block {
                statements,
                result: None,
            })
        } else {
            Err(ParserError::Many(errors))
        }
    }

    fn parse_block(&mut self) -> Result<'a, Block<'a>> {
        let token = self.cursor.current();

        if !self.cursor.advance_if(TokenType::Begin) {
            return Err(ParserError::Msg {
                msg: "Block needs begin keyword".to_owned(),
                token,
            });
        }

        let mut stmts = Vec::<Result<Statement>>::new();

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
        let statements = stmts
            .into_iter()
            .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
            .collect_vec();

        if errors.is_empty() {
            Ok(Block {
                statements,
                result: None,
            })
        } else {
            Err(ParserError::Many(errors))
        }
    }

    fn parse_let_decl(&mut self) -> Result<'a, LetDecl<'a>> {
        self.cursor.expects(TokenType::Let)?;

        let ident = self.parse_ident()?;

        let ty = if self.cursor.advance_if(TokenType::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.cursor.expects(TokenType::Equal)?;

        let value = self.parse_expr()?;

        self.cursor.expects(TokenType::Semicolon)?;

        Ok(LetDecl { ident, ty, value })
    }

    fn parse_const_decl(&mut self) -> Result<'a, ConstDecl<'a>> {
        self.cursor.expects(TokenType::Const)?;

        let ident = self.parse_ident()?;

        let ty = if self.cursor.advance_if(TokenType::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.cursor.expects(TokenType::Equal)?;

        let value = self.parse_expr()?;

        self.cursor.expects(TokenType::Semicolon)?;

        Ok(ConstDecl { ident, ty, value })
    }

    fn parse_fn(&mut self) -> Result<'a, Fn<'a>> {
        let token = self.cursor.current();

        self.cursor.expects(TokenType::Fn)?;

        let ident = self.parse_ident()?;

        self.cursor.expects(TokenType::LParen)?;

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

        self.cursor.expects(TokenType::RParen)?;

        let ret = if self.cursor.advance_if(TokenType::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let block = self.parse_block_end(TokenType::End)?;

        Ok(Fn {
            ident,
            params,
            ret,
            block,
        })
    }

    fn parse_extern_fn(&mut self) -> Result<'a, ExternFn<'a>> {
        let token = self.cursor.current();

        self.cursor.expects(TokenType::Extern)?;
        self.cursor.expects(TokenType::Fn)?;

        let ident = self.parse_ident()?;

        self.cursor.expects(TokenType::LParen)?;

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

        self.cursor.expects(TokenType::RParen)?;

        let ret = if self.cursor.advance_if(TokenType::Arrow) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.cursor.expects(TokenType::Semicolon)?;
        Ok(ExternFn { ident, params, ret })
    }

    fn parse_import(&mut self) -> Result<'a, Import<'a>> {
        self.cursor.expects(TokenType::Import)?;

        let path = match self.cursor.get_advance() {
            Token {
                ty: TokenType::String,
                text,
                ..
            } => Ok(text),
            token => Err(ParserError::Msg {
                msg: "Import expects a path".to_owned(),
                token,
            }),
        }?;

        self.cursor.expects(TokenType::Semicolon)?;

        Ok(Import(path))
    }

    fn parse_if(&mut self) -> Result<'a, If<'a>> {
        let token = self.cursor.current();

        self.cursor.expects(TokenType::If)?;

        let cond = self.parse_expr()?;

        if !self.cursor.advance_if(TokenType::Then) {
            return Err(ParserError::Msg {
                msg: "If statement must have \"then\" after it's condition".to_owned(),
                token,
            });
        }

        let then = {
            let mut stmts = Vec::<Result<Statement>>::new();

            let endings = &[TokenType::Else, TokenType::End];

            while !self.cursor.matches_many(endings) {
                stmts.push(self.parse_stmt());
            }

            let mut errors = vec![];
            let statements = stmts
                .into_iter()
                .filter_map(|r| r.map_err(|e| errors.push(e)).ok())
                .collect_vec();

            if errors.is_empty() {
                Ok(Block {
                    statements,
                    result: None,
                })
            } else {
                Err(ParserError::Many(errors))
            }
        }?;

        let otherwise = if self.cursor.advance_if(TokenType::Else) {
            Some(self.parse_block_end(TokenType::End)?)
        } else {
            self.cursor.expects(TokenType::End)?;
            None
        };

        Ok(If {
            cond,
            then,
            otherwise,
        })
    }

    fn parse_toplevel_stmt(&mut self) -> Result<'a, TopLevelStatement<'a>> {
        let token = self.cursor.current();

        match token.ty {
            TokenType::Const => Ok(TopLevelStatement::ConstDecl(self.parse_const_decl()?)),
            TokenType::Let => Ok(TopLevelStatement::LetDecl(self.parse_let_decl()?)),
            TokenType::Import => Ok(TopLevelStatement::Import(self.parse_import()?)),
            TokenType::Type => self.parse_type_stmt(),
            TokenType::Fn => Ok(TopLevelStatement::Fn(self.parse_fn()?)),
            TokenType::Extern => Ok(TopLevelStatement::ExternFn(self.parse_extern_fn()?)),
            _ => Err(ParserError::Msg {
                msg: format!("Invalid toplevel statement, got \"{token}\""),
                token,
            }),
        }
    }

    pub fn run(mut self) -> Result<'a, AST<'a>> {
        let mut statements: Vec<TopLevelStatement> = vec![];

        while !self.cursor.eof() {
            statements.push(self.parse_toplevel_stmt()?);
        }

        Ok(AST { statements })
    }
}
