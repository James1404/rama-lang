#pragma once

#include "ast.hpp"
#include "common.hpp"
#include "token.hpp"

class Parser {
    u32 position = 0;
    vec<Token> tokens;
    AST::AST out;

    Token current();
    void advance();
    bool advance_if(TokenType type);
    bool match(TokenType type);

    AST::Ref parse_ident();

    AST::Ref parse_value();
    AST::Ref parse_expr();

    AST::Ref parse_type();
    AST::Ref parse_struct();
    AST::Ref parse_enum();

    AST::Ref parse_if();

    AST::Ref parse_var();
    AST::Ref parse_const();

    AST::Ref parse_fn_decl();
    AST::Ref parse_fn_stmt();
    AST::Ref parse_stmt();

  public:
    AST::AST run();

    Parser(vec<Token> tokens) : tokens(tokens) {}
};
