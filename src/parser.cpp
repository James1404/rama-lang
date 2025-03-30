#include "parser.hpp"
#include <algorithm>

Token Parser::current() { return this->tokens.at(this->position); }

void Parser::advance() { this->position++; }

bool Parser::advance_if(TokenType type) {
    if (this->match(type)) {
        this->advance();
        return true;
    }

    return false;
}

bool Parser::match(TokenType type) { return this->current().type == type; }

AST::Ref Parser::parse_ident() {}

AST::Ref Parser::parse_value() {}
AST::Ref Parser::parse_expr() {}

AST::Ref Parser::parse_type() {}
AST::Ref Parser::parse_struct() {}
AST::Ref Parser::parse_enum() {}

AST::Ref Parser::parse_if() {}

AST::Ref Parser::parse_var() {}
AST::Ref Parser::parse_const() {}

AST::Ref Parser::parse_fn_decl() {}
AST::Ref Parser::parse_fn_stmt() {}
AST::Ref Parser::parse_stmt() {}

AST::AST Parser::run() { return std::move(this->out); }
