#include "parser.hpp"
#include "ast.hpp"
#include "token.hpp"
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

AST::Ref Parser::advance_alloc(AST::Node node) {
    advance();
    return out.alloc(node);
}

AST::Ref Parser::parse_ident() {
    Token start = current();

    if (start.type == TokenType::Ident) {
        advance();

        return out.alloc(AST::Ident(start));
    }

    return out.alloc(AST::Error("invalid ident token", start));
}

AST::Ref Parser::parse_value() {
    Token tok = current();

    AST::Ref value;
    switch (tok.type) {
    case TokenType::Ident:
        value = parse_ident();
        break;
    case TokenType::Float:
        value = advance_alloc(AST::Float(tok.text));
        break;
    case TokenType::Int:
        value = advance_alloc(AST::Int(tok.text));
        break;
    case TokenType::String:
        value = advance_alloc(AST::String(tok.text));
        break;
    case TokenType::True:
        value = advance_alloc(AST::Bool(true));
        break;
    case TokenType::False:
        value = advance_alloc(AST::Bool(false));
        break;
    case TokenType::LParen: {
        advance();

        AST::Ref expr = parse_expr();
        if(advance_if(TokenType::RParen)) {
            value = expr;
        }
        else {
            value = out.alloc(AST::Error("'(' Needs a closing ')'", tok));
        }
        break;
    }
    case TokenType::LBrace:
        value = parse_scope();
        break;
    case TokenType::If:
        value = parse_if();
        break;
    case TokenType::Comptime:
        advance();
        value = out.alloc(AST::Comptime(parse_expr()));
        break;
    default:
        value = advance_alloc(AST::Error("Unable to parse value", tok));
        break;
    }

    return value;
}

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

AST::Ref Parser::parse_scope() {}
AST::Ref Parser::parse_toplevel() {}

AST::AST Parser::run() { return std::move(this->out); }
