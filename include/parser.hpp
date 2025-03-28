#pragma once

#include "common.hpp"
#include "ast.hpp"
#include "token.hpp"

class Parser {
    u32 current = 0;
    vec<Token> tokens;
    AST::AST out;

    void advance();
    void advance_if(TokenType type);
    bool match(TokenType type);
public:
    void run();
};
