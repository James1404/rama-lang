#pragma once

#include "token.hpp"

#include <vector>

class Lexer {
    std::string_view src;
    std::vector<Token> out;

    int start = 0;
    int location = 0;

    int line = 0;
    int line_offset = 0;

    char current();

    void advance();
    bool advance_if(char c);

    bool match(char c);

    bool eof();

    void append_token(TokenType type);
    void append_ident();
    void append_single(TokenType type);
    void append_single_or_next(char cond, TokenType t, TokenType f);
public:
    Lexer(std::string_view src);
    
    std::vector<Token> run();
};
