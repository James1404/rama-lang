#pragma once

#include "common.hpp"
#include "token.hpp"

class Lexer {
    string_view src;
    vec<Token> out;

    i32 start = 0;
    i32 location = 0;

    i32 line = 0;
    i32 line_offset = 0;

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
    Lexer(string_view src);
    
    vec<Token> run();
};
