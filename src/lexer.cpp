#include "lexer.hpp"
#include "token.hpp"
#include <utility>

Lexer::Lexer(std::string_view src) {
    this->src = src;
}

char Lexer::current() {
    return this->src[this->location];
}

void Lexer::advance() {
    this->location++;
}

bool Lexer::advance_if(char c) {
    if(this->match(c)) {
        this->advance();
        return true;
    }

    return false;
}

bool Lexer::match(char c) { return this->current() == c; }

bool Lexer::eof() { return this->location >= this->src.size(); }

void Lexer::append_token(TokenType type) {
    this->out.push_back(Token {
            .type = type,
            .location = this->start,
            .line = this->line,
            .line_offset = this->line_offset,
            .text = this->src.substr(this->start, this->location - this->start),
        });
}

std::vector<Token> Lexer::run() {
    while(!this->eof()) {
        char c = this->current();
        this->start = this->current();
        this->advance();

        switch (c) {
        case '(': { this->append_token(TokenType::LParen); } break;
        case ')': { this->append_token(TokenType::RParen); } break;
        case '{': { this->append_token(TokenType::LBrace); } break;
        case '}': { this->append_token(TokenType::RBrace); } break;
        case '[': { this->append_token(TokenType::LBracket); } break;
        case ']': { this->append_token(TokenType::RBracket); } break;
            
        case ';': { this->append_token(TokenType::SemiColon); } break;
        case ':': { this->append_token(TokenType::Colon); } break;

        case '.': { this->append_token(TokenType::Dot); } break;
        case ',': { this->append_token(TokenType::Comma); } break;

        case '+': { this->append_token(TokenType::Plus); } break;
        case '-': { this->append_token(TokenType::Minus); } break;
        case '*': { this->append_token(TokenType::Multiply); } break;
        case '/': { this->append_token(TokenType::Divide); } break;

        default: {} break;
        }
    }

    return std::move(this->out);
}
