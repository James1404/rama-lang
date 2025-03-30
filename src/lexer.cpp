#include "lexer.hpp"
#include "token.hpp"

#include <cassert>
#include <cstdlib>
#include <utility>

Lexer::Lexer(string_view src) { this->src = src; }

char Lexer::current() { return this->src.at(this->location); }

void Lexer::advance() { this->location++; }

bool Lexer::advance_if(char c) {
    if (this->match(c)) {
        this->advance();
        return true;
    }

    return false;
}

bool Lexer::match(char c) { return this->current() == c; }

bool Lexer::eof() { return this->location >= this->src.size(); }

void Lexer::append_token(TokenType type) {
    this->out.push_back(Token{
        .type = type,
        .location = this->start,
        .line = this->line,
        .line_offset = this->line_offset,
        .text = this->src.substr(this->start, this->location - this->start),
    });
}

void Lexer::append_ident() {
    auto text = this->src.substr(this->start, this->location - this->start);

    TokenType type = string_to_tokentype(text);

    this->out.push_back(Token{
        .type = type,
        .location = this->start,
        .line = this->line,
        .line_offset = this->line_offset,
        .text = text,
    });
}

void Lexer::append_single(TokenType type) {
    this->advance();
    this->append_token(type);
}

void Lexer::append_single_or_next(char cond, TokenType t, TokenType f) {
    this->advance();
    this->append_token(this->advance_if(cond) ? t : f);
}

static bool is_letter(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool is_digit(char c) { return (c >= '0' && c <= '9'); }

static bool is_ident_start_char(char c) { return is_letter(c) || c == '_'; }
static bool is_ident_middle_char(char c) {
    return is_ident_start_char(c) || is_digit(c);
}

std::vector<Token> Lexer::run() {
    while (!this->eof()) {
        char c = this->current();
        this->start = this->location;

        switch (c) {
        case '(': {
            this->append_single(TokenType::LParen);
        } break;
        case ')': {
            this->append_single(TokenType::RParen);
        } break;
        case '{': {
            this->append_single(TokenType::LBrace);
        } break;
        case '}': {
            this->append_single(TokenType::RBrace);
        } break;
        case '[': {
            this->append_single(TokenType::LBracket);
        } break;
        case ']': {
            this->append_single(TokenType::RBracket);
        } break;

        case ';': {
            this->append_single(TokenType::SemiColon);
        } break;
        case ':': {
            this->append_single(TokenType::Colon);
        } break;

        case '.': {
            this->append_single(TokenType::Dot);
        } break;
        case ',': {
            this->append_single(TokenType::Comma);
        } break;

        case '+': {
            this->append_single(TokenType::Plus);
        } break;
        case '-': {
            this->append_single(TokenType::Minus);
        } break;
        case '*': {
            this->append_single(TokenType::Multiply);
        } break;
        case '/': {
            this->append_single(TokenType::Divide);
        } break;

        case '=': {
            this->append_single_or_next('=', TokenType::EqualEq,
                                        TokenType::Equal);
        } break;
        case '!': {
            this->append_single_or_next('=', TokenType::NotEq, TokenType::Not);
        } break;

        case '"': {
            this->advance();
            while (!this->match('"')) {
                this->advance();
            }

            this->advance();
            this->append_token(TokenType::String);
        } break;

        case '\n':
        case '\r': {
            this->line++;
            this->line_offset = 0;
        }
        case '\t':
        case ' ': {
            this->advance();
        } break;

        default: {
            if (is_ident_start_char(c)) {
                this->advance();

                while (is_ident_middle_char(this->current())) {
                    this->advance();
                }

                this->append_ident();
                continue;
            }

            if (is_digit(c)) {
                this->advance();

                bool is_float = false;
                while (is_digit(this->current()) || this->current() == '.') {
                    if (this->match('.'))
                        is_float = true;

                    this->advance();
                }

                this->append_token(is_float ? TokenType::Float
                                            : TokenType::Int);
                continue;
            }

            fmt::println("Current char: '{}'", this->current());
            abort();
        } break;
        }
    }

    return std::move(this->out);
}
