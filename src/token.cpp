#include "token.hpp"

#include <map>

static std::map<string_view, TokenType> keywords = {
    {"true", TokenType::True},
    {"false", TokenType::False},

    {"type", TokenType::Type},
    {"enum", TokenType::Enum},
    {"struct", TokenType::Struct},

    {"const", TokenType::Const},
    {"var", TokenType::Var},

    {"import", TokenType::Import},

    {"fn", TokenType::Fn},

    {"for", TokenType::For},
    {"while", TokenType::While},
    {"loop", TokenType::Loop},

    {"continue", TokenType::Continue},
    {"break", TokenType::Break},
    {"return", TokenType::Return},
};

TokenType string_to_tokentype(string_view str) {
    if (auto iter = keywords.find(str); iter != keywords.end()) {
        return iter->second;
    }

    return TokenType::Ident;
}
