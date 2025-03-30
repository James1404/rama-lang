#include "token.hpp"

#include <map>

static std::map<string_view, TokenType> keywords = {
    {"true", TokenType::True},
    {"false", TokenType::False},

    {"if", TokenType::If},
    {"else", TokenType::Else},

    {"for", TokenType::For},
    {"while", TokenType::While},
    {"loop", TokenType::Loop},

    {"continue", TokenType::Continue},
    {"break", TokenType::Break},
    {"return", TokenType::Return},

    {"and", TokenType::And},
    {"or", TokenType::Or},

    {"comptime", TokenType::Comptime},
    {"extern", TokenType::Extern},

    {"type", TokenType::Type},
    {"enum", TokenType::Enum},
    {"struct", TokenType::Struct},
    {"distinct", TokenType::Distinct},

    {"const", TokenType::Const},
    {"var", TokenType::Var},

    {"import", TokenType::Import},

    {"fn", TokenType::Fn},

    {"defer", TokenType::Defer},
    {"as", TokenType::As},
};

TokenType string_to_tokentype(string_view str) {
    if (auto iter = keywords.find(str); iter != keywords.end()) {
        return iter->second;
    }

    return TokenType::Ident;
}
