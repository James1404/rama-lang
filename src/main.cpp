#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "lexer.hpp"

std::string load_from(std::filesystem::path filename) {
    std::ifstream file(filename);
    std::stringstream buffer;

    buffer << file.rdbuf();

    return buffer.str();
}

void compile(std::filesystem::path filename) {
    std::string src = load_from(filename);
    
    Lexer lexer(src);
    auto tokens = lexer.run();
}

void tests() {
    std::string path = "./tests";
    for (const auto& entry : std::filesystem::directory_iterator(path)) {
        compile(entry.path());
    }
}

int main() {
    tests();

    return 0;
}
