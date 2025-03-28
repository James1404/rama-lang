#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "lexer.hpp"

namespace fs = std::filesystem;

std::string load_from(fs::path filename) {
    std::ifstream file(filename);
    std::stringstream buffer;

    buffer << file.rdbuf();

    return buffer.str();
}

void compile(fs::path filename) {
    std::string src = load_from(filename);
    
    Lexer lexer(src);
    auto tokens = lexer.run();
}

void tests() {
    fs::path path = fs::current_path() / "test";
    std::cout << fs::current_path().append("test") << std::endl;

    for (const auto& entry : fs::directory_iterator(path)) {
        compile(entry.path());
    }
}

int main() {
    tests();

    return 0;
}
