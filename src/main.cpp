#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "lexer.hpp"

#include <fmt/core.h>

#include <argparse/argparse.hpp>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

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

    for (const auto& entry : fs::directory_iterator(path)) {
        compile(entry.path());
    }
}

int main(int argc, char *argv[]) {
    argparse::ArgumentParser program("rama");

    program.add_argument("--verbose").default_value(false).implicit_value(true);

    argparse::ArgumentParser test_command("test");
    test_command.add_description("Run all the builtin test files");

    program.add_subparser(test_command);


    try {
        program.parse_args(argc, argv);
    }
    catch (const std::exception& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    if(program.is_subcommand_used(test_command)) {
        tests();
    }
}
