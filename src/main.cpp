#include <fstream>
#include <iostream>
#include <sstream>

#include "common.hpp"
#include "lexer.hpp"

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

string load_from(fs::path filename) {
    std::ifstream file(filename);
    std::stringstream buffer;

    buffer << file.rdbuf();

    return buffer.str();
}

void compile(fs::path filename) {
    string src = load_from(filename);
    
    Lexer lexer(src);
    auto tokens = lexer.run();

    for (const auto& tok : tokens) {
        fmt::println("{}: \"{}\"", (i32)tok.type, tok.text);
    }
}

void tests() {
    fs::path path = fs::current_path() / "test";

    i32 index = 0;
    for (const auto& entry : fs::directory_iterator(path)) {
        fmt::println("<=== Running test {} ===>", index);
        compile(entry.path());

        index++;
    }
}

i32 main(i32 argc, char *argv[]) {
    argparse::ArgumentParser program("rama");

    program.add_argument("--verbose").default_value(false).implicit_value(true);

    argparse::ArgumentParser test_command("run_tests");
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
