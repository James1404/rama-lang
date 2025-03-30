#include "ast.hpp"
#include "fmt/base.h"

template <class... Ts> struct overloads : Ts... {
    using Ts::operator()...;
};

namespace AST {
    Node AST::get(Ref ref) { return this->data[ref]; }

    Node* AST::get_ptr(Ref ref) { return &this->data[ref]; }

    void AST::pretty_print() {
        const auto visitor = overloads{
            [](Binary node) {},
            [](Unary node) {},

            [](Float node) {},
            [](Int node) {},
            [](String node) {},
            [](Bool node) {},
            [](Ident node) {},

            [](TopLevelScope node) {},
            [](Scope node) {},

            [](ConstDecl node) {},
            [](VarDecl node) {},

            [](Assignment node) {},

            [](ParamaterList node) {},
            [](Paramater node) {},

            [](FnDecl node) {},
            [](FnCall node) {},

            [](Return node) {},
            [](ImplicitReturn node) {},

            [](Dot node) {},

            [](If node) {},

            [](Match node) {},
            [](MatchBranch node) {},

            [](Comptime node) {},
            [](Extern node) {},

            [](Field node) {},

            [](Type node) {},
            [](Struct node) {},
            [](Enum node) {},

            [](Distinct node) {},
            [](PtrType node) {},

            [](Defer node) {},

            [](Cast node) {},

            [](auto node) { fmt::println("unknown type"); },
        };

        if (auto root = this->root) {
            std::visit(visitor, this->get(*root));
        } else {
            fmt::println("No root node");
        }
    }

    Ref AST::alloc(Node node) {
        u64 index = data.size();
        data.push_back(node);
        return index;
    }
} // namespace AST
