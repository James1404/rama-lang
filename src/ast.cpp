#include "ast.hpp"

namespace AST {
    Node AST::get(Ref ref) {
        return this->data[ref];
    }

    Node* AST::get_ptr(Ref ref) {
        return &this->data[ref];
    }
}
