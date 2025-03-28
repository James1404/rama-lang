#pragma once

#include "common.hpp"
#include "token.hpp"

namespace AST {
    using Ref = usize;

    enum class NodeType {
        Binary,
        Unary,
    };
    union NodeData {
        struct{
            Ref lhs;
            Ref rhs;
            Token op;
        } Binary;
        struct{
            Ref value;
            Token op;
        } Unary;
    };

    struct Node {
        NodeType type;
        NodeData data;
    };

    class AST {
        vec<Node> data;

        Node get(Ref ref);
        Node* get_ptr(Ref ref);
    };
}
