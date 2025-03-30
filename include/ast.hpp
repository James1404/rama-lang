#pragma once

#include "common.hpp"
#include "token.hpp"
#include <optional>
#include <variant>

namespace AST {
    using Ref = usize;

    struct Error {
        string_view msg;
        Token tok;
    };

    struct Binary {
        Ref lhs;
        Ref rhs;
        Token op;
    };

    struct Unary {
        Ref value;
        Token op;
    };

    struct Float {
        string_view value;
    };
    struct Int {
        string_view value;
    };
    struct String {
        string_view value;
    };
    struct Bool {
        bool value;
    };
    struct Ident {
        Token value;
    };

    struct TopLevelScope {
        vec<Ref> nodes;
    };
    struct Scope {
        vec<Ref> nodes;
    };

    struct ConstDecl {
        Ref ident;
        std::optional<Ref> type;
        Ref value;
    };
    struct VarDecl {
        Ref ident;
        std::optional<Ref> type;
        Ref value;
    };

    struct Assignment {
        Ref ident;
        Ref value;
    };

    struct ParamaterList {
        vec<Ref> nodes;
    };
    struct Paramater {
        Ref ident;
        Ref value;
    };

    struct FnDecl {
        Ref params, ret, block;
    };
    struct FnCall {
        Ref fn;
        vec<Ref> args;
    };

    struct Return {
        Ref value;
    };
    struct ImplicitReturn {
        Ref value;
    };

    struct Dot {
        Ref lhs, ident;
    };

    struct If {
        Ref cond, t, f;
    };

    struct Match {
        Ref value;
        vec<Ref> branches;
    };
    struct MatchBranch {
        Ref pattern, value;
    };

    struct Comptime {
        Ref value;
    };
    struct Extern {
        Ref value;
    };

    struct FnType {
        Ref params, ret;
    };

    struct Field {
        Ref ident;
        std::optional<Ref> type, default_value;
    };

    struct Type {};
    struct Struct {};
    struct Enum {};

    struct Distinct {
        Ref value;
    };
    struct PtrType {
        Ref value;
    };

    struct Defer {
        Ref value;
    };

    struct Cast {
        Ref value, type;
    };

    using Node = std::variant<
        Error,
        
        Binary, Unary,

        Float, Int, String, Bool, Ident,

        TopLevelScope, Scope,

        ConstDecl, VarDecl,

        Assignment,

        ParamaterList, Paramater,

        FnDecl, FnCall,

        Return, ImplicitReturn,

        Dot,

        If,

        Match, MatchBranch,

        Comptime, Extern,

        FnType,

        Field,

        Type, Struct, Enum,

        Distinct, PtrType,

        Defer,

        Cast>;

    class AST {
        std::optional<Ref> root = {};
        vec<Node> data;

    public:
        Node get(Ref ref);
        Node* get_ptr(Ref ref);

        void pretty_print();

        Ref alloc(Node node);
    };
} // namespace AST
