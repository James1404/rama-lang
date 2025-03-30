#pragma once

#include "common.hpp"

namespace UIR {
    // Register index
    using Ref = usize;

    enum class Opcodes {
        Nop,
        
        Add, // %0 = %1 + %2
        Sub, // %0 = %1 - %2
        Mul, // %0 = %1 * %2
        Div, // %0 = %1 / %2
        
        Cmp_lt, // %0 = %1 < %2
        Cmp_gt, // %0 = %1 > %2
        Cmp_le, // %0 = %1 <= %2
        Cmp_ge, // %0 = %1 >= %2
        Cmp_eq, // %0 = %1 == %2
        Cmp_nq, // %0 = %1 != %2

        Negate, // %0 = -(%1)
        Not, // %0 = !(%1)

        Cast, // %0 = %1 as %2
        TypeOf, // %0 = typeof %1

        Load_const, // %0 = const[%1]
        Load_builtin, // %0 = builtin[%1]

        Load, // %0 = env[%1]
        Store, // env[%0] = %1

        Ref, // %0 = &(%1)
        Deref, // %0 = *(%1)

        // Terminators
        Goto, // goto %0
        If, // if %0 then goto %1 else goto %2
        Return, // return %0
        Return_implicit, // return_implicit %0
    };

    // CFG Index
    using Loc = usize;

    struct BasicBlock {
        vec<u8> code;
        usize block_len;
    };

    struct CFG {
        vec<BasicBlock> blocks;
        u32 register_count = 0;

        Loc append();
        BasicBlock* get(Loc loc);
        Ref reg();
    };

    struct Decl {
        enum class Mode {
            Var,
            Const,
            Fn
        };
        
        Mode mode;
        CFG graph;
    };
}
