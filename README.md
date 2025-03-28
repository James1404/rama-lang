# Warlock

Warlock is a work-in-progress, custom lisp dialect, focused on speed, low level control, and safety.

The goal of this langauge for it to be suitable for system's programming, similar to C. So Warlock will support manual memory management, and strong type system, and a borrow checker for better safety guarentees.

Warlock will be compiled to machine code via LLVM, meaning you'll have a fast binary to use.

- [ ] Implement compile time macros.
- [ ] Add a static type system from the paper [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/pdf/1306.6032).
- [ ] Compile to machine code using LLVM
- [ ] Implement a borrow checker to guarantee memory safety.

## Building

### Dependencies

- GCC
- CMake
- llvm-18

The better option is to use the nix package manager and run `nix develop`
