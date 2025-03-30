# Rama

Rama is a work-in-progress, programming language, that's focused on speed, low level control, and safety.

The goal of this langauge for it to be suitable for system's programming, similar to C. So Rama will support manual memory management, and strong type system, and a potentially even a borrow checker for better safety guarentees.

Rama will be compiled to machine code via LLVM, meaning you'll have a fast binary to use.

- [x] Complete the lexer.
- [ ] Complete the parser.
- [ ] Add an untyped IR (UIR) for compile time execution and for type checking / inference.
- [ ] Compile UIR to a lower level typed IR (TIR), with fully defined types.
- [ ] Compile TIR down to machine code via LLVM.
- [ ] Implement a borrow checker to guarantee memory safety.

## Building

### Dependencies

- GCC
- CMake
- llvm-18

The better option is to use the nix package manager and run `nix develop`
