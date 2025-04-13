# Rama

Rama is a work-in-progress, programming language, that's focused on speed, low level control, and safety.

The goal of this langauge for it to be suitable for system's programming, similar to C. So Rama will support manual memory management, and strong type system, and a potentially even a borrow checker for better safety guarentees.

Rama will be compiled to machine code via LLVM, meaning you'll have a fast binary to use.

- [ ] Language Design
  - [x] Type system
    - [x] Struct design
    - [x] Enum design
    - [x] Interface design
  - [x] Control flow structures
    - [x] If statements
    - [ ] For loop
      - [ ] Loop over common containers
      - [ ] Loop over ranges
    - [ ] While loop
    - [ ] Plain loops
- [x] Complete the lexer.
- [x] Complete the parser.
- [ ] Semantic Analysis (Sema)
  - [x] Check and infer types
  - [x] Output fully typed TIR
- [ ] Typed IR (TIR)
  - [ ] Complete TIR Instruction set
  - [ ] Full format specification
- [x] Compile TIR down to machine code via LLVM.
- [ ] Implement a borrow checker to guarantee memory safety.

## Building

Either run: `cargo run`

### Dependencies

- Rustc
- Cargo
- LLVM-20

The better option is to use the nix package manager and run `nix develop`
