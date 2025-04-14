build:
    @cargo build

run *ARGS='':
    @cargo run -- {{ARGS}}

compile file: (run "--print-ast compile" file)

tests: (run "--print-ast --backend c test")

dbg:
    rust-gdb target/debug/rama-lang

commit MSG:
    @git add .
    @git commit -m "{{MSG}}"

status:
    @git status

push:
    @git push

update-flake:
    nix flake update
