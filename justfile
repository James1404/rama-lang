build:
    @cargo build

run *ARGS='':
    @cargo run -- {{ARGS}}

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
