build:
    @cargo build

run *ARGS='':
    @cargo run -- {{ARGS}}

tests: (run "test -v")

commit MSG:
    @git add .
    @git commit -m "{{MSG}}"

status:
    @git status

push:
    @git push
