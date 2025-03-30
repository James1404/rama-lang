use std::{fs, io, path::Path};

use lexer::Lexer;

mod ast;
mod lexer;
mod parser;
mod tokens;

fn compile<P>(path: P) -> io::Result<()>
where
    P: AsRef<Path>,
{
    let src = std::fs::read_to_string(path)?;

    let mut lexer = Lexer::new(&src);
    let tokens = lexer.run();

    for tok in tokens {
        println!("{}: \"{}\"", Into::<&'static str>::into(tok.ty), tok.text);
    }

    Ok(())
}

fn tests() -> io::Result<()> {
    let paths = fs::read_dir("./test")?;

    for (idx, path) in paths.enumerate() {
        let path = path?.path();

        if path.is_file() {
            println!("<=== Running test {} \"{}\" ===>", idx, path.display());
            compile(path)?;
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    tests()?;

    println!("Hello, world!");

    Ok(())
}
