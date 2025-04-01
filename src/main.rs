use std::{fs, io, path::Path};

use lexer::Lexer;
use parser::Parser;

use clap::{Parser as ClapParser, Subcommand};
use sema::Sema;
use uirgen::UIRGen;

mod ast;
mod lexer;
mod parser;
mod sema;
mod tir;
mod tokens;
mod uir;
mod uirgen;

#[derive(ClapParser)]
#[command(version, about, author, long_about = "A small WIP Compiler")]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Test,
}

fn compile<P>(path: P) -> io::Result<()>
where
    P: AsRef<Path>,
{
    let src = std::fs::read_to_string(path)?;

    let lexer = Lexer::new(&src);
    let tokens = lexer.run();

    for tok in &tokens {
        println!("{}: \"{}\"", Into::<&'static str>::into(&tok.ty), tok.text);
    }

    let parser = Parser::new(&tokens);
    let ast = parser.run();

    ast.pretty_print();

    let uirgen = UIRGen::new(ast);
    let uir = uirgen.run();

    uir.pretty_print();

    let sema = Sema::new(uir);
    let tir = sema.run();

    tir.pretty_print();

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
    env_logger::init();

    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Test) => {
            tests()?;
        }
        None => {}
    }

    Ok(())
}
