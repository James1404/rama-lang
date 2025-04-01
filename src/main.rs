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
    Test {
        #[arg(short, long)]
        verbose: bool,
    },
}

fn compile<P>(path: P, verbose: bool) -> io::Result<()>
where
    P: AsRef<Path>,
{
    let src = std::fs::read_to_string(path)?;

    let lexer = Lexer::new(&src);
    let tokens = lexer.run();

    if verbose {
        println!("<== Printing Tokens ==>");
        for tok in &tokens {
            println!("{}: \"{}\"", Into::<&'static str>::into(&tok.ty), tok.text);
        }
        println!()
    }

    let parser = Parser::new(&tokens);
    let ast = parser.run();

    if verbose {
        ast.pretty_print();
    }

    // let uirgen = UIRGen::new(ast);
    // let uir = uirgen.run();

    // if verbose {
    //     uir.pretty_print();
    // }

    // let sema = Sema::new(uir);
    // let tir = sema.run();

    // if verbose {
    //     tir.pretty_print();
    // }

    Ok(())
}

fn tests(verbose: bool) -> io::Result<()> {
    let paths = fs::read_dir("./test")?;
    let paths = paths.into_iter().flatten().collect::<Vec<fs::DirEntry>>();

    for (idx, path) in paths.into_iter().enumerate() {
        let path = path.path();

        if path.is_file() {
            println!("<=== Running test {} \"{}\" ===>", idx, path.display());
            compile(path, verbose)?;
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    env_logger::init();

    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Test { verbose }) => {
            tests(*verbose)?;
        }
        None => {}
    }

    Ok(())
}
