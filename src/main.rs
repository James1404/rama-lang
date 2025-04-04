use std::{fs, io, path::Path};

use clap_complete::{Generator, Shell, generate};
use lexer::Lexer;
use log::error;
use parser::Parser;

use clap::{Command, Parser as ClapParser, Subcommand};
use sema::{Sema, SemaError};

mod ast;
mod lexer;
mod parser;
mod sema;
mod tir;
mod tokens;

#[derive(ClapParser)]
#[command(version, about, author, long_about = "A small WIP Compiler")]
#[command(propagate_version = true)]
#[command(name = "completion-derive")]
struct Cli {
    #[arg(long = "generate", value_enum)]
    generator: Option<Shell>,
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
    let astview = ast.to_view();

    if verbose {
        astview.pretty_print();
    }

    let mut sema = Sema::new(astview);
    let errors = sema.run();

    for error in errors {
        match error {
            SemaError::InvalidTerm(term) => {
                error!("Invalid term: {:?}", astview.get(term));
            }
            err => error!("{:?}", err),
        }
    }

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

#[allow(dead_code)]
fn print_completions<G: Generator>(generator: G, cmd: &mut Command) {
    generate(
        generator,
        cmd,
        cmd.get_name().to_string(),
        &mut io::stdout(),
    );
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
