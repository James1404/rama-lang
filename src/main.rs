use std::{fmt::Debug, fs, io, path::Path};

use clap_complete::{Generator, Shell, generate};
use lexer::Lexer;
use log::error;
use parser::Parser;

use clap::{Command, Parser as ClapParser, Subcommand};
use sema::{Sema, SemaError};

mod ast;
mod backend;
mod lexer;
mod parser;
mod sema;
mod tir;
mod typed_ast;
mod types;

#[derive(ClapParser)]
#[command(version, about, author, long_about = "A small WIP Compiler")]
#[command(propagate_version = true)]
#[command(name = "completion-derive")]
struct Cli {
    #[arg(long = "generate", value_enum)]
    generator: Option<Shell>,
    #[command(subcommand)]
    command: Option<Commands>,
    
    #[arg(long)]
    print_tokens: bool,
    #[arg(long)]
    print_ast: bool,
}

#[derive(Subcommand)]
enum Commands {
    Test,
}

fn compile<P>(path: P, cli: &Cli) -> io::Result<()>
where
    P: AsRef<Path>,
{
    let src = std::fs::read_to_string(path)?;

    let lexer = Lexer::new(&src);
    let tokens = lexer.run();

    if cli.print_tokens {
        println!("<== Printing Tokens ==>");
        for tok in &tokens {
            println!("{}: \"{}\"", Into::<&'static str>::into(&tok.ty), tok.text);
        }
        println!()
    }

    let parser = Parser::new(&tokens);
    let ast = match parser.run() {
        Ok(ast) => ast,
        Err(err) => {
            error!(
                "[{}; {}] {}",
                err.token.pos.line, err.token.pos.start, err.msg
            );
            return Ok(());
        }
    };
    let astview = ast.to_view();

    if cli.print_ast {
        astview.pretty_print();
    }

    let mut sema = Sema::new(astview);
    let (tast, errors) = sema.run();

    for error in errors {
        println!("Error: {}", error);
    }

    tast.print();

    let mut backend = backend::llvm::Codegen::new(tast);
    backend.run();

    Ok(())
}

fn tests(cli: &Cli) -> io::Result<()> {
    let paths = fs::read_dir("./test")?;
    let paths = paths.into_iter().flatten().collect::<Vec<fs::DirEntry>>();

    for (idx, path) in paths.into_iter().enumerate() {
        let path = path.path();

        if path.is_file() {
            println!("<=== Running test {} \"{}\" ===>", idx, path.display());
            compile(&path, cli)?;
            println!("<=== Ending Test \"{}\" ===>", path.display());
            println!();
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
        Some(Commands::Test) => {
            tests(&cli)?;
        }
        None => {}
    }

    Ok(())
}
