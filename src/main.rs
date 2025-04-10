use std::{fs, io, path::Path};

#[macro_use]
extern crate derive_more;

use clap_complete::{Generator, Shell, generate};
use lexer::Lexer;
use log::error;
use metadata::Metadata;
use parser::Parser;

use clap::{Command, Parser as ClapParser, Subcommand};
use sema::Sema;

mod metadata;
mod lexer;
mod ast;
mod parser;
mod sema;
mod types;
mod valuescope;
mod typed_ast;
mod tir;
mod tirgen;
mod backend;

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
    P: AsRef<Path> + Clone,
{
    let src = std::fs::read_to_string(path.clone())?;

    let fullpath = path.as_ref().to_str().unwrap();
    let metadata = Metadata {
        filename: path.as_ref().file_name().unwrap(),
        path: fullpath,
    };

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
                err.token.pos.line, err.token.pos.offset, err.msg
            );
            return Ok(());
        }
    };
    let astview = ast.to_view();

    if cli.print_ast {
        astview.pretty_print();
    }

    let sema = Sema::new(astview);
    let (tir, errors) = sema.run();

    for error in &errors {
        println!("Error: {}", error);
    }
    if !errors.is_empty() {
        return Ok(());
    }

    tir.pretty_print();

    println!("<== Starting CodeGen ==>");
    let backend = backend::llvm::Codegen::new(tir, metadata);
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
