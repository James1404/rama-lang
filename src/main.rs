use std::{fs, io, path::Path};

#[macro_use]
extern crate derive_more;

//use backend::Backend;
use clap_complete::{Generator, Shell, generate};
use lexer::Lexer;
use metadata::Metadata;
use parser::Parser;

use clap::{Command, Parser as ClapParser, Subcommand};
use sema::{Sema, SemaError};

mod ast;
//mod backend;
mod lexer;
mod metadata;
mod parser;
mod sema;
mod rair;
mod typed_ast;
mod types;
mod valuescope;

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

    // #[arg(short, long, value_enum, default_value_t = Backend::C)]
    // backend: Backend,
}

#[derive(Subcommand)]
enum Commands {
    Test,
    Compile {
        file: String,
    },
}

fn compile<P>(path: P, cli: &Cli) -> io::Result<()>
where
    P: AsRef<Path> + Clone,
{
    let src = std::fs::read_to_string(path.clone())?;

    let metadata = Metadata {
        name: path.as_ref().file_stem().unwrap().to_str().unwrap(),
        path: path.as_ref(),
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
            err.error();
            return Ok(());
        }
    };
    let astview = ast.to_view();

    if cli.print_ast {
        astview.pretty_print();
    }

    let sema = Sema::new(astview);
    let (tast, errors) = sema.run();

    for error in &errors {
        match error {
            SemaError::InvalidTerm(term) => {
                println!("Error: [InvalidTerm] {:#?}", astview.get(*term))
            }
            SemaError::Err(msg) => println!("Error: {}", msg),
            _ => println!("Error: {}", error),
        }
    }
    if !errors.is_empty() {
        return Ok(());
    }

    //tast.print();

    let builder = rair::Builder::new(tast);
    let ril = builder.build();

    ril.pretty_print();

    // println!("<== Starting CodeGen ==>");
    // match backend::compile(tir, metadata, cli.backend) {
    //     Ok(_) => {},
    //     Err(err) => error!("{}", err),
    // }

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
        Some(Commands::Compile { file }) => {
            compile(file, &cli)?;
        }
        None => {}
    }

    Ok(())
}
