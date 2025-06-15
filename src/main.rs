mod ast;
mod lexer;
mod metadata;
mod parser;
mod scope;
mod sema;
mod ty;
// mod tast;
mod ril;
// mod backend;

use std::{fs, io, path::Path};

#[macro_use]
extern crate derive_more;

use lexer::Lexer;
use metadata::Metadata;
use parser::Parser;

use clap::{Command, Parser as ClapParser, Subcommand};
use clap_complete::{Generator, Shell, generate};
use sema::{Sema, SemaError};

#[derive(ClapParser)]
#[command(version, about, author, long_about = "The Rama Compiler")]
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
    // #[arg(short, long, value_enum, default_value_t = Backend::LLVM)]
    // backend: Backend,
}

#[derive(Subcommand)]
enum Commands {
    Tests,
    Compile { file: String },
}

fn compile<P>(path: P, cli: &Cli) -> io::Result<()>
where
    P: AsRef<Path> + Clone,
{
    let src = std::fs::read_to_string(path.clone())?;

    let metadata = Metadata::new(path.as_ref(), Path::new("build/"))?;

    let lexer = Lexer::new(&src);
    let tokens = lexer.run();

    if cli.print_tokens {
        println!("<== Printing Tokens ==>");
        for tok in &tokens {
            println!("{}: \"{}\"", Into::<&'static str>::into(tok.ty), tok.text);
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

    if cli.print_ast {
        println!("{ast}");
    }

    let sema = Sema::new(ast);
    let (tast, errors) = sema.run();

    for error in &errors {
        match error {
            SemaError::InvalidTerm(term) => {
                println!("Error: [InvalidTerm] {:#?}", term)
            }
            SemaError::Err(msg) => println!("Error: {}", msg),
            _ => println!("Error: {}", error),
        }
    }
    if !errors.is_empty() {
        return Ok(());
    }

    //    let builder = ril::Builder::new(&tast);
    //    let ril = builder.build();
    //
    //    ril.pretty_print();
    //
    //    println!("<== Starting CodeGen ==>");
    //    match backend::compile(ril, metadata, cli.backend) {
    //        Ok(_) => {}
    //        Err(err) => eprintln!("{}", err),
    //    }
    //
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
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Tests) => {
            tests(&cli)?;
        }
        Some(Commands::Compile { file }) => {
            compile(file, &cli)?;
        }
        None => {}
    }

    Ok(())
}
