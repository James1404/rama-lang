mod ast;
mod lexer;
mod metadata;
mod parser;
mod ty;

use std::io;
use std::path::Path;

use lexer::Lexer;
use metadata::Metadata;
use parser::Parser;

pub struct Output {
    pub tokens: bool,
    pub ast: bool,
}

pub struct Config {
    pub output: Output
}

pub fn compile_file(path: impl AsRef<Path> + Clone, config: Config) -> io::Result<()> {
    let src = std::fs::read_to_string(path.clone())?;

    let metadata = Metadata::new(path.as_ref(), Path::new("build/"))?;

    let lexer = Lexer::new(&src);
    let tokens = lexer.run();

    if config.output.tokens {
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

    if config.output.ast {
        println!("{ast}");
    }

    Ok(())
}
