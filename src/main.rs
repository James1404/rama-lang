use std::{fs, io};

use clap::{Command, Parser as ClapParser, Subcommand};
use clap_complete::{Generator, Shell, generate};
use rama_lang::{compile_file, Config, Output};

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

impl From<&Cli> for Config {
    fn from(value: &Cli) -> Self {
        Self {
            output: Output {
                ast: value.print_ast,
                tokens: value.print_tokens,
            },
        }
    }
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

fn tests(cli: &Cli) -> io::Result<()> {
    let paths = fs::read_dir("./test")?;
    let paths = paths.into_iter().flatten().collect::<Vec<fs::DirEntry>>();

    for (idx, path) in paths.into_iter().enumerate() {
        let path = path.path();

        if path.is_file() {
            println!("<=== Running test {} \"{}\" ===>", idx, path.display());
            compile_file(&path, cli.into())?;
            println!("<=== Ending Test \"{}\" ===>", path.display());
            println!();
        }
    }

    Ok(())
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Tests) => {
            tests(&cli)?;
        }
        Some(Commands::Compile { file }) => {
            compile_file(file, (&cli).into())?;
        }
        None => {}
    }

    Ok(())
}
