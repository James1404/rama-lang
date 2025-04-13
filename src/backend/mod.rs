use std::{fmt::Display, io};

use clap::ValueEnum;

use crate::{metadata::Metadata, tir::TIR};

mod c;
mod llvm;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Backend {
    LLVM,
    C,
}

#[derive(Debug, From, Error)]
pub enum Error {
    IOError(io::Error),
    CompileError { msg: String },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::IOError(err) => write!(f, "{}", err),
            Error::CompileError { msg} => write!(f, "{}", msg),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn compile<'a>(tir: TIR<'a>, metadata: Metadata<'a>, backend: Backend) -> Result<()> {
    match backend {
        Backend::LLVM => {
            let backend = llvm::Codegen::new(tir, metadata);
            backend.run();
            Ok(())
        }
        Backend::C => match c::compile(tir, metadata) {
            Err(err) => Err(Error::IOError(err)),
            Ok(_) => Ok(()),
        },
    }
}
