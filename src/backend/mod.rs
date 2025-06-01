use clap::ValueEnum;
use std::{fmt::Display, io};
use thiserror::Error;

use crate::{metadata::Metadata, ril::RIL};

//mod c;
mod llvm;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Backend {
    LLVM,
    //C,
}

#[derive(Debug, From, Error)]
pub enum Error {
    #[error("IO Error: {_0}")]
    IOError(io::Error),
    #[error("CodeGen Error: {msg}")]
    CompileError { msg: String },
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn compile<'a>(ril: RIL<'a, 'a>, metadata: Metadata<'a>, backend: Backend) -> Result<()> {
    match backend {
        Backend::LLVM => {
            llvm::compile(ril, metadata);
            Ok(())
        } // Backend::C => match c::compile(ril, metadata) {
          //     Err(err) => Err(Error::IOError(err)),
          //     Ok(_) => Ok(()),
          // },
    }
}
