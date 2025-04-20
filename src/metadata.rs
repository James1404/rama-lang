use std::{io, path::Path};

pub struct Metadata<'a> {
    pub name: &'a str,
    pub path: &'a Path,
    pub outdir: &'a Path,
}

impl<'a> Metadata<'a> {
    pub fn new(path: &'a Path, outdir: &'a Path) -> io::Result<Self> {
        let name = path.file_stem().unwrap().to_str().unwrap();

        std::fs::create_dir_all(outdir)?;

        Ok(Self { name, path, outdir })
    }
}
