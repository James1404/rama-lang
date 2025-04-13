use std::path::Path;

pub struct Metadata<'a> {
    pub name: &'a str,
    pub path: &'a Path,
}
