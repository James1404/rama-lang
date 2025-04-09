use std::ffi::OsStr;

pub struct Metadata<'a> {
    pub filename: &'a OsStr,
    pub path: &'a str,
}
