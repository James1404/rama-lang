use std::ffi::{OsStr, OsString};

pub struct Metadata<'a> {
    pub filename: &'a OsStr,
    pub path: &'a str,
}
