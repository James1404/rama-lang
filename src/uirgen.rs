#![allow(dead_code)]

use crate::{ast::AST, uir::UIR};

pub struct UIRGen<'a> {
    ast: AST<'a>,
    uir: UIR,
}

impl<'a> UIRGen<'a> {
    pub fn new(ast: AST<'a>) -> Self {
        Self {
            ast,
            uir: UIR::new(),
        }
    }

    pub fn run(self) -> UIR {
        self.uir
    }
}
