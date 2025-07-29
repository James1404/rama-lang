use super::visit::{self, Visit};

struct Printer {}

impl<'a> Visit<'a> for Printer {
    fn visit_ident(&mut self, value: Ident<'a>) {
        visit::visit_ident(self, value);
    }
}
