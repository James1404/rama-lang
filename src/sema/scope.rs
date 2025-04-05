use crate::sema::types::TypeID;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(in crate::sema) struct Scope<'a> {
    definitions: HashMap<&'a str, TypeID>,
    parent: Option<&'a Scope<'a>>,
    children: Vec<Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub(in crate::sema) fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            parent: None,
            children: vec![],
        }
    }

    pub(in crate::sema) fn new_with_parent(parent: &'a Scope<'a>) -> Self {
        Self {
            definitions: HashMap::new(),
            parent: Some(parent),
            children: vec![],
        }
    }

    pub(in crate::sema) fn push(&mut self, ident: &'a str, ty: TypeID) {
        self.definitions.insert(ident, ty);
    }

    pub(in crate::sema) fn get(&self, ident: &'a str) -> Option<TypeID> {
        match self.definitions.get(ident) {
            Some(ty) => Some(*ty),
            None => match self.parent {
                Some(parent) => parent.get(ident),
                None => None,
            },
        }
    }
}
