use crate::sema::types::TypeID;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(super) enum Def {
    Const(TypeID),
    Var(TypeID),
    Fn(TypeID),
    Type(TypeID),
}

#[derive(Debug, Clone)]
pub(super) struct Scope<'a> {
    definitions: HashMap<&'a str, Def>,
    parent: Option<&'a Scope<'a>>,
    children: Vec<Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub(super) fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            parent: None,
            children: vec![],
        }
    }

    pub(super) fn new_with_parent(parent: &'a Scope<'a>) -> Self {
        Self {
            definitions: HashMap::new(),
            parent: Some(parent),
            children: vec![],
        }
    }

    pub(super) fn push(&mut self, ident: &'a str, def: Def) {
        self.definitions.insert(ident, def);
    }

    pub(super) fn get(&self, ident: &'a str) -> Option<Def> {
        match self.definitions.get(ident) {
            Some(def) => Some(def.clone()),
            None => match self.parent {
                Some(parent) => parent.get(ident),
                None => None,
            },
        }
    }
}
