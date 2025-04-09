use std::collections::HashMap;

use derive_more::{Display, From, Into};
use typed_index_collections::TiVec;

#[derive(Debug, Clone, Copy, Display, From, Into)]
pub struct Index(pub usize);

#[derive(Debug, Clone)]
pub struct Scope<'a, T: Clone> {
    parent: Option<Index>,
    data: HashMap<&'a str, T>,
}

pub struct ScopeArena<'a, T: Clone> {
    data: TiVec<Index, Scope<'a, T>>,
    current: Index,
}

impl<'a, T: Clone> ScopeArena<'a, T> {
    pub fn new() -> Self {
        let mut data = TiVec::new();
        let current = data.push_and_get_key(Scope {
            parent: None,
            data: HashMap::new(),
        });

        Self { data, current }
    }

    fn get_index(&self, index: Index) -> &Scope<T> {
        self.data.get(index).unwrap()
    }

    fn get_index_mut(&mut self, index: Index) -> &mut Scope<'a, T> {
        &mut self.data[index]
    }

    fn get_from_index(&self, index: Index, key: &'a str) -> Option<T> {
        let current = self.get_index(index);
        match current.data.get(key) {
            Some(value) => Some(value.clone()),
            None => match current.parent {
                Some(parent) => self.get_from_index(parent, key),
                None => None,
            },
        }

    }

    pub fn get(&self, key: &'a str) -> Option<T> {
        self.get_from_index(self.current, key)
    }

    pub fn push(&mut self, key: &'a str, value: T) {
        let scope = self.get_index_mut(self.current);
        scope.data.insert(key, value);
    }

    pub fn down(&mut self) {
        self.data.push_and_get_key(Scope {
            parent: Some(self.current),
            data: HashMap::new(),
        });
    }

    pub fn up(&mut self) {
        let current = self.get_index(self.current);
        let Some(parent) = current.parent else {
            panic!();
        };
        self.current = parent;
    }
}
