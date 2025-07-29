use std::collections::HashMap;

use derive_more::{Display, From, Into};
use typed_index_collections::TiVec;

use crate::ast::Ident;

#[derive(Debug, Default, Clone, Copy, Display, From, Into)]
pub struct Index(pub usize);

#[derive(Debug, Clone)]
pub struct Scope<'a, T: Clone> {
    parent: Option<Index>,
    data: HashMap<Ident<'a>, T>,
}

#[derive(Debug, Default, Clone)]
pub struct ScopeArena<'a, T>
where
    T: Clone,
{
    data: TiVec<Index, Scope<'a, T>>,
    current: Index,
}

impl<'a, T> ScopeArena<'a, T>
where
    T: Clone,
{
    pub fn new() -> Self {
        let mut data = TiVec::new();
        let current = data.push_and_get_key(Scope {
            parent: None,
            data: HashMap::new(),
        });

        Self { data, current }
    }

    fn get_index(&self, index: Index) -> &Scope<'a, T> {
        self.data.get(index).unwrap()
    }

    fn get_index_mut(&mut self, index: Index) -> &mut Scope<'a, T> {
        &mut self.data[index]
    }

    fn get_from_index(&self, index: Index, key: Ident<'a>) -> Option<T> {
        let current = self.get_index(index);
        match current.data.get(&key) {
            Some(value) => Some(value.clone()),
            None => match current.parent {
                Some(parent) => self.get_from_index(parent, key),
                None => None,
            },
        }
    }

    pub fn get(&self, key: Ident<'a>) -> Option<T> {
        self.get_from_index(self.current, key)
    }

    pub fn get_unchecked(&self, key: Ident<'a>) -> T {
        unsafe { self.get_from_index(self.current, key).unwrap_unchecked() }
    }

    pub fn push(&mut self, key: Ident<'a>, value: T) {
        let scope = self.get_index_mut(self.current);
        scope.data.insert(key, value);
    }

    pub fn down(&mut self) {
        self.current = self.data.push_and_get_key(Scope {
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

pub struct Iterator<'a, T>
where
    T: Clone,
{
    current_scope: Index,
    data_iter: std::collections::hash_map::Iter<'a, Ident<'a>, T>,
    arena: &'a ScopeArena<'a, T>,
}

impl<'a, T> std::iter::Iterator for Iterator<'a, T>
where
    T: Clone,
{
    type Item = (Ident<'a>, T);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((name, item)) = self.data_iter.next() {
            return Some((*name, item.clone()));
        }

        let current = self.arena.get_index(self.current_scope);
        match current.parent {
            Some(parent) => {
                let current = self.arena.get_index(parent);
                self.current_scope = parent;
                self.data_iter = current.data.iter();
                self.next()
            }
            None => None,
        }
    }
}
