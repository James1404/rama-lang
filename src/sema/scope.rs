use crate::{types::TypeID, valuescope::ScopeArena};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(super) enum Def {
    Const(TypeID),
    Var(TypeID),
    Fn(TypeID),
    Type(TypeID),
}

pub type Scope<'a> = ScopeArena<'a, Def>;
