use crate::ty::TypeRef;

#[derive(Debug, Clone)]
pub(super) struct Frame<'a> {
    pub(super) return_type: Option<TypeRef<'a>>,
}
