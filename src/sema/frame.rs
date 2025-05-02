use crate::ty::TypeID;

#[derive(Debug, Clone, Copy)]
pub(super) struct Frame {
    pub(super) return_type: Option<TypeID>,
}
