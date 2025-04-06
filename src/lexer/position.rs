#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub current: usize,
    pub start: usize,
    pub line: usize,
    pub offset: usize,
}
