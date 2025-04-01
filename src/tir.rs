pub enum Instruction {
}

pub enum Terminator {
}

type Loc = usize;
type Ref = usize;

pub struct BasicBlock {
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

pub struct CFG {
    blocks: Vec<BasicBlock>,
    register_count: usize,
}

pub struct TIR {
}

impl TIR {
    pub fn new() -> Self {
        Self {}
    }

    pub fn pretty_print(&self) {
        todo!()
    }
}
