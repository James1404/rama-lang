use crate::{tir::TIR, uir::UIR};

pub struct Sema {
    uir: UIR,
    tir: TIR,
}

impl Sema {
    pub fn new(uir: UIR) -> Self {
        Self {
            uir,
            tir: TIR::new(),
        }
    }

    pub fn run(self) -> TIR {
        self.tir
    }
}
