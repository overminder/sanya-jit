// LIR: Assembly-like IR to ease register allocation.

use assembler::x64::R64;

pub enum Lir {
    MovRR(LReg, LReg),
    AddRR(LReg, LReg),
    Ret,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct LBlockId(pub usize);

pub struct LBlock {
    id: LBlockId,
    irs: Vec<Lir>,
}

pub struct LGraph {
    blocks: Vec<LBlock>,
    // Enough for 16 vertices: 32 * 8 = 16 ^ 2.
    small_matrix: [u8; 32],
}

impl LGraph {
    pub fn new() -> Self {
        LGraph {
            blocks: vec![],
            small_matrix: [0; 32],
        }
    }

    pub fn add(&mut self, irs: Vec<Lir>, entries: &[LBlockId], exits: &[LBlockId]) -> LBlockId {
        let bid = self.next_block_id();
        let b = LBlock {
            id: bid,
            irs: irs,
        };
    }
}

pub enum LReg {
    Physical(R64),
    Virtual(u32),
}
