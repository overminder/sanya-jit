// LIR: Assembly-like IR to ease register allocation.

use c1::graph::{Graph, NodeIx};
use assembler::x64::{R64, Cond};

pub enum Lir {
    MovRR(LReg, LReg),
    MovRI(LReg, i64),
    LeaRRR(LReg, LReg, LReg),
    CmpRR(LReg, LReg),
}

// Branches.
pub enum LirB {
    Jcc(Cond, LBlockId, LBlockId),
    CallR(LReg),
    CallL(LBlockId),
    J(LBlockId),
    Ret,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct LBlockId(NodeIx);

pub struct LBlock {
    id: LBlockId,
    mids: Vec<Lir>,
    last: LirB,
}

pub struct LGraph {
    g: Graph<LBlock>,
}

impl LGraph {
    pub fn new() -> Self {
        LGraph {
            g: Graph::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LReg {
    Physical(R64),
    Virtual(u32),
}
