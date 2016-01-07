use self::R64::*;

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum R64 {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,

    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,

    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

impl R64 {
    pub fn lower_part(self) -> u8 {
        (self as u8) & 0x7
    }

    pub fn is_extended(self) -> bool {
        (self as u8) & 0x8 == 0x8
    }

    pub fn is_rsp_or_r12(&self) -> bool {
        self.lower_part() == RSP.lower_part()
    }

    pub fn is_rbp_or_r13(&self) -> bool {
        self.lower_part() == RBP.lower_part()
    }
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Cond {
    E = 4,
    NE = 5,
    L = 12,
    GE = 13,
    LE = 14,
    G = 15,
}

impl Cond {
    pub fn inverse(self) -> Self {
        use self::Cond::*;

        match self {
            E => NE,
            NE => E,
            L => GE,
            LE => G,
            G => LE,
            GE => L,
        }
    }
}

