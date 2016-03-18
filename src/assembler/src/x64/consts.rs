use self::R64::*;

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum R64 {
    rax = 0,
    rcx = 1,
    rdx = 2,
    rbx = 3,

    rsp = 4,
    rbp = 5,
    rsi = 6,
    rdi = 7,

    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
}

impl R64 {
    pub fn lower_part(self) -> u8 {
        (self as u8) & 0x7
    }

    pub fn is_extended(self) -> bool {
        (self as u8) & 0x8 == 0x8
    }

    pub fn is_rsp_or_r12(&self) -> bool {
        self.lower_part() == rsp.lower_part()
    }

    pub fn is_rbp_or_r13(&self) -> bool {
        self.lower_part() == rbp.lower_part()
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

