use self::R64::*;

use std::mem;

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Width {
    B = 1,
    W = 2,
    D = 4,
    Q = 8,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct ErasedReg {
    pub repr: u8,
    pub width: Width,
}


impl ErasedReg {
    pub fn lower_part(self) -> u8 {
        self.repr & 0x7
    }

    pub fn is_extended(self) -> bool {
        self.repr & 0x8 == 0x8
    }

    pub fn is_like_sp(&self) -> bool {
        self.lower_part() == rsp.lower_part()
    }

    pub fn is_like_bp(&self) -> bool {
        self.lower_part() == rbp.lower_part()
    }
}

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

// XXX: Obviously duplicated. Consider generating them?
#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum R32 {
    eax = 0,
    ecx = 1,
    edx = 2,
    ebx = 3,

    esp = 4,
    ebp = 5,
    esi = 6,
    edi = 7,

    r8d = 8,
    r9d = 9,
    r10d = 10,
    r11d = 11,
    r12d = 12,
    r13d = 13,
    r14d = 14,
    r15d = 15,
}

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum R8 {
    al = 0,
    cl = 1,
    dl = 2,
    bl = 3,

    spl = 4,
    bpl = 5,
    sil = 6,
    dil = 7,

    r8b = 8,
    r9b = 9,
    r10b = 10,
    r11b = 11,
    r12b = 12,
    r13b = 13,
    r14b = 14,
    r15b = 15,
}

impl R64 {
    pub fn erased(self) -> ErasedReg {
        ErasedReg {
            repr: self as u8,
            width: Width::Q,
        }
    }

    pub fn to_r32(self) -> R32 {
        R32::from_u8(self as u8)
    }

    pub fn lower_part(self) -> u8 {
        self.erased().lower_part()
    }

    pub fn is_extended(self) -> bool {
        self.erased().is_extended()
    }
}

impl R32 {
    fn from_u8(repr: u8) -> Self {
        unsafe { mem::transmute(repr) }
    }

    pub fn erased(self) -> ErasedReg {
        ErasedReg {
            repr: self as u8,
            width: Width::D,
        }
    }
}

impl From<R64> for ErasedReg {
    fn from(r: R64) -> Self {
        r.erased()
    }
}

impl From<R32> for ErasedReg {
    fn from(r: R32) -> Self {
        r.erased()
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

