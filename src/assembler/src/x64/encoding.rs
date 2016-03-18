use super::consts::{R64, ErasedReg};
use super::consts::R64::*;
use self::Mod::*;
use self::RegOrOpExt::*;
use self::Scale::*;
use self::Addr::*;

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct REX(u8);

impl REX {
    pub fn encoding(self) -> u8 {
        self.0
    }

    pub fn none() -> Self {
        REX(0b01000000)
    }

    pub fn is_none(self) -> bool {
        self == REX::none()
    }

    pub fn w() -> Self {
        REX(REX::none().0 | (1 << 3))
    }
    pub fn r() -> Self {
        REX(REX::none().0 | (1 << 2))
    }
    pub fn x() -> Self {
        REX(REX::none().0 | (1 << 1))
    }
    pub fn b() -> Self {
        REX(REX::none().0 | (1 << 0))
    }

    pub fn or(self, other: REX) -> Self {
        REX(self.0 | other.0)
    }

    pub fn with_modrm(mut self, modrm: ModRM) -> Self {
        if modrm.reg.is_extended_reg() {
            self = self.or(REX::r());
        }
        if modrm.rm.is_extended() {
            self = self.or(REX::b());
        }
        self
    }

    pub fn with_sib(mut self, sib: SIB) -> Self {
        if sib.index.is_extended() {
            self = self.or(REX::x());
        }
        if sib.base.is_extended() {
            self = self.or(REX::b());
        }
        self
    }

    pub fn with_modrm_rm(mut self, rm: R64) -> Self {
        if rm.is_extended() {
            self = self.or(REX::b());
        }
        self
    }

    // Only emits when self is not NONE.
    pub fn emit<A: AsMut<Vec<u8>>>(&self, e: &mut A) {
        if !self.is_none() {
            e.as_mut().push(self.encoding())
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Mod {
    Indirect = 0,
    Indirect8 = 1,
    Indirect32 = 2,
    Direct = 3,

    // Indirect, with rm = bp: index * scale + disp32
    IndirectBP = 4,
}

impl Mod {
    fn encoding(self) -> u8 {
        (self as u8) & 0x3
    }
}

#[derive(Copy, Clone)]
pub enum RegOrOpExt {
    Reg(ErasedReg),
    OpExt(u8),
}

impl RegOrOpExt {
    pub fn reg<R>(r: R) -> Self where ErasedReg: From<R> {
        RegOrOpExt::Reg(ErasedReg::from(r))
    }

    pub fn lower_part(&self) -> u8 {
        match self {
            &Reg(r) => r.lower_part(),
            &OpExt(o) => o,
        }
    }

    pub fn is_extended_reg(&self) -> bool {
        match self {
            &Reg(r) => r.is_extended(),
            &OpExt(_) => false,
        }
    }
}

#[derive(Copy, Clone)]
pub struct ModRM {
    pub mod_: Mod,
    pub reg: RegOrOpExt,
    pub rm: R64,
}

impl ModRM {
    pub fn new(mod_: Mod, reg: RegOrOpExt, rm: R64) -> Self {
        ModRM {
            mod_: mod_,
            reg: reg,
            rm: rm,
        }
    }

    pub fn direct(reg: R64, rm: R64) -> Self {
        ModRM::new(Direct, RegOrOpExt::reg(reg), rm)
    }

    pub fn direct_opext(opext: u8, rm: R64) -> Self {
        ModRM::new(Direct, OpExt(opext), rm)
    }

    pub fn encoding(&self) -> u8 {
        ((self.mod_.encoding()) << 6) | (self.reg.lower_part() << 3) | self.rm.lower_part()
    }
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum Scale {
    S1 = 0,
    S2 = 1,
    S4 = 2,
    S8 = 3,
}

impl Scale {
    pub fn from_u8(u: u8) -> Self {
        match u {
            1 => S1,
            2 => S2,
            4 => S4,
            8 => S8,
            _ => panic!("Not a valid scale: {}", u),
        }
    }
}

#[derive(Copy, Clone)]
pub struct SIB {
    scale: Scale,
    index: R64,
    base: R64,
}

impl SIB {
    pub fn new(s: Scale, i: R64, b: R64) -> Self {
        SIB {
            scale: s,
            index: i,
            base: b,
        }
    }

    pub fn new_b(b: R64) -> Self {
        SIB::new(S1, rsp, b)
    }

    pub fn new_si(s: Scale, i: R64) -> Self {
        SIB::new(s, i, rbp)
    }

    pub fn encoding(self) -> u8 {
        ((self.scale as u8) << 6) | (self.index.lower_part() << 3) | self.base.lower_part()
    }
}

// Memory operand.

#[derive(Debug, Clone)]
pub enum Addr {
    B(R64),
    IS(R64, Scale),
    BIS(R64, R64, Scale),
    BD(R64, i32),
    BISD(R64, R64, Scale, i32),
    // | rip-relative: disp32(%rip)
    PcRel(i32),
}

impl Addr {
    pub fn is_index_scale(&self) -> bool {
        match self {
            &IS(..) => true,
            _ => false,
        }
    }

    pub fn is_pc_relative(&self) -> bool {
        match self {
            &PcRel(..) => true,
            _ => false,
        }
    }

    pub fn base(&self) -> Option<R64> {
        match self {
            &B(r) |
            &BIS(r, _, _) |
            &BD(r, _) |
            &BISD(r, _, _, _) => Some(r),
            &IS(_, _) => None,
            &PcRel(_) => Some(rbp),
        }
    }

    pub fn disp(&self) -> Option<i32> {
        match self {
            &B(_) |
            &IS(_, _) |
            &BIS(_, _, _) => None,
            &BD(_, d) |
            &BISD(_, _, _, d) |
            &PcRel(d) => Some(d),
        }
    }
}
