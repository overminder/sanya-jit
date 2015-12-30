pub mod traits;
mod tests;

use self::traits::*;

use std::ops::{Add, Mul};

/// Mostly from dart/runtime/vm/assembler_x64.cc
/// And http://wiki.osdev.org/X86-64_Instruction_Encoding
/// And http://www.felixcloutier.com/x86
/// And pypy/rpython/jit/backend/x86/rx86.py
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

    fn is_rsp_or_r12(&self) -> bool {
        self.lower_part() == R64::RSP.lower_part()
    }

    fn is_rbp_or_r13(&self) -> bool {
        self.lower_part() == R64::RBP.lower_part()
    }
}

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
struct REX(u8);

impl REX {
    fn encoding(self) -> u8 {
        self.0
    }

    fn none() -> Self {
        REX(0b01000000)
    }

    fn is_none(self) -> bool {
        self == REX::none()
    }

    fn w() -> Self {
        REX(REX::none().0 | (1 << 3))
    }
    fn r() -> Self {
        REX(REX::none().0 | (1 << 2))
    }
    fn x() -> Self {
        REX(REX::none().0 | (1 << 1))
    }
    fn b() -> Self {
        REX(REX::none().0 | (1 << 0))
    }

    fn or(self, other: REX) -> Self {
        REX(self.0 | other.0)
    }

    fn with_modrm(mut self, modrm: ModRM) -> Self {
        if modrm.reg.is_extended_reg() {
            self = self.or(REX::r());
        }
        if modrm.rm.is_extended() {
            self = self.or(REX::b());
        }
        self
    }

    fn with_sib(mut self, sib: SIB) -> Self {
        if sib.index.is_extended() {
            self = self.or(REX::x());
        }
        if sib.base.is_extended() {
            self = self.or(REX::b());
        }
        self
    }

    fn with_modrm_rm(mut self, rm: R64) -> Self {
        if rm.is_extended() {
            self = self.or(REX::b());
        }
        self
    }

    // Only emits when self is not NONE.
    fn emit(&self, e: &mut Emit) {
        if !self.is_none() {
            e.write_byte(self.encoding())
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Mod {
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
enum RegOrOpExt {
    Reg(R64),
    OpExt(u8),
}

impl RegOrOpExt {
    fn lower_part(&self) -> u8 {
        match self {
            &RegOrOpExt::Reg(r) => r.lower_part(),
            &RegOrOpExt::OpExt(o) => o,
        }
    }

    fn is_extended_reg(&self) -> bool {
        match self {
            &RegOrOpExt::Reg(r) => r.is_extended(),
            &RegOrOpExt::OpExt(_) => false,
        }
    }
}

#[derive(Copy, Clone)]
struct ModRM {
    mod_: Mod,
    reg: RegOrOpExt,
    rm: R64,
}

impl ModRM {
    fn new(mod_: Mod, reg: RegOrOpExt, rm: R64) -> Self {
        ModRM {
            mod_: mod_,
            reg: reg,
            rm: rm,
        }
    }

    fn direct(reg: R64, rm: R64) -> Self {
        ModRM::new(Mod::Direct, RegOrOpExt::Reg(reg), rm)
    }

    fn direct_opext(opext: u8, rm: R64) -> Self {
        ModRM::new(Mod::Direct, RegOrOpExt::OpExt(opext), rm)
    }

    fn encoding(&self) -> u8 {
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
    fn from_u8(u: u8) -> Self {
        use self::Scale::*;
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
struct SIB {
    scale: Scale,
    index: R64,
    base: R64,
}

impl SIB {
    fn new(s: Scale, i: R64, b: R64) -> Self {
        SIB {
            scale: s,
            index: i,
            base: b,
        }
    }

    fn new_b(b: R64) -> Self {
        SIB::new(Scale::S1, R64::RSP, b)
    }

    fn new_si(s: Scale, i: R64) -> Self {
        SIB::new(s, i, R64::RBP)
    }

    fn encoding(self) -> u8 {
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
    fn is_index_scale(&self) -> bool {
        match self {
            &Addr::IS(..) => true,
            _ => false,
        }
    }

    fn is_pc_relative(&self) -> bool {
        match self {
            &Addr::PcRel(..) => true,
            _ => false,
        }
    }

    fn base(&self) -> Option<R64> {
        use self::Addr::*;

        match self {
            &B(r) |
            &BIS(r, _, _) |
            &BD(r, _) |
            &BISD(r, _, _, _) => Some(r),
            &IS(_, _) => None,
            &PcRel(_) => Some(R64::RBP),
        }
    }

    fn disp(&self) -> Option<i32> {
        use self::Addr::*;

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

impl Add for R64 {
    type Output = Addr;
    fn add(self, rhs: R64) -> Self::Output {
        Addr::BIS(self, rhs, Scale::S1)
    }
}

impl Add<i32> for R64 {
    type Output = Addr;
    fn add(self, rhs: i32) -> Self::Output {
        Addr::BD(self, rhs)
    }
}

impl Add<Addr> for R64 {
    type Output = Addr;
    fn add(self, rhs: Addr) -> Self::Output {
        use self::Addr::*;

        if let IS(index, scale) = rhs {
            BIS(self, index, scale)
        } else {
            panic!("{:?} + {:?} is not defined.", self, rhs)
        }
    }
}

impl Mul<u8> for R64 {
    type Output = Addr;
    fn mul(self, rhs: u8) -> Self::Output {
        Addr::IS(self, Scale::from_u8(rhs))
    }
}

pub fn is_imm8(i: i32) -> bool {
    i < 128 && i >= -128
}

// Emission utilties.

fn emit_rm64dst_r64src(buf: &mut Emit, opcode: u8, dst: R64, src: R64) {
    let modrm = ModRM::direct(src, dst);
    let rex = REX::w().with_modrm(modrm);
    buf.write_bytes(&[rex.encoding(), opcode, modrm.encoding()]);
}

// 81 /$opext i32
fn emit_arith_rm64_i32(buf: &mut Emit, opext: u8, dst: R64, src: i32) {
    assert!(opext < 8);
    let modrm = ModRM::direct_opext(opext, dst);
    let rex = REX::w().with_modrm(modrm);
    buf.write_bytes(&[rex.encoding(), 0x81, modrm.encoding()]);
    buf.write_i32(src);
}

// opcode /r
fn emit_rm64_r(buf: &mut Emit, opcode: u8, dst: R64, rex: REX) {
    rex.with_modrm_rm(dst).emit(buf);
    buf.write_byte(opcode | dst.lower_part());
}

fn emit_rm64_opext(buf: &mut Emit, rex: REX, opcode: u8, opext: u8, rm: R64) {
    let modrm = ModRM::direct_opext(opext, rm);
    rex.with_modrm_rm(rm).emit(buf);
    buf.write_byte(opcode);
    buf.write_byte(modrm.encoding());
}

// XXX: Refactor this.
fn emit_addr(buf: &mut Emit, mut rex: REX, opcode: u8, reg: RegOrOpExt, op: &Addr) {
    use self::Addr::*;

    let mut modrm;
    let mut mb_sib = None;
    let mut mb_disp = None;

    let mb_base = op.base();
    if let Some(base) = mb_base {
        if base.is_rsp_or_r12() {
            // rsp | r12: SIB will be in use.
            mb_sib = Some(SIB::new_b(base));
        }
        modrm = ModRM::new(Mod::Indirect, reg, base);
    } else {
        // mod = 00 & rm = RBP: index * scale + disp32
        assert!(op.is_index_scale());
        modrm = ModRM::new(Mod::IndirectBP, reg, R64::RSP);
        mb_disp = Some(0);
    }

    if let Some(disp) = op.disp() {
        if op.is_pc_relative() {
            // Must be disp32: mod = 0b00
        } else if is_imm8(disp) {
            modrm.mod_ = Mod::Indirect8;
        } else {
            modrm.mod_ = Mod::Indirect32;
        }
        mb_disp = Some(disp);
    } else {
        if mb_base.map(|b| b.is_rbp_or_r13()).unwrap_or(false) {
            // bp / r13 can't use Mod::Indirect and thus need to use a imm8
            // as the displacment.
            modrm.mod_ = Mod::Indirect8;
            mb_disp = Some(0);
        }
    }

    match op {
        &B(_) |
        &BD(_, _) |
        &PcRel(_) => {}
        &BIS(_, index, scale) |
        &BISD(_, index, scale, _) => {
            mb_sib = Some(SIB::new(scale, index, mb_base.unwrap()));
            modrm.rm = R64::RSP;
        }
        &IS(index, scale) => {
            mb_sib = Some(SIB::new_si(scale, index));
        }
    }

    rex = rex.with_modrm(modrm);
    mb_sib.map(|sib| rex = rex.with_sib(sib));
    rex.emit(buf);
    buf.write_byte(opcode);
    buf.write_byte(modrm.encoding());
    mb_sib.map(|sib| buf.write_byte(sib.encoding()));
    if modrm.mod_ == Mod::Indirect8 {
        buf.write_byte(mb_disp.unwrap() as u8);
    } else if modrm.mod_ == Mod::Indirect32 || modrm.mod_ == Mod::IndirectBP || op.is_pc_relative() {
        buf.write_i32(mb_disp.unwrap());
    } else {
        assert_eq!(Mod::Indirect, modrm.mod_);
    }
}

// Code emission.

impl EmitPush<R64> for Emit {
    fn push(&mut self, op: R64) -> &mut Self {
        REX::none().with_modrm_rm(op).emit(self);
        self.write_byte(0x50 | op.lower_part());
        self
    }
}

impl EmitPush<i32> for Emit {
    fn push(&mut self, op: i32) -> &mut Self {
        self.write_byte(0x68);
        self.write_i32(op);
        self
    }
}

impl<'a> EmitPush<&'a Addr> for Emit {
    fn push(&mut self, op: &Addr) -> &mut Self {
        emit_addr(self, REX::none(), 0xFF, RegOrOpExt::OpExt(6), op);
        self
    }
}

impl EmitPop<R64> for Emit {
    fn pop(&mut self, op: R64) -> &mut Self {
        REX::none().with_modrm_rm(op).emit(self);
        self.write_byte(0x58 | op.lower_part());
        self
    }
}

impl<'a> EmitPop<&'a Addr> for Emit {
    fn pop(&mut self, op: &Addr) -> &mut Self {
        emit_addr(self, REX::none(), 0x8F, RegOrOpExt::OpExt(0), op);
        self
    }
}


impl EmitArith<R64, R64> for Emit {
    fn add(&mut self, dst: R64, src: R64) -> &mut Self {
        emit_rm64dst_r64src(self, 0x01, dst, src);
        self
    }

    fn sub(&mut self, dst: R64, src: R64) -> &mut Self {
        emit_rm64dst_r64src(self, 0x29, dst, src);
        self
    }

    fn cmp(&mut self, dst: R64, src: R64) -> &mut Self {
        emit_rm64dst_r64src(self, 0x39, dst, src);
        self
    }
}

impl EmitArith<R64, i32> for Emit {
    fn add(&mut self, dst: R64, src: i32) -> &mut Self {
        emit_arith_rm64_i32(self, 0, dst, src);
        self
    }

    fn sub(&mut self, dst: R64, src: i32) -> &mut Self {
        emit_arith_rm64_i32(self, 5, dst, src);
        self
    }

    fn cmp(&mut self, dst: R64, src: i32) -> &mut Self {
        emit_arith_rm64_i32(self, 7, dst, src);
        self
    }
}

impl<'a> EmitArith<R64, &'a Addr> for Emit {
    fn add(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x03, RegOrOpExt::Reg(dst), src);
        self
    }

    fn sub(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x2B, RegOrOpExt::Reg(dst), src);
        self
    }

    fn cmp(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x3B, RegOrOpExt::Reg(dst), src);
        self
    }
}

impl EmitMov<R64, R64> for Emit {
    fn mov(&mut self, dst: R64, src: R64) -> &mut Self {
        emit_rm64dst_r64src(self, 0x89, dst, src);
        self
    }
}

impl EmitMov<R64, i64> for Emit {
    fn mov(&mut self, dst: R64, src: i64) -> &mut Self {
        emit_rm64_r(self, 0xB8, dst, REX::w());
        self.write_i64(src);
        self
    }
}

impl<'a> EmitMov<R64, &'a Addr> for Emit {
    fn mov(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x8B, RegOrOpExt::Reg(dst), src);
        self
    }
}

impl<'a> EmitMov<&'a Addr, R64> for Emit {
    fn mov(&mut self, dst: &Addr, src: R64) -> &mut Self {
        emit_addr(self, REX::w(), 0x89, RegOrOpExt::Reg(src), dst);
        self
    }
}

impl<'a> EmitLea<R64, &'a Addr> for Emit {
    fn lea(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x8D, RegOrOpExt::Reg(dst), src);
        self
    }
}

impl EmitBranch<R64> for Emit {
    fn jmp(&mut self, op: R64) -> &mut Self {
        emit_rm64_opext(self, REX::none(), 0xFF, 4, op);
        self
    }

    fn call(&mut self, op: R64) -> &mut Self {
        emit_rm64_opext(self, REX::none(), 0xFF, 2, op);
        self
    }
}

impl<'a> EmitBranch<&'a Addr> for Emit {
    fn jmp(&mut self, op: &Addr) -> &mut Self {
        emit_addr(self, REX::none(), 0xFF, RegOrOpExt::OpExt(4), op);
        self
    }

    fn call(&mut self, op: &Addr) -> &mut Self {
        emit_addr(self, REX::none(), 0xFF, RegOrOpExt::OpExt(2), op);
        self
    }
}

impl EmitRet for Emit {
    fn ret(&mut self) -> &mut Self {
        self.write_byte(0xC3);
        self
    }
}
