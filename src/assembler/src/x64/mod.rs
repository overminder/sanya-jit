/// Mostly from dart/runtime/vm/assembler_x64.cc
/// And http://wiki.osdev.org/X86-64_Instruction_Encoding
/// And http://www.felixcloutier.com/x86
/// And pypy/rpython/jit/backend/x86/rx86.py

pub mod traits;
pub mod encoding;
pub mod utils;
mod consts;
mod tests;

pub use self::consts::*;
pub use self::encoding::*;
pub use self::traits::*;
use super::emit::{Emit, Label};
use self::encoding::Addr::*;

use std::ops::{Add, Mul};

pub fn emit_nop_until_aligned(emit: &mut Emit, alignment: usize) -> usize {
    let here = emit.here();
    let mask = alignment - 1;
    if (here & mask) != 0 {
        let nops = alignment - (here & mask);
        for _ in 0..nops {
            emit.nop();
        }
        nops
    } else {
        0
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

impl Add<i32> for Addr {
    type Output = Addr;
    fn add(self, rhs: i32) -> Self::Output {
        match self {
            B(r) => BD(r, rhs),
            BIS(b, i, s) => BISD(b, i, s, rhs),
            _ => panic!("{:?} + {:?} is not defined.", self, rhs),
        }
    }
}

impl Add<Addr> for R64 {
    type Output = Addr;
    fn add(self, rhs: Addr) -> Self::Output {
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

// XXX: Refactor this. It's a mess and it's too slow.
fn emit_addr_with_opcodes(buf: &mut Emit, mut rex: REX, opcodes: &[u8], reg: RegOrOpExt, op: &Addr) {
    let mut modrm;
    let mut mb_sib = None;
    let mut mb_disp = None;

    let mb_base = op.base();
    if let Some(base) = mb_base {
        if base.erased().is_like_sp() {
            // rsp | r12: SIB will be in use.
            mb_sib = Some(SIB::new_b(base));
        }
        modrm = ModRM::new(Mod::Indirect, reg, base);
    } else {
        // mod = 00 & rm = rbp: index * scale + disp32
        assert!(op.is_index_scale());
        modrm = ModRM::new(Mod::IndirectBP, reg, R64::rsp);
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
        if mb_base.map(|b| b.erased().is_like_bp()).unwrap_or(false) {
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
            modrm.rm = R64::rsp;
        }
        &IS(index, scale) => {
            mb_sib = Some(SIB::new_si(scale, index));
        }
    }

    rex = rex.with_modrm(modrm);
    mb_sib.map(|sib| rex = rex.with_sib(sib));
    rex.emit(buf);
    for opcode in opcodes {
        buf.write_byte(*opcode);
    }
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

fn emit_addr(buf: &mut Emit, rex: REX, opcode: u8, reg: RegOrOpExt, op: &Addr) {
    emit_addr_with_opcodes(buf, rex, &[opcode], reg, op)
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
        emit_addr(self, REX::w(), 0x03, RegOrOpExt::reg(dst), src);
        self
    }

    fn sub(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x2B, RegOrOpExt::reg(dst), src);
        self
    }

    fn cmp(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x3B, RegOrOpExt::reg(dst), src);
        self
    }
}

impl<'a> EmitArith<&'a Addr, R64> for Emit {
    fn add(&mut self, dst: &Addr, src: R64) -> &mut Self {
        emit_addr(self, REX::w(), 0x01, RegOrOpExt::reg(src), dst);
        self
    }

    fn sub(&mut self, dst: &Addr, src: R64) -> &mut Self {
        emit_addr(self, REX::w(), 0x29, RegOrOpExt::reg(src), dst);
        self
    }

    fn cmp(&mut self, dst: &Addr, src: R64) -> &mut Self {
        emit_addr(self, REX::w(), 0x39, RegOrOpExt::reg(src), dst);
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
        emit_addr(self, REX::w(), 0x8B, RegOrOpExt::reg(dst), src);
        self
    }
}

impl<'a> EmitMov<&'a Addr, R64> for Emit {
    fn mov(&mut self, dst: &Addr, src: R64) -> &mut Self {
        emit_addr(self, REX::w(), 0x89, RegOrOpExt::reg(src), dst);
        self
    }
}

impl<'a> EmitMov<R32, &'a Addr> for Emit {
    fn mov(&mut self, dst: R32, src: &Addr) -> &mut Self {
        emit_addr(self, REX::none(), 0x8B, RegOrOpExt::reg(dst), src);
        self
    }
}

// Yeah, pretty much lots of duplicated codes...
impl<'a> EmitMovx<R64, &'a Addr> for Emit {
    fn movsb(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr_with_opcodes(self, REX::w(), &[0x0F, 0xBE], RegOrOpExt::reg(dst), src);
        self
    }

    fn movsw(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr_with_opcodes(self, REX::w(), &[0x0F, 0xBF], RegOrOpExt::reg(dst), src);
        self
    }

    fn movsl(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr_with_opcodes(self, REX::w(), &[0x63], RegOrOpExt::reg(dst), src);
        self
    }

    fn movzb(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr_with_opcodes(self, REX::w(), &[0x0F, 0xB6], RegOrOpExt::reg(dst), src);
        self
    }

    fn movzw(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr_with_opcodes(self, REX::w(), &[0x0F, 0xB7], RegOrOpExt::reg(dst), src);
        self
    }

    fn movzl(&mut self, dst: R64, src: &Addr) -> &mut Self {
        // `mov r64, rm32` automatically zero-extends.
        self.mov(dst.to_r32(), src)
    }
}

impl<'a> EmitLea<R64, &'a mut Label> for Emit {
    fn lea(&mut self, dst: R64, src: &mut Label) -> &mut Self {
        self.lea(dst, &Addr::PcRel(0xff_i32 /* any imm32 */));
        src.patch_last_i32(self);
        self
    }
}

impl<'a> EmitLea<R64, &'a Addr> for Emit {
    fn lea(&mut self, dst: R64, src: &Addr) -> &mut Self {
        emit_addr(self, REX::w(), 0x8D, RegOrOpExt::reg(dst), src);
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

impl EmitBranch<i32> for Emit {
    fn jmp(&mut self, op: i32) -> &mut Self {
        self.write_byte(0xE9);
        self.write_i32(op);
        self
    }

    fn call(&mut self, op: i32) -> &mut Self {
        self.write_byte(0xE8);
        self.write_i32(op);
        self
    }
}

impl<'a> EmitBranch<&'a mut Label> for Emit {
    fn jmp(&mut self, op: &mut Label) -> &mut Self {
        self.jmp(0xff_i32 /* any imm32 */);
        op.patch_last_i32(self);
        self
    }

    fn call(&mut self, op: &mut Label) -> &mut Self {
        self.call(0xff_i32);
        op.patch_last_i32(self);
        self
    }
}

impl EmitJcc<i32> for Emit {
    fn jcc(&mut self, cond: Cond, op: i32) -> &mut Self {
        if is_imm8(op) {
            self.write_byte(0x70 + (cond as u8));
            self.write_byte(op as u8);
        } else {
            self.write_byte(0x0F);
            self.write_byte(0x80 + (cond as u8));
            self.write_i32(op);
        }
        self
    }
}

impl<'a> EmitJcc<&'a mut Label> for Emit {
    fn jcc(&mut self, cond: Cond, op: &mut Label) -> &mut Self {
        self.jcc(cond, 0xff_i32 /* any imm32 */);
        op.patch_last_i32(self);
        self
    }
}

impl EmitCmovcc<R64, R64> for Emit {
    fn cmovcc(&mut self, cond: Cond, dst: R64, src: R64) -> &mut Self {
        let modrm = ModRM::direct(dst /* reg */, src /* rm */);
        let rex = REX::w().with_modrm(modrm);
        self.write_bytes(&[rex.encoding(), 0x0F, 0x40 + (cond as u8), modrm.encoding()]);
        self
    }
}

impl EmitRet for Emit {
    fn ret(&mut self) -> &mut Self {
        self.write_byte(0xC3);
        self
    }
}

impl EmitNop for Emit {
    fn nop(&mut self) -> &mut Self {
        self.write_byte(0x90);
        self
    }
}
