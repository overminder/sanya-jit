pub mod traits;
mod tests;

use self::traits::*;

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
        self as isize > 7
    }
}

#[repr(u8)]
#[derive(Copy, Clone)]
enum Mod {
    Indirect = 0,
    Indirect8 = 1,
    Indirect32 = 2,
    Direct = 3,
}

#[derive(Copy, Clone)]
enum RegOrOpExt {
    R(R64),
    O(u8),
}

impl RegOrOpExt {
    fn lower_part(&self) -> u8 {
        match self {
            &RegOrOpExt::R(r) => r.lower_part(),
            &RegOrOpExt::O(o) => o,
        }
    }

    fn is_extended_reg(&self) -> bool {
        match self {
            &RegOrOpExt::R(r) => r.is_extended(),
            &RegOrOpExt::O(o) => false,
        }
    }
}

struct ModRM {
    mod_: Mod,
    reg: RegOrOpExt,
    rm: R64,
}

impl ModRM {
    fn direct(reg: R64, rm: R64) -> Self {
        ModRM {
            mod_: Mod::Direct,
            reg: RegOrOpExt::R(reg),
            rm: rm,
        }
    }

    fn direct_opext(opext: u8, rm: R64) -> Self {
        ModRM {
            mod_: Mod::Direct,
            reg: RegOrOpExt::O(opext),
            rm: rm,
        }
    }

    fn encoding(&self) -> u8 {
        ((self.mod_ as u8) << 6) | (self.reg.lower_part() << 3) | self.rm.lower_part()
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

    fn with_modrm(mut self, modrm: &ModRM) -> Self {
        if modrm.reg.is_extended_reg() {
            self = self.or(REX::r());
        }
        if modrm.rm.is_extended() {
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
}

fn emit_reg_rex(buf: &mut Emit, r: R64, mut rex: REX) {
    if r.is_extended() {
        rex = rex.or(REX::b())
    }

    if !rex.is_none() {
        buf.write_byte(rex.encoding())
    }
}

impl EmitPush<R64> for Emit {
    fn push(&mut self, op: R64) -> &mut Self {
        emit_reg_rex(self, op, REX::none());
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

impl EmitPop<R64> for Emit {
    fn pop(&mut self, op: R64) -> &mut Self {
        emit_reg_rex(self, op, REX::none());
        self.write_byte(0x58 | op.lower_part());
        self
    }
}

fn emit_rm64dst_r64src(buf: &mut Emit, opcode: u8, dst: R64, src: R64) {
    let modrm = ModRM::direct(src, dst);
    let rex = REX::w().with_modrm(&modrm);
    buf.write_bytes(&[rex.encoding(), opcode, modrm.encoding()]);
}

// 81 /$opext id
fn emit_arith_rm64_i32(buf: &mut Emit, opext: u8, dst: R64, src: i32) {
    assert!(opext < 8);
    let modrm = ModRM::direct_opext(opext, dst);
    let rex = REX::w().with_modrm(&modrm);
    buf.write_bytes(&[rex.encoding(), 0x81, modrm.encoding()]);
    buf.write_i32(src);
}

// REX.W + $opcode + rd io
fn emit_rm64dst_i64(buf: &mut Emit, opcode: u8, dst: R64, src: i64) {
    let rex = REX::w().with_modrm_rm(dst);
    buf.write_bytes(&[rex.encoding(), opcode | dst.lower_part()]);
    buf.write_i64(src);
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

impl EmitMov<R64, R64> for Emit {
    fn mov(&mut self, dst: R64, src: R64) -> &mut Self {
        emit_rm64dst_r64src(self, 0x89, dst, src);
        self
    }
}

impl EmitMov<R64, i64> for Emit {
    fn mov(&mut self, dst: R64, src: i64) -> &mut Self {
        emit_rm64dst_i64(self, 0xB8, dst, src);
        self
    }
}

impl EmitRet for Emit {
    fn ret(&mut self) -> &mut Self {
        self.write_byte(0xC3);
        self
    }
}
