pub mod traits;
mod tests;

use self::traits::*;

use byteorder::{ByteOrder, NativeEndian};

/// Mostly from dart/runtime/vm/assembler_x64.cc
/// And http://wiki.osdev.org/X86-64_Instruction_Encoding
/// And http://www.felixcloutier.com/x86
/// And pypy/rpython/jit/backend/x86/rx86.py
#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Reg {
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

impl Reg {
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

struct ModRM {
    mod_: Mod,
    reg: Reg,
    rm: Reg,
}

impl ModRM {
    fn direct(reg: Reg, rm: Reg) -> Self {
        ModRM {
            mod_: Mod::Direct,
            reg: reg,
            rm: rm,
        }
    }

    fn encoding(self) -> u8 {
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
        if modrm.reg.is_extended() {
            self = self.or(REX::r());
        }
        if modrm.rm.is_extended() {
            self = self.or(REX::b());
        }
        self
    }
}

fn emit_reg_rex(buf: &mut Emit, r: Reg, mut rex: REX) {
    if r.is_extended() {
        rex = rex.or(REX::b())
    }

    if !rex.is_none() {
        buf.write_byte(rex.encoding())
    }
}

#[derive(Copy, Clone)]
pub enum Imm64 {
    I32(i32),
    I64(i64),
}

impl EmitPush<Reg> for Emit {
    fn push(&mut self, op: Reg) -> &mut Self {
        emit_reg_rex(self, op, REX::none());
        self.write_byte(0x50 | op.lower_part());
        self
    }
}

impl EmitPush<Imm64> for Emit {
    fn push(&mut self, op: Imm64) -> &mut Self {
        match op {
            Imm64::I32(i) => {
                let mut encoding = [0x68, 0, 0, 0, 0];
                NativeEndian::write_i32(&mut encoding[1..], i);
                self.write_bytes(&encoding);
            }
            Imm64::I64(_) => {
                panic!("XXX: push(Imm64)");
            }
        }
        self
    }
}

impl EmitPop<Reg> for Emit {
    fn pop(&mut self, op: Reg) -> &mut Self {
        emit_reg_rex(self, op, REX::none());
        self.write_byte(0x58 | op.lower_part());
        self
    }
}

fn arith_reg_reg(buf: &mut Emit, opcode: u8, dst: Reg, src: Reg) {
    let modrm = ModRM::direct(src, dst);
    let rex = REX::w().with_modrm(&modrm);
    buf.write_bytes(&[rex.encoding(), opcode, modrm.encoding()]);
}

impl EmitArith<Reg, Reg> for Emit {
    fn add(&mut self, dst: Reg, src: Reg) -> &mut Self {
        arith_reg_reg(self, 0x01 /* r/m64 += r64 */, dst, src);
        self
    }

    fn sub(&mut self, dst: Reg, src: Reg) -> &mut Self {
        arith_reg_reg(self, 0x29 /* r/m64 += r64 */, dst, src);
        self
    }

    fn cmp(&mut self, dst: Reg, src: Reg) -> &mut Self {
        arith_reg_reg(self, 0x39 /* r/m64 += r64 */, dst, src);
        self
    }
}

impl EmitArith<Reg, Imm64> for Emit {
    fn add(&mut self, dst: Reg, src: Imm64) -> &mut Self {
        panic!("not implmented");
        // arith_reg_reg(self, 0x01 /* r/m64 += r64 */, dst, src).unwrap();
        self
    }

    fn sub(&mut self, dst: Reg, src: Imm64) -> &mut Self {
        panic!("not implmented");
        // arith_reg_reg(self, 0x29 /* r/m64 += r64 */, dst, src).unwrap();
        self
    }

    fn cmp(&mut self, dst: Reg, src: Imm64) -> &mut Self {
        panic!("not implmented");
        self
    }
}

impl EmitRet for Emit {
    fn ret(&mut self) -> &mut Self {
        self.write_byte(0xC3);
        self
    }
}
