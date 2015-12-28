/// Mostly from dart/runtime/vm/assembler_x64.cc
/// And http://wiki.osdev.org/X86-64_Instruction_Encoding
/// And http://www.felixcloutier.com/x86
/// And pypy/rpython/jit/backend/x86/rx86.py

use std::io::{self, Write};
use byteorder::{ByteOrder, NativeEndian};

pub type EmitResult = io::Result<()>;

use x64_traits::*;

#[repr(isize)]
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
        (self as isize as u8) & 0x7
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
        ((self.mod_ as u8) << 6)
            | (self.reg.lower_part() << 3)
            | self.rm.lower_part()
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

    fn w() -> Self { REX(REX::none().0 | (1 << 3)) }
    fn r() -> Self { REX(REX::none().0 | (1 << 2)) }
    fn x() -> Self { REX(REX::none().0 | (1 << 1)) }
    fn b() -> Self { REX(REX::none().0 | (1 << 0)) }

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

fn emit_reg_rex<A: Write>(buf: &mut A, r: Reg, mut rex: REX) -> EmitResult {
    if r.is_extended() {
        rex = rex.or(REX::b())
    }

    if !rex.is_none() {
        buf.write_all(&[rex.0])
    } else {
        Ok(())
    }
}

#[derive(Copy, Clone)]
pub enum Imm64 {
    I32(i32),
    I64(i64),
}

impl<A: Write> EmitPush<Reg> for Emit<A> {
    fn push(&mut self, op: Reg) -> &mut Self {
        emit_reg_rex(self, op, REX::none()).unwrap();
        self.write_all(&[0x50 | op.lower_part()]).unwrap();
        self
    }
}

impl<A: Write> EmitPush<Imm64> for Emit<A> {
    fn push(&mut self, op: Imm64) -> &mut Self {
        match op {
            Imm64::I32(i) => {
                let mut encoding = [0x68, 0, 0, 0, 0];
                NativeEndian::write_i32(&mut encoding[1..], i);
                self.write_all(&encoding).unwrap();
            },
            Imm64::I64(_) => {
                panic!("XXX: push(Imm64)");
            },
        }
        self
    }
}

impl<A: Write> EmitPop<Reg> for Emit<A> {
    fn pop(&mut self, op: Reg) -> &mut Self {
        emit_reg_rex(self, op, REX::none()).unwrap();
        self.write_all(&[0x58 | op.lower_part()]).unwrap();
        self
    }
}

fn arith_reg_reg<A: Write>(buf: &mut A, opcode: u8, dst: Reg, src: Reg) -> EmitResult {
    let modrm = ModRM::direct(src, dst);
    let rex = REX::w().with_modrm(&modrm);
    buf.write_all(&[rex.encoding(), opcode, modrm.encoding()])
}

impl<A: Write> EmitArith<Reg, Reg> for Emit<A> {
    fn add(&mut self, dst: Reg, src: Reg) -> &mut Self {
        arith_reg_reg(self, 0x01 /* r/m64 += r64 */, dst, src).unwrap();
        self
    }

    fn sub(&mut self, dst: Reg, src: Reg) -> &mut Self {
        arith_reg_reg(self, 0x29 /* r/m64 += r64 */, dst, src).unwrap();
        self
    }

    fn cmp(&mut self, dst: Reg, src: Reg) -> &mut Self {
        arith_reg_reg(self, 0x39 /* r/m64 += r64 */, dst, src).unwrap();
        self
    }
}

impl<A: Write> EmitArith<Reg, Imm64> for Emit<A> {

impl<A: Write> EmitRet for Emit<A> {
    fn ret(&mut self) -> &mut Self {
        self.write_all(&[0xC3]).unwrap();
        self
    }
}


#[cfg(test)]
fn assert_encoding<F>(expected: &[u8], f: F)
    where F: for<'a> FnOnce(&'a mut Emit<&'a mut Vec<u8>>) -> &'a mut Emit<&'a mut Vec<u8>>
{
    // Welp, I don't like this type signature...
    let mut v = vec![];
    f(&mut emit(&mut v));
    assert_eq!(v, expected);
}

#[cfg(test)]
fn assert_att_disas<F>(expected: &str, f: F)
    where F: for<'a> FnOnce(&'a mut Emit<&'a mut Vec<u8>>) -> &'a mut Emit<&'a mut Vec<u8>>
{
    let mut v = vec![];
    f(&mut emit(&mut v));
    let disas = objdump_disas(&v);
    assert!(disas.contains(expected), "{} doesn't contain {}", disas, expected);
}

#[cfg(test)]
fn objdump_disas(bs: &[u8]) -> String {
    use tempdir::TempDir;
    use std::process::Command;
    use std::fs::File;

    let tdir = TempDir::new("assembler.x64.text").unwrap();
    let mut tmp_path = tdir.path().to_owned();
    tmp_path.push("disas");

    let mut f = File::create(&tmp_path).unwrap();
    f.write_all(bs).unwrap();

    let stdout = Command::new("objdump")
        .arg("-D")
        .arg("-bbinary")
        .arg("-mi386")
        .arg("-Mx86-64")
        // ^ AT&T
        .arg(&tmp_path)
        .output()
        .unwrap()
        .stdout;

    String::from_utf8(stdout).unwrap()
}

#[test]
fn test_push_encoding() {
    assert_encoding(&[0x50], |v| v.push(Reg::RAX));
    assert_encoding(&[0x57], |v| v.push(Reg::RDI));
    assert_encoding(&[0x41, 0x50], |v| v.push(Reg::R8));
    assert_encoding(&[0x41, 0x57], |v| v.push(Reg::R15));

    assert_att_disas("push   %rax", |v| v.push(Reg::RAX));
    assert_att_disas("push   %rdi", |v| v.push(Reg::RDI));
    assert_att_disas("push   %r8", |v| v.push(Reg::R8));
    assert_att_disas("push   %r15", |v| v.push(Reg::R15));

    assert_encoding(&[0x68, 0xff, 0xff, 0xff, 0x7f], |v| v.push(Imm64::I32(0x7fffffff)));
    assert_att_disas("pushq  $0x7fffffff", |v| v.push(Imm64::I32(0x7fffffff)));
}

#[test]
fn test_pop_encoding() {
    assert_encoding(&[0x58], |v| v.pop(Reg::RAX));
    assert_encoding(&[0x5f], |v| v.pop(Reg::RDI));
    assert_encoding(&[0x41, 0x58], |v| v.pop(Reg::R8));
    assert_encoding(&[0x41, 0x5f], |v| v.pop(Reg::R15));

    assert_att_disas("pop    %rax", |v| v.pop(Reg::RAX));
    assert_att_disas("pop    %rdi", |v| v.pop(Reg::RDI));
    assert_att_disas("pop    %r8", |v| v.pop(Reg::R8));
    assert_att_disas("pop    %r15", |v| v.pop(Reg::R15));
}

#[test]
fn test_arith_encoding() {
    assert_encoding(&[0x48, 0x01, 0xc0], |v| v.add(Reg::RAX, Reg::RAX));
    assert_encoding(&[0x48, 0x01, 0xc7], |v| v.add(Reg::RDI, Reg::RAX));
    assert_encoding(&[0x48, 0x01, 0xf8], |v| v.add(Reg::RAX, Reg::RDI));
    assert_encoding(&[0x49, 0x01, 0xc0], |v| v.add(Reg::R8, Reg::RAX));
    assert_encoding(&[0x4c, 0x01, 0xc0], |v| v.add(Reg::RAX, Reg::R8));
    assert_encoding(&[0x4d, 0x01, 0xc0], |v| v.add(Reg::R8, Reg::R8));

    assert_att_disas("add    %rax,%rax", |v| v.add(Reg::RAX, Reg::RAX));
    assert_att_disas("add    %rax,%rdi", |v| v.add(Reg::RDI, Reg::RAX));

    assert_att_disas("sub    %r8,%r15", |v| v.sub(Reg::R15, Reg::R8));
    assert_att_disas("cmp    %r8,%r15", |v| v.cmp(Reg::R15, Reg::R8));
}

#[test]
fn test_jit_pushpop() {
    use mem::JitMem;
    let mut bs = vec![];
    emit(&mut bs)
        .push(Reg::RDI)
        .pop(Reg::RAX)
        .ret();
    let jitmem = JitMem::new(&bs);
    let arg = 12345;
    let res = unsafe { jitmem.call_ptr_ptr(arg) };
    assert_eq!(arg, res);
}

#[test]
fn test_jit_add() {
    use mem::JitMem;
    let mut bs = vec![];
    emit(&mut bs)
        .push(Imm64::I32(0))
        .pop(Reg::RAX)
        .add(Reg::RAX, Reg::R8)
        .add(Reg::RAX, Reg::R9)
        .ret();
    let jitmem = JitMem::new(&bs);
    let res = unsafe { jitmem.call_ptr6_ptr(0, 1, 2, 3, 4, 5) };
    assert_eq!(9, res);
}

