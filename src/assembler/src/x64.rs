/// Mostly from dart/runtime/vm/assembler_x64.cc
/// And http://wiki.osdev.org/X86-64_Instruction_Encoding
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
    pub fn as_lower(self) -> u8 {
        (self as isize as u8) & 0x7
    }

    pub fn is_extended(self) -> bool {
        self as isize > 7
    }
}

pub struct Operand {
    length: u8,
    rex: u8,
    encoding: [u8; 6],
}

impl Operand {}

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
struct REX(u8);

impl REX {
    fn none() -> Self {
        REX(0b01000000)
    }

    fn is_none(self) -> bool {
        self.0 == REX::none().0
    }

    fn b() -> Self {
        REX(REX::none().0 | (1 << 0))
    }

    fn or_b(self) -> Self {
        REX(self.0 | REX::b().0)
    }
}

fn emit_reg_rex<A: Write>(buf: &mut A, r: Reg, mut rex: REX) -> EmitResult {
    if r.is_extended() {
        rex = rex.or_b()
    }

    if !rex.is_none() {
        buf.write_all(&[rex.0])
    } else {
        Ok(())
    }
}

impl<A: Write> EmitPush<Reg> for Emit<A> {
    fn push(&mut self, op: Reg) -> &mut Self {
        emit_reg_rex(self, op, REX::none()).unwrap();
        self.write_all(&[0x50 | op.as_lower()]).unwrap();
        self
    }
}

impl<A: Write> EmitPush<i32> for Emit<A> {
    fn push(&mut self, op: i32) -> &mut Self {
        let mut encoding = [0x68, 0, 0, 0, 0];
        NativeEndian::write_i32(&mut encoding[1..], op);
        self.write_all(&encoding).unwrap();
        self
    }
}

impl<A: Write> EmitPop<Reg> for Emit<A> {
    fn pop(&mut self, op: Reg) -> &mut Self {
        emit_reg_rex(self, op, REX::none()).unwrap();
        self.write_all(&[0x58 | op.as_lower()]).unwrap();
        self
    }
}

impl<A: Write> EmitAdd<Reg, Reg> for Emit<A> {
    fn add(&mut self, dst: Reg, src: Reg) -> &mut Self {
        fn addq_rr<A: Write>(buf: &mut A, dst: Reg, src: Reg) -> EmitResult {
            // try!(emit_op_rex(buf, dst,
            Ok(())
        }
        self
    }
}

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
fn objdump_disas_contains(bs: &[u8], needle: &str) -> bool {
    use std::process::Command;
    use std::fs::File;
    use std::str;

    let tmp_path = "/tmp/assembler.x64.test";
    let mut f = File::create(tmp_path).unwrap();
    f.write_all(bs).unwrap();

    let stdout = Command::new("objdump")
        .arg("-D")
        .arg("-bbinary")
        .arg("-mi386")
        .arg("-Mx86-64")
        .arg(tmp_path)
        .output()
        .unwrap()
        .stdout;

    str::from_utf8(&stdout).unwrap().contains(needle)
}

#[test]
fn test_push_encoding() {
    assert_encoding(&[0x50], |v| v.push(Reg::RAX));
    assert_encoding(&[0x57], |v| v.push(Reg::RDI));
    assert_encoding(&[0x41, 0x50], |v| v.push(Reg::R8));
    assert_encoding(&[0x41, 0x57], |v| v.push(Reg::R15));

    assert_encoding(&[0x68, 0xff, 0xff, 0xff, 0x7f], |v| v.push(0x7fffffff_i32));
}

#[test]
fn test_pop_encoding() {
    assert_encoding(&[0x58], |v| v.pop(Reg::RAX));
    assert_encoding(&[0x5f], |v| v.pop(Reg::RDI));
    assert_encoding(&[0x41, 0x58], |v| v.pop(Reg::R8));
    assert_encoding(&[0x41, 0x5f], |v| v.pop(Reg::R15));
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
