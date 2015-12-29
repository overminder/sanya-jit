#![cfg(test)]

use super::*;
use super::traits::*;
use test::Bencher;

fn assert_encoding<F>(expected: &[u8], f: F)
    where F: for<'a> FnOnce(&'a mut Emit) -> &'a mut Emit
{
    // Welp, I don't like this type signature...
    let mut e = Emit(vec![]);
    f(&mut e);
    assert_eq!(e.inner_ref(), expected);
}

fn assert_att_disassembly<F>(expected: &str, f: F)
    where F: for<'a> FnOnce(&'a mut Emit) -> &'a mut Emit
{
    let mut e = Emit(vec![]);
    f(&mut e);
    let disassembly = objdump_disas_lines(e.inner_ref()).join("\n");
    assert!(disassembly.contains(expected),
            "{} doesn't contain {}",
            disassembly,
            expected);
}

fn objdump_disas_lines(bs: &[u8]) -> Vec<String> {
    use tempdir::TempDir;
    use std::process::{Stdio, Command};
    use std::fs::File;
    use std::io::{BufReader, BufRead, Write};

    let tdir = TempDir::new("assembler.x64.text").unwrap();
    let mut tmp_path = tdir.path().to_owned();
    tmp_path.push("disassembly");

    let mut f = File::create(&tmp_path).unwrap();
    f.write_all(bs).unwrap();

    let mut objdump = Command::new("objdump")
                          .arg("-D")
                          .arg("-bbinary")
                          .arg("-mi386")
                          .arg("-Mx86-64")
                          .arg(&tmp_path)
                          .stdout(Stdio::piped())
                          .spawn()
                          .unwrap();

    let stdout = BufReader::new(objdump.stdout.as_mut().unwrap());
    stdout.lines().skip(7 /* Non-disassembly lines */).map(|line| line.unwrap()).collect()
}

// Util traits.

// XXX: Consider switch to quickcheck when we have time.
trait Enumerate : Sized {
    fn possible_enumerations() -> Vec<Self>;
}

trait ATTSyntax {
    fn as_att_syntax(&self) -> String;
}

trait Instr: ATTSyntax {
    fn emit(&self, buf: &mut Emit);
}

// Impls for external types.

impl Enumerate for Reg {
    fn possible_enumerations() -> Vec<Self> {
        use super::Reg::*;
        vec![RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
             R8, R9, R10, R11, R12, R13, R14, R15,]
    }
}

impl Enumerate for Imm64 {
    fn possible_enumerations() -> Vec<Self> {
        vec![0, 1, 2, 0x7fffffff, -1, -2, -3]
            .into_iter()
            .map(|i| Imm64::I32(i))
            .collect()
    }
}

impl ATTSyntax for Reg {
    fn as_att_syntax(&self) -> String {
        use super::Reg::*;
        match *self {
            RAX => "%rax",
            RCX => "%rcx",
            RDX => "%rdx",
            RBX => "%rbx",

            RSP => "%rsp",
            RBP => "%rbp",
            RSI => "%rsi",
            RDI => "%rdi",

            R8 => "%r8",
            R9 => "%r9",
            R10 => "%r10",
            R11 => "%r11",
            R12 => "%r12",
            R13 => "%r13",
            R14 => "%r14",
            R15 => "%r15",
        }
        .to_owned()
    }
}

impl ATTSyntax for Imm64 {
    fn as_att_syntax(&self) -> String {
        let i = match *self {
            Imm64::I32(i) => i as i64,
            Imm64::I64(i) => i,
        };
        format!("$0x{:x}", i)
    }
}

// Internal types and impls.

// Push

enum Push {
    Reg(Reg),
    Imm64(Imm64),
}

impl ATTSyntax for Push {
    fn as_att_syntax(&self) -> String {
        let mut rator = "push";
        let rand = match self {
            &Push::Reg(r) => r.as_att_syntax(),
            &Push::Imm64(i) => {
                rator = "pushq";
                i.as_att_syntax()
            }
        };
        // 7: objdump's padding.
        format!("{:7}{}", rator, rand)
    }
}

impl Instr for Push {
    fn emit(&self, buf: &mut Emit) {
        match self {
            &Push::Reg(r) => buf.push(r),
            &Push::Imm64(i) => buf.push(i),
        };
    }
}

impl Enumerate for Push {
    fn possible_enumerations() -> Vec<Self> {
        Reg::possible_enumerations()
            .into_iter()
            .map(|r| Push::Reg(r))
            .chain(Imm64::possible_enumerations().into_iter().map(|i| Push::Imm64(i)))
            .collect()
    }
}

// Pop

enum Pop {
    Reg(Reg),
}

impl Instr for Pop {
    fn emit(&self, buf: &mut Emit) {
        match self {
            &Pop::Reg(r) => buf.pop(r),
        };
    }
}

impl Enumerate for Pop {
    fn possible_enumerations() -> Vec<Self> {
        Reg::possible_enumerations()
            .into_iter()
            .map(|r| Pop::Reg(r))
            .collect()
    }
}

impl ATTSyntax for Pop {
    fn as_att_syntax(&self) -> String {
        let mut rator = "pop";
        let rand = match self {
            &Pop::Reg(r) => r.as_att_syntax(),
        };
        // 7: objdump's padding.
        format!("{:7}{}", rator, rand)
    }
}

// Ret

struct Ret;

impl Instr for Ret {
    fn emit(&self, buf: &mut Emit) {
        buf.ret();
    }
}

impl Enumerate for Ret {
    fn possible_enumerations() -> Vec<Self> {
        vec![Ret]
    }
}

impl ATTSyntax for Ret {
    fn as_att_syntax(&self) -> String {
        "ret".to_owned()
    }
}

// Tests

fn assert_assembly_matches_disassembly<A: Instr + Enumerate>() {
    let instrs = A::possible_enumerations();
    let mut code_buf = Emit(Vec::with_capacity(instrs.len()));
    for i in &instrs {
        i.emit(&mut code_buf);
    }
    let disas_lines = objdump_disas_lines(code_buf.inner_ref());
    for (disas_line, instr) in disas_lines.iter().zip(instrs) {
        let instr_att = instr.as_att_syntax();
        assert!(disas_line.contains(&instr_att),
                "{} does't contain {}",
                disas_line,
                instr_att);
    }
}

#[test]
fn test_assembly_matches_disassembly() {
    assert_assembly_matches_disassembly::<Push>();
    assert_assembly_matches_disassembly::<Pop>();
    assert_assembly_matches_disassembly::<Ret>();
}

#[test]
fn test_push_encoding() {
    assert_encoding(&[0x50], |v| v.push(Reg::RAX));
    assert_encoding(&[0x57], |v| v.push(Reg::RDI));
    assert_encoding(&[0x41, 0x50], |v| v.push(Reg::R8));
    assert_encoding(&[0x41, 0x57], |v| v.push(Reg::R15));

    assert_att_disassembly("push   %rax", |v| v.push(Reg::RAX));
    assert_att_disassembly("push   %rdi", |v| v.push(Reg::RDI));
    assert_att_disassembly("push   %r8", |v| v.push(Reg::R8));
    assert_att_disassembly("push   %r15", |v| v.push(Reg::R15));

    assert_encoding(&[0x68, 0xff, 0xff, 0xff, 0x7f],
                    |v| v.push(Imm64::I32(0x7fffffff)));
    assert_att_disassembly("pushq  $0x7fffffff", |v| v.push(Imm64::I32(0x7fffffff)));
}

#[test]
fn test_pop_encoding() {
    assert_encoding(&[0x58], |v| v.pop(Reg::RAX));
    assert_encoding(&[0x5f], |v| v.pop(Reg::RDI));
    assert_encoding(&[0x41, 0x58], |v| v.pop(Reg::R8));
    assert_encoding(&[0x41, 0x5f], |v| v.pop(Reg::R15));

    assert_att_disassembly("pop    %rax", |v| v.pop(Reg::RAX));
    assert_att_disassembly("pop    %rdi", |v| v.pop(Reg::RDI));
    assert_att_disassembly("pop    %r8", |v| v.pop(Reg::R8));
    assert_att_disassembly("pop    %r15", |v| v.pop(Reg::R15));
}

#[test]
fn test_ret_encoding() {
    assert_encoding(&[0xC3], |v| v.ret());
}

#[test]
fn test_arith_encoding() {
    assert_encoding(&[0x48, 0x01, 0xc0], |v| v.add(Reg::RAX, Reg::RAX));
    assert_encoding(&[0x48, 0x01, 0xc7], |v| v.add(Reg::RDI, Reg::RAX));
    assert_encoding(&[0x48, 0x01, 0xf8], |v| v.add(Reg::RAX, Reg::RDI));
    assert_encoding(&[0x49, 0x01, 0xc0], |v| v.add(Reg::R8, Reg::RAX));
    assert_encoding(&[0x4c, 0x01, 0xc0], |v| v.add(Reg::RAX, Reg::R8));
    assert_encoding(&[0x4d, 0x01, 0xc0], |v| v.add(Reg::R8, Reg::R8));

    assert_att_disassembly("add    %rax,%rax", |v| v.add(Reg::RAX, Reg::RAX));
    assert_att_disassembly("add    %rax,%rdi", |v| v.add(Reg::RDI, Reg::RAX));

    assert_att_disassembly("sub    %r8,%r15", |v| v.sub(Reg::R15, Reg::R8));
    assert_att_disassembly("cmp    %r8,%r15", |v| v.cmp(Reg::R15, Reg::R8));
}

#[test]
fn test_jit_pushpop() {
    use mem::JitMem;
    let mut emit = Emit(vec![]);
    emit.push(Reg::RDI)
        .pop(Reg::RAX)
        .ret();
    let jitmem = JitMem::new(emit.inner_ref());
    let arg = 12345;
    let res = unsafe { jitmem.call_ptr_ptr(arg) };
    assert_eq!(arg, res);
}

#[test]
fn test_jit_add() {
    use mem::JitMem;
    let mut emit = Emit(vec![]);
    emit.push(Imm64::I32(0))
        .pop(Reg::RAX)
        .add(Reg::RAX, Reg::R8)
        .add(Reg::RAX, Reg::R9)
        .ret();
    let jitmem = JitMem::new(emit.inner_ref());
    let res = unsafe { jitmem.call_ptr6_ptr(0, 1, 2, 3, 4, 5) };
    assert_eq!(9, res);
}

// Benchmarks.

const ALLOC_SIZE: usize = 32000;

#[bench]
fn bench_emit_add(b: &mut Bencher) {
    b.iter(|| {
        let mut emit = Emit(Vec::with_capacity(ALLOC_SIZE));
        for _ in 0..(ALLOC_SIZE / 3) {
            emit.add(Reg::R8, Reg::R9);
        }
    });
}

#[bench]
fn bench_emit_ret(b: &mut Bencher) {
    b.iter(|| {
        let mut emit = Emit(Vec::with_capacity(ALLOC_SIZE));
        for _ in 0..ALLOC_SIZE {
            emit.ret();
        }
    });
}

#[bench]
fn bench_emit_push(b: &mut Bencher) {
    b.iter(|| {
        let mut emit = Emit(Vec::with_capacity(ALLOC_SIZE));
        for _ in 0..(ALLOC_SIZE * 2 / 3) {
            emit.push(Reg::R8);
        }
    });
}

#[bench]
fn bench_emit_raw_vec(b: &mut Bencher) {
    b.iter(|| {
        let mut v = Vec::with_capacity(ALLOC_SIZE);
        for _ in 0..ALLOC_SIZE {
            v.push(0xff);
        }
    });
}
