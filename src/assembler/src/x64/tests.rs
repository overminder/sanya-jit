#![cfg(test)]

use tempdir::TempDir;
use std::process::{Stdio, Command};
use std::fs::File;
use std::path::Path;
use std::io::{BufReader, BufRead, Write};

use super::*;
use super::traits::*;
use test::Bencher;

// Test traits.

// XXX: Consider switching to quickcheck when we have time.
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

impl Enumerate for R64 {
    fn possible_enumerations() -> Vec<Self> {
        use super::R64::*;
        vec![RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
             R8, R9, R10, R11, R12, R13, R14, R15,]
    }
}

impl ATTSyntax for R64 {
    fn as_att_syntax(&self) -> String {
        use super::R64::*;
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

impl Enumerate for i32 {
    fn possible_enumerations() -> Vec<Self> {
        vec![0, 1, 2, 0x7fffffff, -1, -2, -3]
    }
}

impl ATTSyntax for i32 {
    fn as_att_syntax(&self) -> String {
        format!("$0x{:x}", *self as i64 /* sign-extended */)
    }
}

impl Enumerate for i64 {
    fn possible_enumerations() -> Vec<Self> {
        vec![0, 1, 2, 0x7fffffff, 0xffffffff, -1, -2, -3]
    }
}

impl Enumerate for Scale {
    fn possible_enumerations() -> Vec<Self> {
        use super::Scale::*;
        vec![S1, S2, S4, S8]
    }
}

impl ATTSyntax for Scale {
    fn as_att_syntax(&self) -> String {
        use super::Scale::*;

        match self {
            &S1 => "1",
            &S2 => "2",
            &S4 => "4",
            &S8 => "8",
        }
        .to_owned()
    }
}

impl ATTSyntax for i64 {
    fn as_att_syntax(&self) -> String {
        format!("$0x{:x}", *self)
    }
}

impl Enumerate for Addr {
    fn possible_enumerations() -> Vec<Self> {
        let bs = R64::possible_enumerations().into_iter().map(|r| Addr::B(r));
        let mut bds = vec![];
        let mut biss = vec![];
        let mut bisds = vec![];
        for b in R64::possible_enumerations() {
            for d in i32::possible_enumerations() {
                bds.push(Addr::BD(b, d));
            }
            for i in R64::possible_enumerations() {
                if i == R64::RSP {
                    // %sp is not a valid index register.
                    continue;
                }
                for s in Scale::possible_enumerations() {
                    biss.push(Addr::BIS(b, i, s));

                    for d in i32::possible_enumerations() {
                        bisds.push(Addr::BISD(b, i, s, d));
                    }
                }
            }
        }
        bs.chain(bds.into_iter()).chain(biss.into_iter()).chain(bisds.into_iter()).collect()
    }
}

fn disp_as_att_syntax(i: i32) -> String {
    use std::fmt::LowerHex;
    use std::ops::Neg;
    use std::num::Zero;

    fn format<A>(i: A) -> String
        where A: Neg<Output = A> + LowerHex + PartialOrd + Zero
    {
        if i < A::zero() {
            format!("-0x{:x}", -i)
        } else {
            format!("0x{:x}", i)
        }
    }

    if is_imm8(i) {
        format(i as i8)
    } else {
        format(i)
    }
}

impl ATTSyntax for Addr {
    fn as_att_syntax(&self) -> String {
        use super::Addr::*;

        let base = self.base();
        let mut index = "".to_owned();
        let mut scale = "".to_owned();

        let mb_disp = if let Some(disp) = self.disp() {
            Some(disp)
        } else if base.is_rbp_or_r13() {
            Some(0)
        } else {
            None
        };

        match self {
            &B(_) => {}
            &BD(_, _) => {}
            &BIS(_, i, s) => {
                index = format!(",{}", i.as_att_syntax());
                scale = format!(",{}", s.as_att_syntax());
            }
            &BISD(_, i, s, _) => {
                index = format!(",{}", i.as_att_syntax());
                scale = format!(",{}", s.as_att_syntax());
            }
            _ => panic!("Not implemented"),
        }
        format!("{disp}({base}{index}{scale})",
                disp = mb_disp.map(|d| disp_as_att_syntax(d)).unwrap_or("".to_owned()),
                base = base.as_att_syntax(),
                index = index,
                scale = scale)
    }
}

// Internal types and impls.

// Push

enum Push {
    R64(R64),
    I32(i32),
    M64(Addr),
}

impl ATTSyntax for Push {
    fn as_att_syntax(&self) -> String {
        let mut rator = "push";
        let rand = match self {
            &Push::R64(r) => r.as_att_syntax(),
            &Push::I32(i) => {
                rator = "pushq";
                i.as_att_syntax()
            }
            &Push::M64(ref m) => {
                rator = "pushq";
                m.as_att_syntax()
            }
        };
        // 7: objdump's padding.
        format!("{:7}{}", rator, rand)
    }
}

impl Instr for Push {
    fn emit(&self, buf: &mut Emit) {
        match self {
            &Push::R64(r) => buf.push(r),
            &Push::I32(i) => buf.push(i),
            &Push::M64(ref m) => buf.push(m),
        };
    }
}

impl Enumerate for Push {
    fn possible_enumerations() -> Vec<Self> {
        let r64s = R64::possible_enumerations().into_iter().map(|r| Push::R64(r));
        let i32s = i32::possible_enumerations().into_iter().map(|i| Push::I32(i));
        let m64s = Addr::possible_enumerations().into_iter().map(|a| Push::M64(a));
        r64s.chain(i32s).chain(m64s).collect()
    }
}

// Pop

enum Pop {
    R64(R64),
}

impl Instr for Pop {
    fn emit(&self, buf: &mut Emit) {
        match self {
            &Pop::R64(r) => buf.pop(r),
        };
    }
}

impl Enumerate for Pop {
    fn possible_enumerations() -> Vec<Self> {
        R64::possible_enumerations()
            .into_iter()
            .map(|r| Pop::R64(r))
            .collect()
    }
}

impl ATTSyntax for Pop {
    fn as_att_syntax(&self) -> String {
        let rator = "pop";
        let rand = match self {
            &Pop::R64(r) => r.as_att_syntax(),
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

// Arith

enum Arith {
    AddR64R64(R64, R64),
    SubR64R64(R64, R64),
    CmpR64R64(R64, R64),

    AddR64I32(R64, i32),
    SubR64I32(R64, i32),
    CmpR64I32(R64, i32),
}

impl Instr for Arith {
    fn emit(&self, buf: &mut Emit) {
        use self::Arith::*;

        match self {
            &AddR64R64(dst, src) => buf.add(dst, src),
            &SubR64R64(dst, src) => buf.sub(dst, src),
            &CmpR64R64(dst, src) => buf.cmp(dst, src),

            &AddR64I32(dst, src) => buf.add(dst, src),
            &SubR64I32(dst, src) => buf.sub(dst, src),
            &CmpR64I32(dst, src) => buf.cmp(dst, src),
        };
    }
}

impl Enumerate for Arith {
    fn possible_enumerations() -> Vec<Self> {
        use self::Arith::*;

        let mut res = vec![];
        for dst in &R64::possible_enumerations() {
            for src in &R64::possible_enumerations() {
                res.push(AddR64R64(*dst, *src));
                res.push(SubR64R64(*dst, *src));
                res.push(CmpR64R64(*dst, *src));
            }
            for src in &i32::possible_enumerations() {
                res.push(AddR64I32(*dst, *src));
                res.push(SubR64I32(*dst, *src));
                res.push(CmpR64I32(*dst, *src));
            }
        }
        res
    }
}

impl ATTSyntax for Arith {
    fn as_att_syntax(&self) -> String {
        use self::Arith::*;
        let rator;
        let rand;

        match self {
            &AddR64R64(dst, src) => {
                rator = "add";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &SubR64R64(dst, src) => {
                rator = "sub";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &CmpR64R64(dst, src) => {
                rator = "cmp";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &AddR64I32(dst, src) => {
                rator = "add";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &SubR64I32(dst, src) => {
                rator = "sub";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &CmpR64I32(dst, src) => {
                rator = "cmp";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
        };

        format!("{:7}{}", rator, rand)
    }
}

// Mov.

enum Mov {
    R64R64(R64, R64),
    R64I64(R64, i64),
}

impl Instr for Mov {
    fn emit(&self, buf: &mut Emit) {
        use self::Mov::*;

        match self {
            &R64R64(dst, src) => buf.mov(dst, src),
            &R64I64(dst, src) => buf.mov(dst, src),
        };
    }
}

impl Enumerate for Mov {
    fn possible_enumerations() -> Vec<Self> {
        use self::Mov::*;

        let mut res = vec![];
        for dst in &R64::possible_enumerations() {
            for src in &R64::possible_enumerations() {
                res.push(R64R64(*dst, *src));
            }
            for src in &i64::possible_enumerations() {
                res.push(R64I64(*dst, *src));
            }
        }
        res
    }
}

impl ATTSyntax for Mov {
    fn as_att_syntax(&self) -> String {
        use self::Mov::*;
        let mut rator = "mov";
        let rand;

        match self {
            &R64R64(dst, src) => {
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &R64I64(dst, src) => {
                rator = "movabs";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
        };

        format!("{:7}{}", rator, rand)
    }
}
// Test utils.

#[allow(unused)]
fn assert_encoding<F>(expected: &[u8], f: F)
    where F: for<'a> FnOnce(&'a mut Emit) -> &'a mut Emit
{
    // Welp, I don't like this type signature...
    let mut e = Emit(vec![]);
    f(&mut e);
    assert_eq!(e.inner_ref(), expected);
}

fn dump_file<A: AsRef<Path>>(path: A, bs: &[u8]) {
    let mut f = File::create(path).unwrap();
    f.write_all(bs).unwrap();
}

fn objdump_disas_lines(bs: &[u8]) -> Vec<String> {

    let tdir = TempDir::new("assembler.x64.text").unwrap();
    let mut tmp_path = tdir.path().to_owned();
    tmp_path.push("disassembly");

    dump_file(&tmp_path, bs);

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

fn assert_assembly_matches_disassembly<A: Instr + Enumerate>() {
    let instrs = A::possible_enumerations();
    let mut code_buf = Emit(Vec::with_capacity(instrs.len()));
    for i in &instrs {
        i.emit(&mut code_buf);
    }
    let disas_lines = objdump_disas_lines(code_buf.inner_ref());

    // line.len() > 16: filter out lines that are too short.
    for (disas_line, instr) in disas_lines.iter().filter(|line| line.len() > 16).zip(instrs) {
        let instr_att = instr.as_att_syntax();
        let mut instr_buf = Emit(vec![]);
        instr.emit(&mut instr_buf);
        if !disas_line.contains(&instr_att) {
            dump_file("/tmp/assembler.a.out", code_buf.inner_ref());
            assert!(false,
                    "{} (binary: {:?}) doesn't contain {}, see /tmp/assembler.a.out",
                    disas_line,
                    instr_buf.inner_ref(),
                    instr_att);
        }
    }
}

// Tests.

#[test]
fn test_assembly_matches_disassembly() {
    assert_assembly_matches_disassembly::<Push>();
    assert_assembly_matches_disassembly::<Pop>();
    assert_assembly_matches_disassembly::<Ret>();
    assert_assembly_matches_disassembly::<Arith>();
    assert_assembly_matches_disassembly::<Mov>();
}

#[test]
fn test_jit_pushpop() {
    use mem::JitMem;
    let mut emit = Emit(vec![]);
    emit.push(R64::RDI)
        .pop(R64::RAX)
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
    emit.push(0_i32)
        .pop(R64::RAX)
        .add(R64::RAX, R64::R8)
        .add(R64::RAX, R64::R9)
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
            emit.add(R64::R8, R64::R9);
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
            emit.push(R64::R8);
        }
    });
}

#[bench]
fn bench_emit_push_i32(b: &mut Bencher) {
    b.iter(|| {
        let mut emit = Emit(Vec::with_capacity(ALLOC_SIZE));
        for _ in 0..(ALLOC_SIZE / 5) {
            emit.push(-1_i32);
        }
    });
}

#[bench]
fn bench_emit_push_rm64(b: &mut Bencher) {
    b.iter(|| {
        let mut emit = Emit(Vec::with_capacity(ALLOC_SIZE));
        for _ in 0..(ALLOC_SIZE / 8) {
            emit.push(&Addr::B(R64::R12));
            emit.push(&Addr::B(R64::R13));
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
