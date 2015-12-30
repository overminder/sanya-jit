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
        vec![0, 1, 0x7fffffff, -0x7fffffff, -1]
    }
}

impl ATTSyntax for i32 {
    fn as_att_syntax(&self) -> String {
        format!("$0x{:x}", *self as i64 /* sign-extended */)
    }
}

impl Enumerate for i64 {
    fn possible_enumerations() -> Vec<Self> {
        vec![0, 1, 0x7fffffffffffffff, -0x7fffffffffffffff, -1]
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
        let mut iss = vec![];
        let pcrels = i32::possible_enumerations().into_iter().map(|d| Addr::PcRel(d));
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
            for s in Scale::possible_enumerations() {
                if b == R64::RSP {
                    continue;
                }
                iss.push(Addr::IS(b, s));
            }
        }
        bs.chain(bds.into_iter())
          .chain(biss.into_iter())
          .chain(bisds.into_iter())
          .chain(iss.into_iter())
          .chain(pcrels)
          .collect()
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

        let mut base = self.base().map_or("".to_owned(), |r| r.as_att_syntax());
        let mut index = "".to_owned();
        let mut scale = "".to_owned();

        let mb_disp = if let Some(disp) = self.disp() {
            Some(disp)
        } else if self.base().map_or(false, |b| b.is_rbp_or_r13()) {
            Some(0)
        } else if self.is_index_scale() {
            Some(0)
        } else {
            None
        };

        match self {
            &B(_) |
            &BD(_, _) => {}
            &BIS(_, i, s) |
            &IS(i, s) |
            &BISD(_, i, s, _) => {
                index = format!(",{}", i.as_att_syntax());
                scale = format!(",{}", s.as_att_syntax());
            }
            &PcRel(_) => {
                base = "%rip".to_owned();
            }
        };
        format!("{disp}({base}{index}{scale})",
                disp = mb_disp.map_or("".to_owned(), |d| disp_as_att_syntax(d)),
                base = base,
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
    M64(Addr),
}

impl Instr for Pop {
    fn emit(&self, buf: &mut Emit) {
        match self {
            &Pop::R64(r) => buf.pop(r),
            &Pop::M64(ref m) => buf.pop(m),
        };
    }
}

impl Enumerate for Pop {
    fn possible_enumerations() -> Vec<Self> {
        R64::possible_enumerations()
            .into_iter()
            .map(|r| Pop::R64(r))
            .chain(Addr::possible_enumerations().into_iter().map(|m| Pop::M64(m)))
            .collect()
    }
}

impl ATTSyntax for Pop {
    fn as_att_syntax(&self) -> String {
        let mut rator = "pop";
        let rand = match self {
            &Pop::R64(r) => r.as_att_syntax(),
            &Pop::M64(ref m) => {
                rator = "popq";
                m.as_att_syntax()
            }
        };
        // 7: objdump's padding.
        format!("{:7}{}", rator, rand)
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

    AddR64M64(R64, Addr),
    SubR64M64(R64, Addr),
    CmpR64M64(R64, Addr),
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

            &AddR64M64(dst, ref src) => buf.add(dst, src),
            &SubR64M64(dst, ref src) => buf.sub(dst, src),
            &CmpR64M64(dst, ref src) => buf.cmp(dst, src),
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
            for src in &Addr::possible_enumerations() {
                res.push(AddR64M64(*dst, src.clone()));
                res.push(SubR64M64(*dst, src.clone()));
                res.push(CmpR64M64(*dst, src.clone()));
            }
        }
        res
    }
}

impl ATTSyntax for Arith {
    fn as_att_syntax(&self) -> String {
        let rator = self.rator();
        let dst = self.dst_as_att_syntax();
        let src = self.src_as_att_syntax();

        format!("{:7}{},{}", rator, src, dst)
    }
}

impl Arith {
    fn rator(&self) -> String {
        use self::Arith::*;
        match self {
            &AddR64R64(_, _) |
            &AddR64I32(_, _) |
            &AddR64M64(_, _) => "add",
            &SubR64R64(_, _) |
            &SubR64I32(_, _) |
            &SubR64M64(_, _) => "sub",
            &CmpR64R64(_, _) |
            &CmpR64I32(_, _) |
            &CmpR64M64(_, _) => "cmp",
        }
        .to_owned()
    }

    fn src_as_att_syntax(&self) -> String {
        use self::Arith::*;
        match self {
            &AddR64R64(_, r) |
            &SubR64R64(_, r) |
            &CmpR64R64(_, r) => r.as_att_syntax(),

            &AddR64I32(_, i) |
            &SubR64I32(_, i) |
            &CmpR64I32(_, i) => i.as_att_syntax(),

            &AddR64M64(_, ref m) |
            &SubR64M64(_, ref m) |
            &CmpR64M64(_, ref m) => m.as_att_syntax(),
        }
    }

    fn dst_as_att_syntax(&self) -> String {
        use self::Arith::*;
        match self {
            &AddR64R64(r, _) |
            &AddR64I32(r, _) |
            &AddR64M64(r, _) |
            &SubR64R64(r, _) |
            &SubR64I32(r, _) |
            &SubR64M64(r, _) |
            &CmpR64R64(r, _) |
            &CmpR64I32(r, _) |
            &CmpR64M64(r, _) => r.as_att_syntax(),
        }
    }
}

// Mov.

enum Mov {
    R64R64(R64, R64),
    R64I64(R64, i64),
    R64M64(R64, Addr),
    M64R64(Addr, R64),
}

impl Instr for Mov {
    fn emit(&self, buf: &mut Emit) {
        use self::Mov::*;

        match self {
            &R64R64(dst, src) => buf.mov(dst, src),
            &R64I64(dst, src) => buf.mov(dst, src),
            &R64M64(dst, ref src) => buf.mov(dst, src),
            &M64R64(ref dst, src) => buf.mov(dst, src),
        };
    }
}

impl Enumerate for Mov {
    fn possible_enumerations() -> Vec<Self> {
        use self::Mov::*;

        let mut res = vec![];
        for r in &R64::possible_enumerations() {
            for src in R64::possible_enumerations() {
                res.push(R64R64(*r, src));
            }
            for src in i64::possible_enumerations() {
                res.push(R64I64(*r, src));
            }
            for m in Addr::possible_enumerations() {
                res.push(R64M64(*r, m.clone()));
                res.push(M64R64(m, *r));
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
            &R64M64(dst, ref src) => {
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &M64R64(ref dst, src) => {
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
        };

        format!("{:7}{}", rator, rand)
    }
}

// Lea.

enum Lea {
    R64M64(R64, Addr),
}

impl Instr for Lea {
    fn emit(&self, buf: &mut Emit) {
        use self::Lea::*;

        match self {
            &R64M64(dst, ref src) => buf.lea(dst, src),
        };
    }
}

impl Enumerate for Lea {
    fn possible_enumerations() -> Vec<Self> {
        use self::Lea::*;

        let mut res = vec![];
        for r in &R64::possible_enumerations() {
            for m in Addr::possible_enumerations() {
                res.push(R64M64(*r, m));
            }
        }
        res
    }
}

impl ATTSyntax for Lea {
    fn as_att_syntax(&self) -> String {
        use self::Lea::*;
        let rator = "lea";
        let rand;

        match self {
            &R64M64(dst, ref src) => {
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
        };

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

// Branches.

enum Branch {
    JmpR64(R64),
    CallR64(R64),
    JmpM64(Addr),
    CallM64(Addr),
}

impl Instr for Branch {
    fn emit(&self, buf: &mut Emit) {
        use self::Branch::*;

        match self {
            &JmpR64(r) => {
                buf.jmp(r);
            }
            &CallR64(r) => {
                buf.call(r);
            }
            &JmpM64(ref m) => {
                buf.jmp(m);
            }
            &CallM64(ref m) => {
                buf.call(m);
            }
        }
    }
}

impl Enumerate for Branch {
    fn possible_enumerations() -> Vec<Self> {
        use self::Branch::*;

        let mut res = vec![];
        for r in &R64::possible_enumerations() {
            res.push(JmpR64(*r));
            res.push(CallR64(*r));
        }
        for m in Addr::possible_enumerations() {
            res.push(JmpM64(m.clone()));
            res.push(CallM64(m));
        }
        res
    }
}

impl ATTSyntax for Branch {
    fn as_att_syntax(&self) -> String {
        use self::Branch::*;

        let rator;
        let rand;

        match self {
            &JmpR64(r) => {
                rator = "jmpq";
                rand = r.as_att_syntax();
            }
            &CallR64(r) => {
                rator = "callq";
                rand = r.as_att_syntax();
            }
            &JmpM64(ref m) => {
                rator = "jmpq";
                rand = m.as_att_syntax();
            }
            &CallM64(ref m) => {
                rator = "callq";
                rand = m.as_att_syntax();
            }
        }
        format!("{:7}*{}", rator, rand)
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

    // line.len() > 20: filter out lines that are too short.
    for (disas_line, instr) in disas_lines.iter().filter(|line| line.len() > 20).zip(instrs) {
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
    assert_assembly_matches_disassembly::<Arith>();
    assert_assembly_matches_disassembly::<Mov>();

    assert_assembly_matches_disassembly::<Branch>();
    assert_assembly_matches_disassembly::<Ret>();
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
