#![cfg(test)]

use x64::*;
use x64::Addr::*;
use x64::Scale::*;
use x64::utils::*;
use mem::JitMem;
use emit::*;

// Test traits.

// XXX: Consider switching to quickcheck when we have time.
// Really...
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
        vec![rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
             r8, r9, r10, r11, r12, r13, r14, r15,]
    }
}

impl ATTSyntax for R64 {
    fn as_att_syntax(&self) -> String {
        use super::R64::*;
        match *self {
            rax => "%rax",
            rcx => "%rcx",
            rdx => "%rdx",
            rbx => "%rbx",

            rsp => "%rsp",
            rbp => "%rbp",
            rsi => "%rsi",
            rdi => "%rdi",

            r8 => "%r8",
            r9 => "%r9",
            r10 => "%r10",
            r11 => "%r11",
            r12 => "%r12",
            r13 => "%r13",
            r14 => "%r14",
            r15 => "%r15",
        }
        .to_owned()
    }
}

impl ATTSyntax for R32 {
    fn as_att_syntax(&self) -> String {
        use super::R32::*;
        match *self {
            eax => "%eax",
            ecx => "%ecx",
            edx => "%edx",
            ebx => "%ebx",

            esp => "%esp",
            ebp => "%ebp",
            esi => "%esi",
            edi => "%edi",

            r8d => "%r8d",
            r9d => "%r9d",
            r10d => "%r10d",
            r11d => "%r11d",
            r12d => "%r12d",
            r13d => "%r13d",
            r14d => "%r14d",
            r15d => "%r15d",
        }
        .to_owned()
    }
}

impl Enumerate for i32 {
    fn possible_enumerations() -> Vec<Self> {
        vec![0, 1, 127, 255, 0x7fffffff, -0x7fffffff, -127, -1]
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
        vec![S1, S2, S4, S8]
    }
}

impl ATTSyntax for Scale {
    fn as_att_syntax(&self) -> String {
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
                if i == R64::rsp {
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
                if b == R64::rsp {
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

enum Disp {
    I32(i32),
    U32(u32),
}

impl ATTSyntax for Disp {
    fn as_att_syntax(&self) -> String {
        use std::fmt::LowerHex;
        use std::ops::Neg;
        use num::Zero;

        fn format<A>(i: A) -> String
            where A: Neg<Output = A> + LowerHex + PartialOrd + Zero
        {
            if i < A::zero() {
                format!("-0x{:x}", -i)
            } else {
                format!("0x{:x}", i)
            }
        }

        match self {
            &Disp::I32(i) => {
                if is_imm8(i) {
                    format(i as i8)
                } else {
                    format(i)
                }
            }
            &Disp::U32(u) => {
                if u < 256 {
                    format!("0x{:x}", u as u8)
                } else {
                    format!("0x{:x}", u)
                }
            }
        }

    }
}

impl ATTSyntax for Addr {
    fn as_att_syntax(&self) -> String {
        let mut base = self.base().map_or("".to_owned(), |r| r.as_att_syntax());
        let mut index = "".to_owned();
        let mut scale = "".to_owned();

        let mb_disp = if let Some(disp) = self.disp() {
            Some(disp)
        } else if self.base().map_or(false, |b| b.erased().is_like_bp()) {
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
                disp = mb_disp.map_or("".to_owned(), |d| Disp::I32(d).as_att_syntax()),
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
    M(Addr),
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
            &Push::M(ref m) => {
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
            &Push::M(ref m) => buf.push(m),
        };
    }
}

impl Enumerate for Push {
    fn possible_enumerations() -> Vec<Self> {
        let r64s = R64::possible_enumerations().into_iter().map(|r| Push::R64(r));
        let i32s = i32::possible_enumerations().into_iter().map(|i| Push::I32(i));
        let m64s = Addr::possible_enumerations().into_iter().map(|a| Push::M(a));
        r64s.chain(i32s).chain(m64s).collect()
    }
}

// Pop

enum Pop {
    R64(R64),
    M(Addr),
}

impl Instr for Pop {
    fn emit(&self, buf: &mut Emit) {
        match self {
            &Pop::R64(r) => buf.pop(r),
            &Pop::M(ref m) => buf.pop(m),
        };
    }
}

impl Enumerate for Pop {
    fn possible_enumerations() -> Vec<Self> {
        R64::possible_enumerations()
            .into_iter()
            .map(|r| Pop::R64(r))
            .chain(Addr::possible_enumerations().into_iter().map(|m| Pop::M(m)))
            .collect()
    }
}

impl ATTSyntax for Pop {
    fn as_att_syntax(&self) -> String {
        let mut rator = "pop";
        let rand = match self {
            &Pop::R64(r) => r.as_att_syntax(),
            &Pop::M(ref m) => {
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

    AddR64M(R64, Addr),
    SubR64M(R64, Addr),
    CmpR64M(R64, Addr),

    AddMR64(Addr, R64),
    SubMR64(Addr, R64),
    CmpMR64(Addr, R64),
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

            &AddR64M(dst, ref src) => buf.add(dst, src),
            &SubR64M(dst, ref src) => buf.sub(dst, src),
            &CmpR64M(dst, ref src) => buf.cmp(dst, src),

            &AddMR64(ref dst, src) => buf.add(dst, src),
            &SubMR64(ref dst, src) => buf.sub(dst, src),
            &CmpMR64(ref dst, src) => buf.cmp(dst, src),
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
                res.push(AddR64M(*dst, src.clone()));
                res.push(SubR64M(*dst, src.clone()));
                res.push(CmpR64M(*dst, src.clone()));

                res.push(AddMR64(src.clone(), *dst));
                res.push(SubMR64(src.clone(), *dst));
                res.push(CmpMR64(src.clone(), *dst));
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
            &AddR64M(_, _) |
            &AddMR64(_, _) => "add",
            &SubR64R64(_, _) |
            &SubR64I32(_, _) |
            &SubMR64(_, _) |
            &SubR64M(_, _) => "sub",
            &CmpR64R64(_, _) |
            &CmpR64I32(_, _) |
            &CmpMR64(_, _) |
            &CmpR64M(_, _) => "cmp",
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

            &AddR64M(_, ref m) |
            &SubR64M(_, ref m) |
            &CmpR64M(_, ref m) => m.as_att_syntax(),

            &AddMR64(_, r) |
            &SubMR64(_, r) |
            &CmpMR64(_, r) => r.as_att_syntax(),
        }
    }

    fn dst_as_att_syntax(&self) -> String {
        use self::Arith::*;
        match self {
            &AddR64R64(r, _) |
            &AddR64I32(r, _) |
            &AddR64M(r, _) |
            &SubR64R64(r, _) |
            &SubR64I32(r, _) |
            &SubR64M(r, _) |
            &CmpR64R64(r, _) |
            &CmpR64I32(r, _) |
            &CmpR64M(r, _) => r.as_att_syntax(),
            &AddMR64(ref m, _) |
            &SubMR64(ref m, _) |
            &CmpMR64(ref m, _) => m.as_att_syntax(),
        }
    }
}

// Mov.

enum Mov {
    R64R64(R64, R64),
    R64I64(R64, i64),
    R64M(R64, Addr),
    R32M(R32, Addr),
    MR64(Addr, R64),
}

impl Instr for Mov {
    fn emit(&self, buf: &mut Emit) {
        use self::Mov::*;

        match self {
            &R64R64(dst, src) => buf.mov(dst, src),
            &R64I64(dst, src) => buf.mov(dst, src),
            &R64M(dst, ref src) => buf.mov(dst, src),
            &R32M(dst, ref src) => buf.mov(dst, src),
            &MR64(ref dst, src) => buf.mov(dst, src),
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
                res.push(R32M(r.to_r32(), m.clone()));
                res.push(R64M(*r, m.clone()));
                res.push(MR64(m, *r));
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
            &R64M(dst, ref src) => {
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &R32M(dst, ref src) => {
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &MR64(ref dst, src) => {
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
        };

        format!("{:7}{}", rator, rand)
    }
}

// Movsx

#[allow(non_camel_case_types)]
enum Movx {
    SBQ_RM(R64, Addr),
    SWQ_RM(R64, Addr),
    SLQ_RM(R64, Addr),
    ZBQ_RM(R64, Addr),
    ZWQ_RM(R64, Addr),
    ZLQ_RM(R64, Addr),
}

impl Instr for Movx {
    fn emit(&self, buf: &mut Emit) {
        use self::Movx::*;

        match self {
            &SBQ_RM(dst, ref src) => buf.movsb(dst, src),
            &SWQ_RM(dst, ref src) => buf.movsw(dst, src),
            &SLQ_RM(dst, ref src) => buf.movsl(dst, src),
            &ZBQ_RM(dst, ref src) => buf.movzb(dst, src),
            &ZWQ_RM(dst, ref src) => buf.movzw(dst, src),
            &ZLQ_RM(dst, ref src) => buf.movzl(dst, src),
        };
    }
}

impl Enumerate for Movx {
    fn possible_enumerations() -> Vec<Self> {
        use self::Movx::*;

        let mut res = vec![];
        for r in &R64::possible_enumerations() {
            for m in Addr::possible_enumerations() {
                res.push(SBQ_RM(*r, m.clone()));
                res.push(SWQ_RM(*r, m.clone()));
                res.push(SLQ_RM(*r, m.clone()));
                res.push(ZBQ_RM(*r, m.clone()));
                res.push(ZWQ_RM(*r, m.clone()));
                res.push(ZLQ_RM(*r, m.clone()));
            }
        }
        res
    }
}

impl ATTSyntax for Movx {
    fn as_att_syntax(&self) -> String {
        use self::Movx::*;
        let rator;
        let rand;

        match self {
            &SBQ_RM(dst, ref src) => {
                rator = "movsbq";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &SWQ_RM(dst, ref src) => {
                rator = "movswq";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &SLQ_RM(dst, ref src) => {
                rator = "movslq";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &ZBQ_RM(dst, ref src) => {
                rator = "movzbq";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &ZWQ_RM(dst, ref src) => {
                rator = "movzwq";
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
            &ZLQ_RM(dst, ref src) => {
                return Mov::R32M(dst.to_r32(), src.clone()).as_att_syntax()
            }
        };

        format!("{:7}{}", rator, rand)
    }
}

// Lea.

enum Lea {
    R64M(R64, Addr),
}

impl Instr for Lea {
    fn emit(&self, buf: &mut Emit) {
        use self::Lea::*;

        match self {
            &R64M(dst, ref src) => buf.lea(dst, src),
        };
    }
}

impl Enumerate for Lea {
    fn possible_enumerations() -> Vec<Self> {
        use self::Lea::*;

        let mut res = vec![];
        for r in &R64::possible_enumerations() {
            for m in Addr::possible_enumerations() {
                res.push(R64M(*r, m));
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
            &R64M(dst, ref src) => {
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
    JmpM(Addr),
    CallM(Addr),
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
            &JmpM(ref m) => {
                buf.jmp(m);
            }
            &CallM(ref m) => {
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
            res.push(JmpM(m.clone()));
            res.push(CallM(m));
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
            &JmpM(ref m) => {
                rator = "jmpq";
                rand = m.as_att_syntax();
            }
            &CallM(ref m) => {
                rator = "callq";
                rand = m.as_att_syntax();
            }
        }
        format!("{:7}*{}", rator, rand)
    }
}

impl Enumerate for Cond {
    fn possible_enumerations() -> Vec<Self> {
        use super::Cond::*;

        vec![E, NE, L, G, LE, GE]
    }
}

impl ATTSyntax for Cond {
    fn as_att_syntax(&self) -> String {
        use super::Cond::*;

        match *self {
            E => "e",
            NE => "ne",
            L => "l",
            G => "g",
            LE => "le",
            GE => "ge",
        }
        .to_owned()
    }
}

struct Jcc {
    cond: Cond,
    offset: i32,
    here: u32,
}

impl Instr for Jcc {
    fn emit(&self, buf: &mut Emit) {
        buf.jcc(self.cond, self.offset);
    }
}

fn jcc_insn_size(offset: i32) -> u32 {
    if is_imm8(offset) {
        2
    } else {
        6
    }
}

impl ATTSyntax for Jcc {
    fn as_att_syntax(&self) -> String {
        use std::num::Wrapping;

        let rator = format!("j{}", self.cond.as_att_syntax());

        let dst = Wrapping(self.here) + Wrapping(self.offset as u32) +
                  Wrapping(jcc_insn_size(self.offset));

        format!("{:7}{}", rator, Disp::U32(dst.0).as_att_syntax())
    }
}

impl Enumerate for Jcc {
    fn possible_enumerations() -> Vec<Self> {
        let mut res = vec![];
        let mut here = 0;
        for i in &i32::possible_enumerations() {
            for c in Cond::possible_enumerations() {
                res.push(Jcc {
                    cond: c,
                    offset: *i,
                    here: here,
                });
                here += jcc_insn_size(*i);
            }
        }
        res
    }
}

enum Cmovcc {
    R64R64(Cond, R64, R64),
}

impl Instr for Cmovcc {
    fn emit(&self, buf: &mut Emit) {
        match self {
            &Cmovcc::R64R64(cond, dst, src) => buf.cmovcc(cond, dst, src),
        };
    }
}

impl ATTSyntax for Cmovcc {
    fn as_att_syntax(&self) -> String {
        let cond;
        let rand;

        match self {
            &Cmovcc::R64R64(cond_, dst, src) => {
                cond = cond_;
                rand = format!("{},{}", src.as_att_syntax(), dst.as_att_syntax());
            }
        }

        let rator = format!("cmov{}", cond.as_att_syntax());

        format!("{:7}{}", rator, rand)
    }
}

impl Enumerate for Cmovcc {
    fn possible_enumerations() -> Vec<Self> {
        let mut res = vec![];
        for dst in &R64::possible_enumerations() {
            for src in &R64::possible_enumerations() {
                for c in Cond::possible_enumerations() {
                    res.push(Cmovcc::R64R64(c, *dst, *src));
                }
            }
        }
        res
    }
}

// Test utils.

#[allow(unused)]
fn assert_encoding<F>(expected: &[u8], f: F)
    where F: for<'a> FnOnce(&'a mut Emit) -> &'a mut Emit
{
    // Welp, I don't like this type signature...
    let mut e = Emit::new();
    f(&mut e);
    assert_eq!(e.as_ref(), expected);
}


fn assert_assembly_matches_disassembly<A: Instr + Enumerate>() {
    let instrs = A::possible_enumerations();
    let mut code_buf = Emit::to_vec(Vec::with_capacity(instrs.len()));
    for i in &instrs {
        i.emit(&mut code_buf);
    }
    let disas_lines = objdump_disas_lines(code_buf.as_ref());
    assert!(!disas_lines.is_empty());

    // line.len() > 20: filter out lines that are too short.
    for (disas_line, instr) in disas_lines.iter().filter(|line| line.len() > 20).zip(instrs) {
        let instr_att = instr.as_att_syntax();
        let mut instr_buf = Emit::new();
        instr.emit(&mut instr_buf);
        if !disas_line.contains(&instr_att) {
            dump_file("/tmp/assembler.a.out", code_buf.as_ref());
            assert!(false,
                    "{} (binary: {:?}) doesn't contain {}, see /tmp/assembler.a.out",
                    disas_line,
                    instr_buf.as_ref(),
                    instr_att);
        }
    }
}

// Tests.

#[test]
fn test_movx_matches_disassembly() {
    assert_assembly_matches_disassembly::<Movx>();
}

#[test]
fn test_mov_matches_disassembly() {
    assert_assembly_matches_disassembly::<Mov>();
}

#[test]
fn test_assembly_matches_disassembly() {
    assert_assembly_matches_disassembly::<Cmovcc>();
    assert_assembly_matches_disassembly::<Push>();
    assert_assembly_matches_disassembly::<Pop>();
    assert_assembly_matches_disassembly::<Arith>();

    assert_assembly_matches_disassembly::<Branch>();
    assert_assembly_matches_disassembly::<Ret>();
    assert_assembly_matches_disassembly::<Jcc>();
}

#[test]
fn test_jit_pushpop() {
    use mem::JitMem;
    let mut emit = Emit::new();
    emit.push(R64::rdi)
        .pop(R64::rax)
        .ret();
    let jitmem = JitMem::new(emit.as_ref());
    let arg = 12345;
    let res = unsafe { jitmem.call_ptr_ptr(arg) };
    assert_eq!(arg, res);
}

#[test]
fn test_jit_add() {
    use mem::JitMem;
    let mut emit = Emit::new();
    emit.push(0_i32)
        .pop(R64::rax)
        .add(R64::rax, R64::r8)
        .add(R64::rax, R64::r9)
        .ret();
    let jitmem = JitMem::new(emit.as_ref());
    let res = unsafe { jitmem.call_ptr6_ptr(0, 1, 2, 3, 4, 5) };
    assert_eq!(9, res);
}

#[test]
fn test_jit_call() {
    use mem::JitMem;
    let mut emit = Emit::new();
    extern "C" fn neg(a: i64) -> i64 {
        -a
    }

    emit.mov(R64::rax, R64::rdi)
        .mov(R64::rdi, R64::rsi)
        .jmp(R64::rax);
    let jitmem = JitMem::new(emit.as_ref());
    let res = unsafe { jitmem.call_ptr6_ptr(neg as isize, 1, 2, 3, 4, 5) };
    assert_eq!(-1, res);
}

#[test]
#[should_panic]
fn test_unbound_label_drop() {
    let mut emit = Emit::new();
    let mut label = Label::new();
    emit.call(&mut label);
}

fn make_fibo_code(emit: &mut Emit) {
    let mut fibo_entry = Label::new();
    let mut fibo_base_case = Label::new();

    emit.bind(&mut fibo_entry)
        .cmp(R64::rdi, 2)
        .jl(&mut fibo_base_case)
        .push(R64::rdi)
        .sub(R64::rdi, 1)
        .call(&mut fibo_entry)
        .pop(R64::rdi)
        .push(R64::rax)
        .sub(R64::rdi, 2)
        .call(&mut fibo_entry)
        .pop(R64::rdi)
        .add(R64::rax, R64::rdi)
        .ret()
        .bind(&mut fibo_base_case)
        .mov(R64::rax, R64::rdi)
        .ret();
}

#[test]
fn test_fibo() {
    let mut emit = Emit::new();
    make_fibo_code(&mut emit);
    let jitmem = JitMem::new(emit.as_ref());
    println!("Start = 0x{:x}", jitmem.start());
    let res = unsafe { jitmem.call_ptr_ptr(10) };
    assert_eq!(55, res);
}

// Benchmarks.

#[cfg(bench)]
mod bench {
    use test::Bencher;

    const ALLOC_SIZE: usize = 32000;

    #[bench]
    fn bench_emit_add(b: &mut Bencher) {
        b.iter(|| {
            let mut emit = Emit::to_vec(Vec::with_capacity(ALLOC_SIZE));
            for _ in 0..(ALLOC_SIZE / 3) {
                emit.add(R64::r8, R64::r9);
            }
            emit
        });
    }

    #[bench]
    fn bench_emit_ret(b: &mut Bencher) {
        b.iter(|| {
            let mut emit = Emit::to_vec(Vec::with_capacity(ALLOC_SIZE));
            for _ in 0..ALLOC_SIZE {
                emit.ret();
            }
            emit
        });
    }

    #[bench]
    fn bench_emit_push(b: &mut Bencher) {
        b.iter(|| {
            let mut emit = Emit::to_vec(Vec::with_capacity(ALLOC_SIZE));
            for _ in 0..(ALLOC_SIZE * 2 / 3) {
                emit.push(R64::r8);
            }
            emit
        });
    }

    #[bench]
    fn bench_emit_push_i32(b: &mut Bencher) {
        b.iter(|| {
            let mut emit = Emit::to_vec(Vec::with_capacity(ALLOC_SIZE));
            for _ in 0..(ALLOC_SIZE / 5) {
                emit.push(-1_i32);
            }
            emit
        });
    }

    #[bench]
    fn bench_emit_push_rm64(b: &mut Bencher) {
        b.iter(|| {
            let mut emit = Emit::to_vec(Vec::with_capacity(ALLOC_SIZE));
            for _ in 0..(ALLOC_SIZE / 8) {
                emit.push(&Addr::B(R64::r12));
                emit.push(&Addr::B(R64::r13));
            }
            emit
        });
    }

    #[bench]
    fn bench_emit_fibo(b: &mut Bencher) {
        let fibo_len = {
            let mut emit = Emit::new();
            make_fibo_code(&mut emit);
            emit.as_ref().len()
        };
        b.iter(|| {
            let mut emit = Emit::to_vec(Vec::with_capacity(ALLOC_SIZE));
            for _ in 0..(ALLOC_SIZE / fibo_len) {
                make_fibo_code(&mut emit);
            }
            emit
        });
    }

    #[bench]
    fn bench_exec_fibo(b: &mut Bencher) {
        let fibo = {
            let mut emit = Emit::new();
            make_fibo_code(&mut emit);
            JitMem::new(emit.as_ref())
        };
        b.iter(|| unsafe { fibo.call_ptr_ptr(30) });
    }

    #[bench]
    fn bench_emit_raw_vec(b: &mut Bencher) {
        b.iter(|| {
            let mut v = Vec::with_capacity(ALLOC_SIZE);
            for _ in 0..ALLOC_SIZE {
                v.push(0xff);
            }
            v
        });
    }
}

