use assembler::x64::R64;
use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::mem::JitMem;
use assembler::emit::{Emit, Label};

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum Op {
    // stack.push(oparg_i8)
    LoadI8 = 0,

    // r = stack.pop(); l = stack.pop(); if l < r then pc += oparg_i8
    BranchLt,

    // res = stack.pop(); dump.pop(); stack.push(res)
    Ret,

    // func_ix = stack.pop(); args = stack.pops(oparg_u8); dump.save(); pc = funcs[func_ix]; locals = args
    Call,

    // r = stack.pop(); l = stack.pop(); stack.push(l + r)
    Add,
}

struct VMRegs {
    pc: R64,
    tmpl: R64,
    tmpr: R64,
    dispatch_table: R64,
}

impl Op {
    fn build_case(self, emit: &mut Emit, vr: &VMRegs) {
        use self::Op::*;

        match self {
            LoadI8 => {
                emit.movsxb(vr.tmpl, &(vr.pc + 1))
                    .push(vr.tmpl);
                build_dispatch_skip_oparg(emit, vr);
            }
            BranchLt => {
                let mut lt = Label::new();
                emit.pop(vr.tmpr)
                    .pop(vr.tmpl)
                    .cmp(vr.tmpl, vr.tmpr)
                    .jl(&mut lt);
                build_dispatch_skip_oparg(emit, vr);
                emit.bind(&mut lt)
                    .movsxb(vr.tmpl, &(vr.pc + 1))
                    .add(vr.pc, vr.tmpl);
                build_dispatch_skip_oparg(emit, vr);
            }
        }
    }
}

fn build_dispatch_skip_oparg(emit: &mut Emit, vr: &VMRegs) {
    emit.movsxb(vr.tmpl, &(vr.pc + 2))
        .add(vr.pc, 2)
        .mov(vr.tmpl, &(vr.dispatch_table + vr.tmpl * 8))
        .jmp(vr.tmpl);
}

fn build_interp() -> (Vec<usize>, JitMem) {
    let mut emit = Emit::new();
    emit.jmp(&(RDI + RSI * 8));
    let mut lbl0 = Label::new();
    emit.bind(&mut lbl0);
    emit.mov(RAX, 42);
    emit.ret();
    let mut lbl1 = Label::new();
    emit.bind(&mut lbl1);
    emit.mov(RAX, 4242);
    emit.ret();
    let jm = JitMem::new(emit.as_ref());

    let entry = jm.as_word();

    (vec![entry + lbl0.offset().unwrap(), entry + lbl1.offset().unwrap()], jm)
}

fn main() {
    let (labels, jm) = build_interp();
    let res = unsafe {
        let lbl_ptr = labels.as_ptr();
        jm.call_ptr6_ptr(lbl_ptr as isize, 1, 0, 0, 0, 0)
    };
    println!("res = {}", res);
}
