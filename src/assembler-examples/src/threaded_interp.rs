use assembler::x64::{Addr, R64};
use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::x64::utils::objdump_disas_lines;
use assembler::mem::JitMem;
use assembler::emit::{Emit, Label};

use std::mem;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum Op {
    // stack.push(oparg_i8)
    LoadI8 = 0,

    // stack.push(stack[oparg_i8])
    LoadL,

    // r = stack.pop(); l = stack.pop(); if l < r then pc += oparg_i8
    BranchLt,

    // Callee clears the stack.
    // func_ix = oparg_u8; stack.push(pc); pc = func_table[oparg_u8]; locals = args
    Call,

    // res = stack.pop(); pc = stack.pop(); stack.pops(oparg_u8); stack.push(res)
    Ret,

    // rax = stack.pop(); halt
    Halt,

    // r = stack.pop(); l = stack.pop(); stack.push(l + r)
    Add,
}

#[derive(Debug, Clone, Copy)]
enum Instr {
    OpOnly(Op),
    OpWithArg(Op, i8),
}

fn instr_to_bs(is: &[Instr]) -> Vec<u8> {
    let mut bs = vec![];
    for i in is {
        match i {
            &Instr::OpOnly(op) => {
                bs.push(op as u8);
            }
            &Instr::OpWithArg(op, arg) => {
                bs.push(op as u8);
                bs.push(arg as u8);
            }
        }
    }
    bs
}

const LAST_OP: Op = Op::Add;

struct VMRegs {
    pc: R64,
    tmpl: R64,
    tmpr: R64,
    dispatch_table: R64,
    func_table: R64,
}

impl VMRegs {
    fn new() -> Self {
        VMRegs {
            pc: rdi,
            dispatch_table: rsi,
            func_table: rdx,
            tmpl: rcx,
            tmpr: r8,
        }
    }
}

impl Op {
    fn from_u8(repr: u8) -> Self {
        unsafe { mem::transmute(repr) }
    }

    fn build_dispatch_case(self, emit: &mut Emit, vr: &VMRegs) {
        use self::Op::*;

        match self {
            LoadI8 => {
                emit.movsb(vr.tmpl, &(vr.pc + 1))
                    .push(vr.tmpl);
                build_dispatch_skip_oparg(emit, vr);
            }
            LoadL => {
                emit.movsb(vr.tmpl, &(vr.pc + 1))
                    .push(&(rsp + vr.tmpl * 8));
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
                    .movsb(vr.tmpl, &(vr.pc + 1))
                    .add(vr.pc, vr.tmpl);
                build_dispatch_skip_oparg(emit, vr);
            }
            Call => {
                emit.movsb(vr.tmpl, &(vr.pc + 1))
                    .push(&(vr.pc + 2))
                    .mov(vr.pc, &(vr.func_table + vr.tmpl * 8));
                build_dispatch_correct_pc(emit, vr);
            }
            Ret => {
                emit.pop(vr.tmpl)
                    .movsb(vr.tmpr, &(vr.pc + 1))
                    .pop(vr.pc)
                    .lea(rsp, &(rsp + vr.tmpr * 8))
                    .push(vr.tmpl);
                build_dispatch_correct_pc(emit, vr);
            }
            Halt => {
                emit.pop(rax)
                    .ret();
            }
            Add => {
                emit.pop(vr.tmpr)
                    .add(&(rsp + 8), vr.tmpr);
                build_dispatch_with_pc_offset(emit, vr, 1);
            }
        }
    }
}

fn build_dispatch_with_pc_offset(emit: &mut Emit, vr: &VMRegs, offset: i32) {
    if offset == 0 {
        emit.movsb(vr.tmpl, &Addr::B(vr.pc));
    } else {
        emit.movsb(vr.tmpl, &(vr.pc + offset))
            .add(vr.pc, offset);
    }

    emit.jmp(&(vr.dispatch_table + vr.tmpl * 8));
}

fn build_dispatch_skip_oparg(emit: &mut Emit, vr: &VMRegs) {
    build_dispatch_with_pc_offset(emit, vr, 2)
}

fn build_dispatch_correct_pc(emit: &mut Emit, vr: &VMRegs) {
    build_dispatch_with_pc_offset(emit, vr, 0)
}

fn build_interp() -> (Vec<usize>, JitMem) {
    let mut emit = Emit::new();
    let vr = VMRegs::new();

    build_dispatch_correct_pc(&mut emit, &vr);
    let mut offset_table = vec![0; 1 + LAST_OP as u8 as usize];

    for op_ix in 0..LAST_OP as u8 {
        let op = Op::from_u8(op_ix);
        let mut op_lbl = Label::new();
        emit.bind(&mut op_lbl);
        op.build_dispatch_case(&mut emit, &vr);
        offset_table.insert(op_ix as usize, op_lbl.offset().unwrap());
    }

    let jm = JitMem::new(emit.as_ref());

    let entry = jm.as_word();
    let label_table = offset_table.iter().map(|o| entry + o).collect();

    println!("jm.entry = {:x}, len = {}", entry, emit.as_ref().len());
    for line in objdump_disas_lines(emit.as_ref()) {
        println!("{}", line);
    }

    (label_table, jm)
}

pub fn main() {
    use self::Instr::*;
    use self::Op::*;

    let (labels, jm) = build_interp();

    let main_code = instr_to_bs(&[
        OpWithArg(LoadI8, 20),
        OpWithArg(LoadI8, 22),
        OpOnly(Add),
        OpOnly(Halt),
    ]);

    let res = unsafe {
        let pc = main_code.as_ptr() as isize;
        let lbl_ptr = labels.as_ptr() as isize;
        jm.call_ptr6_ptr(pc, lbl_ptr, 0, 0, 0, 0)
    };
    println!("res = {}", res);
}
