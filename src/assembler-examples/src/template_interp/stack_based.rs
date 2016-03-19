use assembler::x64::{Addr, R64};
use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::emit::{Emit, Label};

use template_interp::shared::{Dispatchable, build_interp, breakpoint};

use std::mem;
use std::env;

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

struct Opts {
    duplicate_branch_op_tails: bool,
}

impl Opts {
    fn new() -> Self {
        Opts {
            duplicate_branch_op_tails: true,
        }
    }
}

impl From<u8> for Op {
    fn from(repr: u8) -> Self {
        Op::from_u8(repr)
    }
}

impl From<Op> for u8 {
    fn from(repr: Op) -> Self {
        repr as u8
    }
}

impl Dispatchable<(VMRegs, Opts)> for Op {
    fn build_dispatch_case(self, emit: &mut Emit, args: &(VMRegs, Opts)) {
        Op::build_dispatch_case(self, emit, &args.0, &args.1);
    }

    fn build_dispatch_with_pc_offset(emit: &mut Emit, args: &(VMRegs, Opts), offset: i32) {
        Op::build_dispatch_with_pc_offset(emit, &args.0, offset);
    }
}

impl Op {
    fn from_u8(repr: u8) -> Self {
        unsafe { mem::transmute(repr) }
    }

    fn build_dispatch_case(self, emit: &mut Emit, vr: &VMRegs, opts: &Opts) {
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
                emit.pop(vr.tmpr)
                    .pop(vr.tmpl)
                    .cmp(vr.tmpl, vr.tmpr);

                if opts.duplicate_branch_op_tails {
                    let mut lt = Label::new();
                    emit.jl(&mut lt);
                    build_dispatch_skip_oparg(emit, vr);
                    emit.bind(&mut lt)
                        .movsb(vr.tmpl, &(vr.pc + 1))
                        .add(vr.pc, vr.tmpl);
                    build_dispatch_skip_oparg(emit, vr);
                } else {
                    let mut ge = Label::new();
                    emit.jge(&mut ge)
                        .movsb(vr.tmpl, &(vr.pc + 1))
                        .add(vr.pc, vr.tmpl)
                        .bind(&mut ge);
                    build_dispatch_skip_oparg(emit, vr);
                }
            }
            Call => {
                emit.movsb(vr.tmpl, &(vr.pc + 1))
                    .add(vr.pc, 2)
                    .push(vr.pc)
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
                    .add(&Addr::B(rsp), vr.tmpr);
                Op::build_dispatch_with_pc_offset(emit, vr, 1);
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
}


fn build_dispatch_skip_oparg(emit: &mut Emit, vr: &VMRegs) {
    Op::build_dispatch_with_pc_offset(emit, vr, 2)
}

fn build_dispatch_correct_pc(emit: &mut Emit, vr: &VMRegs) {
    Op::build_dispatch_with_pc_offset(emit, vr, 0)
}


pub fn main(n: u8) {
    use self::Instr::*;
    use self::Op::*;

    let mut opts = Opts::new();
    if env::var("NO_DUP_BR_TAILS").is_ok() {
        opts.duplicate_branch_op_tails = false;
    };

    let (labels, jm) = build_interp(LAST_OP, &(VMRegs::new(), opts));

    let main_code = instr_to_bs(&[
        OpWithArg(LoadI8, n as i8),
        OpWithArg(Call, 1),
        OpOnly(Halt),
    ]);

    let id_code = instr_to_bs(&[
        OpWithArg(LoadL, 1),
        OpWithArg(Ret, 1),
    ]);

    let fibo_code = instr_to_bs(&[
        OpWithArg(LoadL, 1),
        OpWithArg(LoadI8, 2),
        OpWithArg(BranchLt, 17),

        // Recur case.
        OpWithArg(LoadL, 1),
        OpWithArg(LoadI8, -1),
        OpOnly(Add),
        OpWithArg(Call, 1),

        OpWithArg(LoadL, 2),
        OpWithArg(LoadI8, -2),
        OpOnly(Add),
        OpWithArg(Call, 1),

        OpOnly(Add),
        OpWithArg(Ret, 1),

        // Base case.
        OpWithArg(LoadL, 1),
        OpWithArg(Ret, 1),
    ]);

    let func_table = [
        id_code.as_ptr(),
        fibo_code.as_ptr(),
    ];

    let res = unsafe {
        let pc = main_code.as_ptr() as isize;
        let lbl_ptr = labels.as_ptr() as isize;
        let func_table_ptr = func_table.as_ptr() as isize;
        if env::var("BREAK").is_ok() {
            breakpoint();
        }
        jm.call_ptr6_ptr(pc, lbl_ptr, func_table_ptr, 0, 0, 0)
    };
    println!("res = {}", res);
}
