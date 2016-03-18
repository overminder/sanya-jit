use assembler::x64::{Addr, R64};
use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::emit::{Emit, Label};

use template_interp::shared::{Dispatchable};

use std::mem;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct Instr {
    op: Op,
    a: i8,
    imm: [i8; 2],
}

#[derive(Debug, Clone, Copy)]
enum Rator {
    A_SX,
    B_SX,
    C_SX,
    I_SX,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum Op {
    // stack[A] = Imm_i16
    LoadI16 = 0,

    // stack[A] += Imm_i16
    AddI,

    // if stack[B] < stack[C]: pc += A_i8
    BLtI,

    // Callee clears the stack.
    // stack[A] = call(func_table[B_u8])
    // The [A] slot will actually be used by the matching return instr.
    Call,

    // Directly consumed by the call instr.
    Enter,

    // t = stack[A]; clean up the stack by popping 2 (retval + saved pc) + Imm_u32 slots; stack[saved_pc.A] = t;
    Ret,

    // rax = stack[A]; halt
    Halt,

    // stack[A] = stack[B] + stack[C];
    Add,
}

impl From<u8> for Op {
    fn from(repr: u8) -> Self {
        unsafe { mem::transmute(repr) }
    }
}

impl From<Op> for u8 {
    fn from(repr: Op) -> Self {
        repr as u8
    }
}

struct VMRegs {
    pc: R64,
    dispatch_table: R64,
    func_table: R64,
    tmpa: R64,
    tmpb: R64,
    tmpc: R64,
    bp: R64,
    sp: R64,
}

impl VMRegs {
    fn new() -> Self {
        VMRegs {
            pc: rdi,
            dispatch_table: rsi,
            func_table: rdx,
            tmpa: rcx,
            tmpb: r8,
            tmpc: r9,
            bp: rbp,
            sp: rsp,
        }
    }
}

fn dispatch_next(emit: &mut Emit, vr: &VMRegs) {
    Op::build_dispatch_with_pc_offset(emit, vr, 4);
}

fn local_var(vr: &VMRegs, ix: R64) -> Addr {
    vr.bp + ix * 8
}

fn load_rator(emit: &mut Emit, vr: &VMRegs, rator: Rator) {
    use self::Rator::*;
    match rator {
        A_SX => {}
        B_SX => {
            emit.movsb(vr.tmpb, &(vr.pc + 2));
        }
        C_SX => {
            emit.movsb(vr.tmpc, &(vr.pc + 3));
        }
        I_SX => {
            emit.movsw(vr.tmpb, &(vr.pc + 2));
        }
    }
}

impl Dispatchable<VMRegs> for Op {
    fn build_dispatch_case(self, emit: &mut Emit, vr: &VMRegs) {
        use self::Op::*;
        match self {
            LoadI16 => {
                load_rator(emit, vr, Rator::I_SX);
                emit.mov(&local_var(vr, vr.tmpa), vr.tmpb);
            }
            AddI => {
                load_rator(emit, vr, Rator::I_SX);
                emit.add(&local_var(vr, vr.tmpa), vr.tmpb);
            }
            BLtI => {
                let mut lt = Label::new();
                load_rator(emit, vr, Rator::B_SX);
                emit.mov(vr.tmpa, &local_var(vr, vr.tmpa))
                    .cmp(vr.tmpa, &local_var(vr, vr.tmpb))
                    .jl(&mut lt);
                dispatch_next(emit, vr);

                load_rator(emit, vr, Rator::C_SX);
                emit.bind(&mut lt)
                    .add(vr.pc, &local_var(vr, vr.tmpc));
                dispatch_next(emit, vr);
            }
            _ => {}
        }
        dispatch_next(emit, vr);
    }

    // Also decodes instr.A.
    fn build_dispatch_with_pc_offset(emit: &mut Emit, vr: &VMRegs, offset: i32) {
        if offset == 0 {
            emit.movsb(vr.tmpc, &Addr::B(vr.pc));
            emit.movsb(vr.tmpa, &(vr.pc + 1));
        } else {
            emit.movsb(vr.tmpc, &(vr.pc + offset))
                .movsb(vr.tmpa, &(vr.pc + (offset + 1)))
                .add(vr.pc, offset);
        }

        emit.jmp(&(vr.dispatch_table + vr.tmpc * 8));
    }
}
