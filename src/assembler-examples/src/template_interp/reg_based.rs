use assembler::x64::{Addr, R64};
use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::emit::{Emit, Label};

use template_interp::shared::{Dispatchable, build_interp, breakpoint};

use std::mem;
use std::env;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct Instr {
    op: Op,
    a: i8,
    imm: [i8; 2],
}

impl Instr {
    fn op_a(op: Op, a: i8) -> Self {
        Instr::op_ai(op, a, 0)
    }

    fn op_i(op: Op, i: i16) -> Self {
        Instr::op_ai(op, 0, i)
    }

    fn op_ai(op: Op, a: i8, i: i16) -> Self {
        Instr {
            op: op,
            a: a,
            imm: unsafe { mem::transmute(i) },
        }
    }

    fn op_abc(op: Op, a: i8, b: i8, c: i8) -> Self {
        Instr {
            op: op,
            a: a,
            imm: [b, c],
        }
    }

    fn op_ab(op: Op, a: i8, b: i8) -> Self {
        Instr::op_abc(op, a, b, 0)
    }
}

fn instrs_to_bs(is: &[Instr]) -> Vec<u8> {
    let mut res = vec![];
    for i in is {
        res.push(i.op as u8);
        res.push(i.a as u8);
        res.push(i.imm[0] as u8);
        res.push(i.imm[1] as u8);
    }
    res
}


#[allow(non_camel_case_types)]
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

    // stack[A] = stack[B] + C_i16
    AddI,

    // stack[A] = stack[B] + stack[C];
    Add,

    // if stack[A] < stack[B]: pc += C_i8
    BLt,

    // if stack[A] < B: pc += C_i8
    BLtI,

    // Callee clears the stack.
    // stack[B] = call(func_table[A_u8])
    // The [B] slot will actually be used by the matching return instr.
    Call,

    // Usually directly consumed by the call instr. Still need to generate
    // impl for it since it can also happen in the entry point.
    // sp += 8 * I_i16
    Enter,

    // t = stack[A]; sp += 8 * I_i16; pc = stack.pop(); stack[pc.B] = t;
    Ret,

    // rax = stack[A]; sp += 8 * I_i16; halt
    Halt,
}

const LAST_OP: Op = Op::Halt;

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
            sp: rsp,
        }
    }
}

const INSTR_SIZE: i32 = 4;

fn dispatch_next_pc_offset(emit: &mut Emit, vr: &VMRegs, offset: i32) {
    if offset == 0 {
        emit.movsb(vr.tmpc, &Addr::B(vr.pc));
        emit.movsb(vr.tmpa, &(vr.pc + 1));
    } else {
        emit.movsb(vr.tmpc, &(vr.pc + offset * INSTR_SIZE))
            .movsb(vr.tmpa, &(vr.pc + (offset * INSTR_SIZE + 1)))
            .add(vr.pc, offset * INSTR_SIZE);
    }

    emit.jmp(&(vr.dispatch_table + vr.tmpc * 8));
}

fn dispatch_next(emit: &mut Emit, vr: &VMRegs) {
    dispatch_next_pc_offset(emit, vr, 1);
}

fn sp_ref(vr: &VMRegs, ix: R64) -> Addr {
    vr.sp + ix * 8
}

fn load_rator(emit: &mut Emit, vr: &VMRegs, rator: Rator) -> R64 {
    use self::Rator::*;
    match rator {
        A_SX => {
            vr.tmpa
        }
        B_SX => {
            emit.movsb(vr.tmpb, &(vr.pc + 2));
            vr.tmpb
        }
        C_SX => {
            emit.movsb(vr.tmpc, &(vr.pc + 3));
            vr.tmpc
        }
        I_SX => {
            emit.movsw(vr.tmpb, &(vr.pc + 2));
            vr.tmpb
        }
    }
}

fn build_dispatch_case(op: Op, emit: &mut Emit, vr: &VMRegs) {
    use self::Op::*;
    match op {
        LoadI16 => {
            let i = load_rator(emit, vr, Rator::I_SX);
            emit.mov(&sp_ref(vr, vr.tmpa), i);
        }
        AddI => {
            let b = load_rator(emit, vr, Rator::B_SX);
            let c = load_rator(emit, vr, Rator::C_SX);
            emit.mov(b, &sp_ref(vr, b))
                .add(b, c)
                .mov(&sp_ref(vr, vr.tmpa), b);
        }
        Add => {
            let b = load_rator(emit, vr, Rator::B_SX);
            let c = load_rator(emit, vr, Rator::C_SX);
            emit.mov(b, &sp_ref(vr, b))
                .add(b, &sp_ref(vr, c))
                .mov(&sp_ref(vr, vr.tmpa), b);
        }
        BLt => {
            let mut lt = Label::new();
            let b = load_rator(emit, vr, Rator::B_SX);
            emit.mov(vr.tmpa, &sp_ref(vr, vr.tmpa))
                .cmp(vr.tmpa, &sp_ref(vr, b))
                .jl(&mut lt);
            dispatch_next(emit, vr);

            emit.bind(&mut lt);
            let c = load_rator(emit, vr, Rator::C_SX);
            emit.lea(vr.pc, &(vr.pc + c * (INSTR_SIZE as u8) + INSTR_SIZE));
            dispatch_next_pc_offset(emit, vr, 0);
            return;
        }
        BLtI => {
            let mut lt = Label::new();
            let b = load_rator(emit, vr, Rator::B_SX);
            emit.cmp(&sp_ref(vr, vr.tmpa), b)
                .jl(&mut lt);
            dispatch_next(emit, vr);

            emit.bind(&mut lt);
            let c = load_rator(emit, vr, Rator::C_SX);
            emit.lea(vr.pc, &(vr.pc + c * (INSTR_SIZE as u8) + INSTR_SIZE));
            dispatch_next_pc_offset(emit, vr, 0);
            return;
        }
        Call => {
            emit.push(vr.pc)
                .mov(vr.pc, &(vr.func_table + vr.tmpa * 8));
            let i = load_rator(emit, vr, Rator::I_SX);
            emit.lea(vr.sp, &(vr.sp + i * 8));
        }
        Enter => {
            let i = load_rator(emit, vr, Rator::I_SX);
            emit.lea(vr.sp, &(vr.sp + i * 8));
        }
        Ret => {
            let i = load_rator(emit, vr, Rator::I_SX);
            emit.mov(vr.tmpa, &sp_ref(vr, vr.tmpa))
                .lea(vr.sp, &(vr.sp + i * 8))
                .pop(vr.pc);
            let b = load_rator(emit, vr, Rator::B_SX);
            emit.mov(&sp_ref(vr, b), vr.tmpa);
        }
        Halt => {
            let i = load_rator(emit, vr, Rator::I_SX);
            emit.mov(rax, &sp_ref(vr, vr.tmpa))
                .lea(vr.sp, &(vr.sp + i * 8))
                .ret();
            return;
        }
    }
    dispatch_next(emit, vr);
}

impl Dispatchable<VMRegs> for Op {
    fn build_dispatch_case(self, emit: &mut Emit, vr: &VMRegs) {
        build_dispatch_case(self, emit, vr)
    }

    // Also decodes instr.A.
    fn build_dispatch_with_pc_offset(emit: &mut Emit, vr: &VMRegs, offset: i32) {
        dispatch_next_pc_offset(emit, vr, offset);
    }
}

pub fn main(n: u8) {
    use self::Op::*;

    let (labels, jm) = build_interp(LAST_OP, &VMRegs::new());

    let main_code = instrs_to_bs(&[
        Instr::op_i(Enter, -1),
        Instr::op_ai(LoadI16, 0, n as i16),
        Instr::op_ab(Call, 2 /* fibo */, 0 /* ->sp[0] */),
        Instr::op_ai(Halt, 0, 1),
    ]);

    let retk_code = instrs_to_bs(&[
        Instr::op_i(Enter, -100),
        Instr::op_ai(LoadI16, 99, 42),
        Instr::op_ai(Ret, 99, 100),
    ]);

    let fibo_code = instrs_to_bs(&[
        Instr::op_i(Enter, -2),
        Instr::op_abc(BLtI, 3, 2, 6),

        // Recur case.
        Instr::op_abc(AddI, 0, 3, -1),
        Instr::op_ab(Call, 2, 1),
        Instr::op_abc(AddI, 0, 3, -2),
        Instr::op_ab(Call, 2, 0),
        Instr::op_abc(Add, 0, 0, 1),
        Instr::op_ai(Ret, 0, 2),

        // Base case.
        Instr::op_ai(Ret, 3, 2),
    ]);

    let func_table = [
        main_code.as_ptr(),
        retk_code.as_ptr(),
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
