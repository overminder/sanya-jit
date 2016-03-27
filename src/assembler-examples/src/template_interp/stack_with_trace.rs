use assembler::x64::{Addr, R64};
use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::emit::{Emit, Label};

use template_interp::shared::{Dispatchable, build_interp, breakpoint};

use std::mem;
use std::env;
use std::collections::HashSet;

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


#[repr(C)]
struct InterpContext {
    pc: isize,
    dispatch_table: isize,
    func_table: isize,
    trace_ctx: isize,
    sp: isize,
}

// Can we have offsetof?
const PC_OFFSET: i32 = 0;
const DT_OFFSET: i32 = 1 * 8;
const FT_OFFSET: i32 = 2 * 8;
const TC_OFFSET: i32 = 3 * 8;
const SP_OFFSET: i32 = 4 * 8;

impl InterpContext {
    fn location_mapping(base: R64, vr: &VMRegs) -> Vec<(R64, Addr)>{
        vec![
            (vr.pc, base + PC_OFFSET),
            (vr.dispatch_table, base + DT_OFFSET),
            (vr.func_table, base + FT_OFFSET),
            (vr.trace_ctx, base + TC_OFFSET),
            (vr.sp, base + SP_OFFSET),
        ]
    }

    fn build_unpack_to_reg(emit: &mut Emit, vr: &VMRegs) {
        // TODO
        let mut last_move: Option<Box<FnMut(&mut Emit)>> = None;
        for (r, m) in InterpContext::location_mapping(rdi, vr) {
            if r == rdi {
                last_move = Some(Box::new(move |emit| {
                    emit.mov(r, &m);
                }));
            } else {
                emit.mov(r, &m);
            }
        }
        if let Some(mut f) = last_move {
            f(emit);
        }
    }
}

#[derive(Debug)]
struct Trace {
    bc_list: Vec<isize>,
    bc_set: HashSet<isize>,
}

impl Trace {
    fn new() -> Self {
        Trace {
            bc_list: Default::default(),
            bc_set: Default::default(),
        }
    }

    // Returns false if this trace is finished.
    fn record(&mut self, bc_ptr: isize) -> bool {
        self.bc_list.push(bc_ptr);
        if self.bc_set.contains(&bc_ptr) {
            return false;
        }
        self.bc_set.insert(bc_ptr);
        true
    }
}

#[derive(Debug)]
struct TraceContext {
    current: Trace,
    traces: Vec<Trace>,
}

impl TraceContext {
    fn new() -> Box<Self> {
        Box::new(TraceContext {
            current: Trace::new(),
            traces: vec![],
        })
    }

    fn as_ptr(&self) -> *const Self {
        self as *const _
    }

    fn record(&mut self, bc_ptr: isize) {
        if !self.current.record(bc_ptr) {
            let mut new_trace = Trace::new();
            mem::swap(&mut self.current, &mut new_trace);
            self.traces.push(new_trace);

            // Shall we record the trace bridge?
            self.record(bc_ptr);
        }
    }

    unsafe extern "C" fn record_unsafe(raw: *mut TraceContext, bc_ptr: isize) {
        (&mut *raw).record(bc_ptr);
    }
}

struct VMRegs {
    pc: R64,
    tmpl: R64,
    tmpr: R64,
    dispatch_table: R64,
    func_table: R64,
    trace_ctx: R64,
    sp: R64,
    scratch: R64,
}

impl VMRegs {
    fn new() -> Self {
        VMRegs {
            pc: rdi,
            dispatch_table: rsi,
            func_table: rdx,
            tmpl: rcx,
            tmpr: r8,
            trace_ctx: r9,
            sp: r10,
            scratch: r11,
        }
    }

    fn regs(&self) -> Vec<R64> {
        vec![self.pc, self.dispatch_table, self.func_table, self.tmpr, self.tmpl, self.trace_ctx]
    }
}

struct Opts {
    duplicate_branch_op_tails: bool,
    trace_on: bool,
}

impl Opts {
    fn new() -> Self {
        Opts {
            duplicate_branch_op_tails: true,
            trace_on: false,
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
    fn build_interp_entry(emit: &mut Emit, args: &(VMRegs, Opts)) {
        InterpContext::build_unpack_to_reg(emit, &args.0);
    }

    fn build_dispatch_case(self, emit: &mut Emit, args: &(VMRegs, Opts)) {
        Op::build_dispatch_case(self, emit, &args.0, &args.1);
    }

    fn build_dispatch_with_pc_offset(emit: &mut Emit, args: &(VMRegs, Opts), offset: i32) {
        build_dispatch_with_pc_offset(emit, &args.0, &args.1, offset);
    }
}

impl Op {
    fn from_u8(repr: u8) -> Self {
        unsafe { mem::transmute(repr) }
    }

    fn build_dispatch_case(self, emit: &mut Emit, vr: &VMRegs, opts: &Opts) {
        use self::Op::*;

        fn push_r(emit: &mut Emit, vr: &VMRegs, r: R64) {
            emit.mov(&(vr.sp + (-8)), r)
                .sub(vr.sp, 8);
        }

        fn push_a(emit: &mut Emit, vr: &VMRegs, a: &Addr) {
            emit.mov(vr.scratch, a)
                .mov(&(vr.sp + (-8)), vr.scratch)
                .sub(vr.sp, 8);
        }

        fn pop_r(emit: &mut Emit, vr: &VMRegs, r: R64) {
            emit.mov(r, &Addr::B(vr.sp))
                .add(vr.sp, 8);
        }

        match self {
            LoadI8 => {
                emit.movsb(vr.tmpl, &(vr.pc + 1));
                push_r(emit, vr, vr.tmpl);
                build_dispatch_skip_oparg(emit, vr, opts);
            }
            LoadL => {
                emit.movsb(vr.tmpl, &(vr.pc + 1));
                push_a(emit, vr, &(vr.sp + vr.tmpl * 8));
                build_dispatch_skip_oparg(emit, vr, opts);
            }
            BranchLt => {
                pop_r(emit, vr, vr.tmpr);
                pop_r(emit, vr, vr.tmpl);
                emit.cmp(vr.tmpl, vr.tmpr);

                if opts.duplicate_branch_op_tails {
                    let mut lt = Label::new();
                    emit.jl(&mut lt);
                    build_dispatch_skip_oparg(emit, vr, opts);
                    emit.bind(&mut lt)
                        .movsb(vr.tmpl, &(vr.pc + 1))
                        .add(vr.pc, vr.tmpl);
                    build_dispatch_skip_oparg(emit, vr, opts);
                } else {
                    let mut ge = Label::new();
                    emit.jge(&mut ge)
                        .movsb(vr.tmpl, &(vr.pc + 1))
                        .add(vr.pc, vr.tmpl)
                        .bind(&mut ge);
                    build_dispatch_skip_oparg(emit, vr, opts);
                }
            }
            Call => {
                emit.movsb(vr.tmpl, &(vr.pc + 1))
                    .add(vr.pc, 2);

                push_r(emit, vr, vr.pc);
                emit.mov(vr.pc, &(vr.func_table + vr.tmpl * 8));
                build_dispatch_correct_pc(emit, vr, opts);
            }
            Ret => {
                pop_r(emit, vr, vr.tmpl);
                emit.movsb(vr.tmpr, &(vr.pc + 1));
                pop_r(emit, vr, vr.pc);
                emit.lea(vr.sp, &(vr.sp + vr.tmpr * 8));
                push_r(emit, vr, vr.tmpl);
                build_dispatch_correct_pc(emit, vr, opts);
            }
            Halt => {
                pop_r(emit, vr, rax);
                emit.ret();
            }
            Add => {
                pop_r(emit, vr, vr.tmpr);
                emit.add(&Addr::B(vr.sp), vr.tmpr);
                build_dispatch_with_pc_offset(emit, vr, opts, 1);
            }
        }
    }

}

fn build_dispatch_with_pc_offset(emit: &mut Emit, vr: &VMRegs, opts: &Opts, offset: i32) {
    if offset == 0 {
        emit.movsb(vr.tmpl, &Addr::B(vr.pc));
    } else {
        emit.movsb(vr.tmpl, &(vr.pc + offset))
            .add(vr.pc, offset);
    }

    // Like PyPy's jit_merge_point
    if opts.trace_on {
        let rs = vr.regs();
        for r in &rs {
            emit.push(*r);
        }

        let mut moar_pops: Box<FnMut(&mut Emit)> = if rs.len() % 2 == 0 {
            // Stack alignment
            emit.push(rax);
            Box::new(|emit| { emit.pop(rax); })
        } else {
            Box::new(|_| ())
        };

        emit.mov(rdi, vr.trace_ctx)
            .mov(rsi, vr.pc)
            .mov(rax, unsafe { mem::transmute::<_, i64>(TraceContext::record_unsafe) })
            .call(rax);

        moar_pops(emit);

        for r in rs.iter().rev() {
            emit.pop(*r);
        }
    }

    emit.jmp(&(vr.dispatch_table + vr.tmpl * 8));
}

fn build_dispatch_skip_oparg(emit: &mut Emit, vr: &VMRegs, opts: &Opts) {
    build_dispatch_with_pc_offset(emit, vr, opts, 2)
}

fn build_dispatch_correct_pc(emit: &mut Emit, vr: &VMRegs, opts: &Opts) {
    build_dispatch_with_pc_offset(emit, vr, opts, 0)
}


pub fn main(n: u8) {
    use self::Instr::*;
    use self::Op::*;

    let mut opts = Opts::new();
    if env::var("NO_DUP_BR_TAILS").is_ok() {
        opts.duplicate_branch_op_tails = false;
    };

    if env::var("TRACE_ON").is_ok() {
        opts.trace_on = true;
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

    let trace_ctx = TraceContext::new();

    let stack = [0_isize; 1024];

    let res = unsafe {
        let pc = main_code.as_ptr() as isize;
        let lbl_ptr = labels.as_ptr() as isize;
        let func_table_ptr = func_table.as_ptr() as isize;
        let trace_ctx_ptr = trace_ctx.as_ptr() as isize;
        let stack_ptr = stack.as_ptr().offset(stack.len() as isize) as isize;
        let ictx = InterpContext {
            pc: pc,
            func_table: func_table_ptr,
            trace_ctx: trace_ctx_ptr,
            dispatch_table: lbl_ptr,
            sp: stack_ptr,
        };
        if env::var("BREAK").is_ok() {
            breakpoint();
        }
        jm.call_ptr_ptr(&ictx as *const _ as isize)
    };
    println!("res = {}", res);
    println!("trace = {:?}", trace_ctx);
}
