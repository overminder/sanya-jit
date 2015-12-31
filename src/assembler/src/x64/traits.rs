/// Basically a way to do multi-dispatch on different kinds of operands.

use super::consts::Cond;

// Instructions.

pub trait EmitPush<Op> {
    fn push(&mut self, op: Op) -> &mut Self;
}

pub trait EmitPop<Op> {
    fn pop(&mut self, op: Op) -> &mut Self;
}

pub trait EmitArith<Dst, Src> {
    fn add(&mut self, dst: Dst, src: Src) -> &mut Self;
    fn sub(&mut self, dst: Dst, src: Src) -> &mut Self;
    fn cmp(&mut self, dst: Dst, src: Src) -> &mut Self;
}

pub trait EmitMov<Dst, Src> {
    fn mov(&mut self, dst: Dst, src: Src) -> &mut Self;
}

pub trait EmitLea<Dst, Src> {
    fn lea(&mut self, dst: Dst, src: Src) -> &mut Self;
}

// Control flows.

pub trait EmitBranch<Op> {
    fn jmp(&mut self, op: Op) -> &mut Self;
    fn call(&mut self, op: Op) -> &mut Self;
}

pub trait EmitJcc<Op> {
    fn jcc(&mut self, cond: Cond, op: Op) -> &mut Self;

    fn je(&mut self, op: Op) -> &mut Self {
        self.jcc(Cond::E, op)
    }

    fn jne(&mut self, op: Op) -> &mut Self {
        self.jcc(Cond::NE, op)
    }

    fn jl(&mut self, op: Op) -> &mut Self {
        self.jcc(Cond::L, op)
    }

    fn jge(&mut self, op: Op) -> &mut Self {
        self.jcc(Cond::GE, op)
    }

    fn jle(&mut self, op: Op) -> &mut Self {
        self.jcc(Cond::LE, op)
    }

    fn jg(&mut self, op: Op) -> &mut Self {
        self.jcc(Cond::G, op)
    }
}

pub trait EmitRet {
    fn ret(&mut self) -> &mut Self;
}
