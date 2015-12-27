use std::io::{self, Write};

/// Basically a way to overload instruction methods.

pub struct Emit<A: Write>(A);

pub fn emit<A: Write>(a: A) -> Emit<A> {
    Emit(a)
}

impl<A: Write> Write for Emit<A> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

impl<A: Write> Emit<A> {
    pub fn into_inner(self) -> A {
        self.0
    }

    pub fn inner_mut(&mut self) -> &mut A {
        &mut self.0
    }
}

pub trait EmitPush<Op> {
    fn push(&mut self, op: Op) -> &mut Self;
}

pub trait EmitPop<Op> {
    fn pop(&mut self, op: Op) -> &mut Self;
}

pub trait EmitRet {
    fn ret(&mut self) -> &mut Self;
}

pub trait EmitAdd<Dst, Src> {
    fn add(&mut self, dst: Dst, src: Src) -> &mut Self;
}
