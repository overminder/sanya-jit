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

// `impl<'a, T, Given: Trait<T>> Trait<&'a T> for Given` works.
// 
// `impl<'a, T, Given: Trait<&'a T>> Trait<T> for Given` doesn't work
// currently - even in GHC it will only work under UndecidableInstances,
// since the instance constraint (Trait<&'a T>) is larger than the
// instance head (Trait<T>). Therefore, to determine whether Given has
// an instance of Trait<T>, rustc will check whether Given has an instance
// of Trait<&'a T>. To check that, rustc will check whether Given has an
// instance of Trait<&&'a T>, and so on...
//
// impl<'a, Op: Clone, Buf: EmitPush<Op>> EmitPush<&'a Op> for Buf {
//     fn push(&mut self, op: &'a Op) -> &mut Self {
//         self.push(op.clone())
//     }
// }

pub trait EmitPop<Op> {
    fn pop(&mut self, op: Op) -> &mut Self;
}

pub trait EmitRet {
    fn ret(&mut self) -> &mut Self;
}

pub trait EmitArith<Dst, Src> {
    fn add(&mut self, dst: Dst, src: Src) -> &mut Self;
    fn sub(&mut self, dst: Dst, src: Src) -> &mut Self;
    fn cmp(&mut self, dst: Dst, src: Src) -> &mut Self;
}
