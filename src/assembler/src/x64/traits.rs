/// Basically a way to do multi-dispatch on different kinds of operands.

use byteorder::{ByteOrder, NativeEndian};

pub struct Emit(pub Vec<u8>);

impl Emit {
    pub fn write_byte(&mut self, b: u8) {
        self.0.push(b)
    }

    pub fn write_bytes(&mut self, bs: &[u8]) {
        // A simple loop suits well for instruction sequences (bs.len() < 8).
        for b in bs {
            self.0.push(*b);
        }
    }

    pub fn write_i32(&mut self, i: i32) {
        let mut buf = [0; 4];
        NativeEndian::write_i32(&mut buf, i);
        self.write_bytes(&buf);
    }

    pub fn write_i64(&mut self, i: i64) {
        let mut buf = [0; 8];
        NativeEndian::write_i64(&mut buf, i);
        self.write_bytes(&buf);
    }

    pub fn take(self) -> Vec<u8> {
        self.0
    }

    pub fn inner_ref(&self) -> &[u8] {
        &self.0
    }
}

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

pub trait EmitRet {
    fn ret(&mut self) -> &mut Self;
}
