extern crate assembler;

use assembler::x64::{Emit, Label};
use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::mem::JitMem;

fn make_fibo_code(emit: &mut Emit) {
    let mut fibo_entry = Label::new();
    let mut fibo_base_case = Label::new();

    emit.bind(&mut fibo_entry)
        .cmp(RDI, 2)
        .jl(&mut fibo_base_case)
        .push(RDI)
        .sub(RDI, 1)
        .call(&mut fibo_entry)
        .pop(RDI)
        .push(RAX)
        .sub(RDI, 2)
        .call(&mut fibo_entry)
        .pop(RDI)
        .add(RAX, RDI)
        .ret()
        .bind(&mut fibo_base_case)
        .mov(RAX, RDI)
        .ret();
}

fn main() {
    let mut emit = Emit::new();
    make_fibo_code(&mut emit);
    let jitmem = JitMem::new(emit.as_ref());
    println!("fibo = 0x{:x}", unsafe { jitmem.start() });
    println!("fibo(40) = {}", unsafe { jitmem.call_ptr_ptr(40) });
}

