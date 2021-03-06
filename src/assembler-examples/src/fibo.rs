use assembler::x64::R64::*;
use assembler::x64::traits::*;
use assembler::mem::JitMem;
use assembler::emit::{Emit, Label};

fn make_fibo_code(emit: &mut Emit) {
    let mut fibo_entry = Label::new();
    let mut fibo_base_case = Label::new();

    emit.bind(&mut fibo_entry)
        .cmp(rdi, 2)
        .jl(&mut fibo_base_case)
        .push(rdi)
        .sub(rdi, 1)
        .call(&mut fibo_entry)
        .pop(rdi)
        .push(rax)
        .sub(rdi, 2)
        .call(&mut fibo_entry)
        .pop(rdi)
        .add(rax, rdi)
        .ret()
        .bind(&mut fibo_base_case)
        .mov(rax, rdi)
        .ret();
}

fn main() {
    let mut emit = Emit::new();
    make_fibo_code(&mut emit);
    let jitmem = JitMem::new(emit.as_ref());
    println!("fibo = 0x{:x}", unsafe { jitmem.start() });
    println!("fibo(40) = {}", unsafe { jitmem.call_ptr_ptr(40) });
}

