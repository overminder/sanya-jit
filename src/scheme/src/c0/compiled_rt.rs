use rt::*;
use rt::oop::*;
use rt::oop_utils::*;

use std::mem::transmute;

pub unsafe extern "C" fn display_oop(oop: Oop, u: &Universe) {
    println!("{}", FmtOop(oop, u));
}

#[allow(unused)]
pub unsafe extern "C" fn eval_oop(_oop: Oop, _u: &Universe) -> Oop {
    panic!("eval_oop")
}

pub unsafe extern "C" fn panic(universe: &Universe, msg: Oop) {
    // Unwind the stack.
    println!("(panic!# {}) called, unwinding the stack.",
             FmtOop(msg, universe));
    for (frame_no, frame) in universe.iter_frame(universe.smt()).enumerate() {
        println!("Frame {}: {:?}", frame_no, frame);
        for (slot_no, oop_slot) in frame.iter_oop().enumerate() {
            let oop = *oop_slot;
            println!("  Slot {}: *{:#x} = {}",
                     slot_no,
                     transmute::<_, usize>(oop_slot),
                     FmtOop(oop, universe));
        }
    }
    panic!();
}

pub unsafe extern "C" fn full_gc(universe: &mut Universe, alloc_size: usize) -> usize {
    universe.full_gc(alloc_size)
}

pub unsafe extern "C" fn alloc_ooparray(universe: &mut Universe, len: Oop, fill: Oop) -> Oop {
    assert!(universe.oop_is_fixnum(len));
    let len: Handle<Fixnum> = universe.oop_handle(len);
    let fill: Handle<Closure> = universe.oop_handle(fill);
    universe.new_ooparray(len.value() as usize, &fill).as_oop()
}

pub unsafe extern "C" fn alloc_i64array(universe: &mut Universe, len: usize, fill: i64) -> Oop {
    universe.new_i64array(len, fill).as_oop()
}

pub unsafe extern "C" fn alloc_closure(u: &Universe, info_entry: usize, payloads: *mut Oop) -> Oop {
    trace!("alloc_closure: payloads = {:#x}", payloads as usize);
    let info = InfoTable::from_entry(info_entry);
    let closure = u.new_closure(info);
    trace!("alloc_closure: Allocated {}", FmtOop(closure.as_oop(), u));
    for (i, upval_slot) in closure.ptr_payloads().iter_mut().enumerate() {
        let src = payloads.offset(i as isize);
        trace!("  move payload: from *{:#x} ({:#x})", src as usize, *src);
        *upval_slot = *src;
        trace!("  move payload: to *{:#x} ({})",
               upval_slot as *mut _ as usize,
               FmtOop(*upval_slot, u));
    }
    closure.as_oop()
}

pub unsafe extern "C" fn compile_module(u: &Universe, module: Oop) -> Oop {
    use ast::sexpr_to_nir;
    use c0;
    use std::mem;

    let module = u.oop_handle(module);
    let sexpr = oop_to_sexpr(module, u);
    let scs = sexpr_to_nir::compile_sc(&[sexpr]).unwrap();
    let cm = c0::compile(&scs, u);
    let mut lm = c0::link(cm, u);
    let entry = lm.take_closure("entry");
    // XXX
    mem::forget(lm);
    *entry.oop()
}
