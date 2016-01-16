use rt::*;
use rt::oop::*;
use rt::oop_utils::*;
use rt::inlinesym::InlineSym;

use std::mem::transmute;

pub unsafe extern "C" fn display_oop(oop: Oop, u: &Universe) {
    println!("{:?}", FmtOop(oop, u));
}

pub unsafe extern "C" fn panic_inline_sym(f: &Fixnum, universe: &Universe) {
    // Unwind the stack.
    let reason = InlineSym::from_word(f.value as usize);
    println!("Panic (cause = {}), Unwinding the stack.", reason.as_str());
    for (frame_no, frame) in universe.iter_frame(universe.smt()).unwrap().enumerate() {
        println!("Frame {}: {:?}", frame_no, frame);
        for (slot_no, oop_slot) in frame.iter_oop().enumerate() {
            let oop = *oop_slot;
            println!("  Slot {}: *{:#x} = {:?}",
                     slot_no,
                     transmute::<_, usize>(oop_slot),
                     FmtOop(oop, universe));
        }
    }

    panic!("panic_inline_sym: {}", reason.as_str());
}

pub unsafe extern "C" fn full_gc(universe: &mut Universe, alloc_size: usize) -> usize {
    universe.full_gc(alloc_size)
}

pub unsafe extern "C" fn alloc_ooparray(universe: &mut Universe, len: Oop, fill: Oop) -> Oop {
    assert!(universe.oop_is_fixnum(len));
    let len: Handle<Fixnum> = universe.oop_handle(len);
    let fill: Handle<Closure> = universe.oop_handle(fill);
    universe.new_ooparray(len.value as usize, &fill).as_oop()
}

pub unsafe extern "C" fn alloc_i64array(universe: &mut Universe, len: usize, fill: i64) -> Oop {
    universe.new_i64array(len, fill).as_oop()
}
