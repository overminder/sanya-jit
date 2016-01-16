use rt::*;
use rt::oop::*;
use rt::inlinesym::InlineSym;

use std::mem::transmute;
use std::fmt::{self, Debug, Formatter};
use std::io::Write;

unsafe fn fmt_oop(oop: Oop, u: &Universe, fmt: &mut Formatter) -> fmt::Result {
    if u.oop_is_fixnum(oop) {
        let i = Fixnum::from_raw(oop);
        try!(write!(fmt, "<Fixnum {}>", i.value));
    } else if u.oop_is_closure(oop) {
        let clo = Closure::from_raw(oop);
        try!(write!(fmt, "<Closure {} @{:#x}>", clo.info().name(), oop));
    } else if u.oop_is_ooparray(oop) {
        let arr = OopArray::from_raw(oop);
        try!(write!(fmt, "<OopArray ["));
        for (i, oop) in arr.content().iter().enumerate() {
            if i != 0 {
                try!(write!(fmt, ", "));
            }
            try!(fmt_oop(*oop, u, fmt));
        }
        try!(write!(fmt, "]>"));
    } else if u.oop_is_i64array(oop) {
        let arr = OopArray::from_raw(oop);
        try!(write!(fmt, "<I64Array ["));
        for (i, val) in arr.content().iter().enumerate() {
            if i != 0 {
                try!(write!(fmt, ", "));
            }
            try!(write!(fmt, "{}", val));
        }
        try!(write!(fmt, "]>"));
    } else {
        try!(write!(fmt, "<UnknownOop {:#x}>", oop));
    }
    Ok(())
}

struct FmtOop<'a>(Oop, &'a Universe);

impl<'a> Debug for FmtOop<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        unsafe { fmt_oop(self.0, self.1, fmt) }
    }
}

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
            println!("  Slot {}: *{:#x} = {}",
                     slot_no,
                     transmute::<_, usize>(oop_slot),
                     oop);
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
