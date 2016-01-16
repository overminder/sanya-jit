use super::Universe;
use super::oop::*;

use std::fmt::{self, Formatter, Debug};

// Format impl

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

pub struct FmtOop<'a>(pub Oop, pub &'a Universe);

impl<'a> Debug for FmtOop<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        unsafe { fmt_oop(self.0, self.1, fmt) }
    }
}
