use super::Universe;
use super::oop::*;
use ast::sexpr::SExpr;

use std::fmt::{self, Formatter, Display};

// Format impl

unsafe fn fmt_oop(oop: Oop, u: &Universe, fmt: &mut Formatter) -> fmt::Result {
    if oop == NULL_OOP {
        try!(write!(fmt, "<null>"));
    } else if u.oop_is_fixnum(oop) {
        let i = Fixnum::from_raw(oop);
        try!(write!(fmt, "{}", i.value()));
    } else if u.oop_is_pair(oop) {
        let mut p = Pair::from_raw(oop);
        try!(write!(fmt, "({}", FmtOop(p.car, u)));
        while u.oop_is_pair(p.cdr) {
            p = Pair::from_raw(p.cdr);
            try!(write!(fmt, " {}", FmtOop(p.car, u)));
        }
        try!(write!(fmt, " . {})", FmtOop(p.cdr, u)));
    } else if u.oop_is_symbol(oop) {
        let s = Symbol::from_raw(oop);
        try!(write!(fmt, "{}", s.as_str()));
    } else if u.oop_is_closure(oop) {
        let clo = Closure::from_raw(oop);
        try!(write!(fmt, "<Closure {} @{:#x}>", clo.info().name(), oop));
    } else if u.oop_is_mutbox(oop) {
        let mb = MutBox::from_raw(oop);
        try!(write!(fmt, "<Box {} @{:#x}>", FmtOop(mb.value(), u), oop));
    } else if u.oop_is_ooparray(oop) {
        let arr = OopArray::from_raw(oop);
        try!(write!(fmt, "["));
        for (i, oop) in arr.content().iter().enumerate() {
            if i != 0 {
                try!(write!(fmt, ", "));
            }
            try!(fmt_oop(*oop, u, fmt));
        }
        try!(write!(fmt, "]"));
    } else if u.oop_is_i64array(oop) {
        let arr = OopArray::from_raw(oop);
        try!(write!(fmt, "i64["));
        for (i, val) in arr.content().iter().enumerate() {
            if i != 0 {
                try!(write!(fmt, ", "));
            }
            try!(write!(fmt, "{}", val));
        }
        try!(write!(fmt, "]"));
    } else {
        try!(write!(fmt, "<UnknownOop {:#x}>", oop));
    }
    Ok(())
}

pub struct FmtOop<'a>(pub Oop, pub &'a Universe);

impl<'a> Display for FmtOop<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        unsafe { fmt_oop(self.0, self.1, fmt) }
    }
}

pub fn oop_to_sexpr(oop: Handle<Closure>, u: &Universe) -> SExpr {
    panic!("oop_to_sexpr: not implemenetd")
}
