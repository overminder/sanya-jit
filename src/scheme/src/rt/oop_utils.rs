use super::Universe;
use super::oop::*;
use ast::sexpr::SExpr;

use std::fmt::{self, Formatter, Display};

// Format impl

unsafe fn fmt_oop(oop: Oop, u: &Universe, fmt: &mut Formatter) -> fmt::Result {
    if oop == NULL_OOP {
        write!(fmt, "<null>")?;
    } else if Singleton::is_singleton(oop) {
        write!(fmt, "{:?}", Singleton::from_oop(oop).unwrap())?;
    } else if u.oop_is_fixnum(oop) {
        let i = Fixnum::from_raw(oop);
        write!(fmt, "{}", i.value())?;
    } else if u.oop_is_pair(oop) {
        let mut p = Pair::from_raw(oop);
        write!(fmt, "({}", FmtOop(p.car, u))?;
        while u.oop_is_pair(p.cdr) {
            p = Pair::from_raw(p.cdr);
            write!(fmt, " {}", FmtOop(p.car, u))?;
        }
        if Singleton::is_nil(p.cdr) {
            write!(fmt, ")")?;
        } else {
            write!(fmt, " . {})", FmtOop(p.cdr, u))?;
        }
    } else if u.oop_is_symbol(oop) {
        let s = Symbol::from_raw(oop);
        write!(fmt, "{}", s.as_str())?;
    } else if u.oop_is_closure(oop) {
        let clo = Closure::from_raw(oop);
        write!(fmt, "<Closure {} @{:#x}>", clo.info().name(), oop)?;
    } else if u.oop_is_closure(oop) {
        let mb = MutBox::from_raw(oop);
        write!(fmt, "<Box {} @{:#x}>", FmtOop(mb.value(), u), oop)?;
    } else if u.oop_is_ooparray(oop) {
        let arr = OopArray::from_raw(oop);
        write!(fmt, "[")?;
        for (i, oop) in arr.content().iter().enumerate() {
            if i != 0 {
                write!(fmt, ", ")?;
            }
            fmt_oop(*oop, u, fmt)?;
        }
        write!(fmt, "]")?;
    } else if u.oop_is_i64array(oop) {
        let arr = OopArray::from_raw(oop);
        write!(fmt, "i64[")?;
        for (i, val) in arr.content().iter().enumerate() {
            if i != 0 {
                write!(fmt, ", ")?;
            }
            write!(fmt, "{}", val)?;
        }
        write!(fmt, "]")?;
    } else {
        write!(fmt, "<UnknownOop {:#x}>", oop)?;
    }
    Ok(())
}

pub struct FmtOop<'a>(pub Oop, pub &'a Universe);

impl<'a> Display for FmtOop<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        unsafe { fmt_oop(self.0, self.1, fmt) }
    }
}

pub fn oop_to_sexpr(_oop: Handle<Closure>, _u: &Universe) -> SExpr {
    panic!("oop_to_sexpr: not implemenetd")
}
