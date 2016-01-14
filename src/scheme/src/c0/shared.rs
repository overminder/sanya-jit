use rt::oop::*;
use rt::*;

use std::collections::HashMap;

pub type GlobalTable = HashMap<String, Handle<Closure>>;

pub type RelocTable = Vec<(usize, Reloc)>;

#[derive(Debug)]
pub enum Reloc {
    Global(String),
    Bool(bool),
    Fixnum(i64),
}

impl Reloc {
    pub unsafe fn reify(&self, globals: &GlobalTable, u: &Universe) -> Oop {
        use self::Reloc::*;

        match self {
            &Global(ref name) => globals[name].as_oop(),
            &Fixnum(v) => u.new_fixnum(v as isize).as_oop(),
            _ => panic!("Not implemented: {:?}", self),
        }
    }
}
