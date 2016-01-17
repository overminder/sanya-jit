use ast::id::Id;
use rt::oop::*;
use rt::*;

use std::collections::HashMap;

pub type GlobalTable = HashMap<Id, Handle<Closure>>;

pub type RelocTable = Vec<(usize, Reloc)>;

pub type InfoRefs = Vec<usize>;

#[derive(Debug)]
pub enum Reloc {
    Global(Id),
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
