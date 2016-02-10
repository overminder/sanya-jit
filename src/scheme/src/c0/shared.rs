use ast::sexpr::SExpr;
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
    Any(SExpr),
}

impl Reloc {
    pub unsafe fn reify(&self, globals: &GlobalTable, u: &Universe) -> Oop {
        use self::Reloc::*;

        match self {
            &Global(ref name) => globals[name].as_oop(),
            &Fixnum(v) => u.new_fixnum(v as isize).as_oop(),
            &Any(ref e) => reify_sexpr(e, u),
            _ => panic!("Not implemented: {:?}", self),
        }
    }
}

unsafe fn reify_sexpr(e: &SExpr, u: &Universe) -> Oop {
    use ast::sexpr::SExpr::*;

    match e {
        &Int(i) => u.new_fixnum(i as isize).as_oop(),
        &Sym(ref s) => u.new_symbol(Id::named(s)).as_oop(),
        &List(ref es) => {
            panic!("Not implemented");
        }
    }
}
