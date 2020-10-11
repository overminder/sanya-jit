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
    Any(SExpr),
}

impl Reloc {
    #[allow(dead_code)]
    pub fn of_fixnum(i: i64) -> Self {
        Reloc::Any(SExpr::Int(i))
    }

    pub fn of_bool(b: bool) -> Self {
        Reloc::Any(SExpr::Bool(b))
    }

    pub fn is_ptr(&self) -> bool {
        use self::Reloc::*;
        use ast::sexpr::SExpr::*;

        // XXX: Sync this with the oop defs.
        match self {
            &Global(ref _name) => true,
            &Any(ref e) => {
                match e {
                    &Bool(..) => false,
                    _ => true,
                }
            }
        }
    }

    pub unsafe fn reify(&self, globals: &GlobalTable, u: &Universe) -> Oop {
        use self::Reloc::*;

        match self {
            &Global(ref name) => globals[name].as_oop(),
            &Any(ref e) => reify_sexpr(e, u),
        }
    }
}

unsafe fn reify_sexpr(e: &SExpr, u: &Universe) -> Oop {
    use ast::sexpr::SExpr::*;

    match e {
        &Int(i) => u.new_fixnum(i as isize).as_oop(),
        &Sym(ref s) => u.new_symbol(Id::named(s)).as_oop(),
        &Bool(b) => {
            if b {
                Singleton::True
            } else {
                Singleton::False
            }
            .as_oop()
        }
        &List(ref es) => {
            let res: Handle<Closure> = u.oop_handle(Singleton::Nil.as_oop());

            if es.len() > 0 {
                panic!("Not implemented");
            }

            res.as_oop()
        }
    }
}

