/// Fast identifier operations using string interning.
/// Mostly from rust's libsyntax.

use std::rc::Rc;
use std::collections::HashMap;
use std::cell::RefCell;
use std::fmt::{self, Formatter, Debug, Display};

#[derive(Clone, Copy, Eq, Ord, Hash, PartialEq, PartialOrd)]
pub struct Id(usize);

impl Id {
    pub fn as_str(self) -> &'static str {
        unsafe { &*(get_interner().find(self) as *const _) }
    }

    pub fn named(s: &str) -> Self {
        get_interner().intern(s)
    }

    pub fn fresh(tag: &str) -> Self {
        get_interner().gensym(tag)
    }
}

impl Debug for Id {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        write!(fmt, "{}", self.as_str())
    }
}

impl Display for Id {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        write!(fmt, "{}", self.as_str())
    }
}

pub struct Interner {
    map: RefCell<HashMap<String, Id>>,
    vect: RefCell<Vec<String>>,
}

impl Interner {
    fn new() -> Self {
        Interner {
            map: Default::default(),
            vect: Default::default(),
        }
    }

    pub fn intern(&self, s: &str) -> Id {
        let mut map = self.map.borrow_mut();
        match map.get(s) {
            Some(id) => return *id,
            None => (),
        }

        let mut vect = self.vect.borrow_mut();

        let new_id = Id(vect.len());
        map.insert(s.to_owned(), new_id);
        vect.push(s.to_owned());
        new_id
    }

    pub fn gensym(&self, tag: &str) -> Id {
        let mut vect = self.vect.borrow_mut();
        let fresh_id = Id(vect.len());
        vect.push(format!("#{}:{}", tag, fresh_id.0));
        fresh_id
    }

    pub fn find(&self, id: Id) -> &str {
        unsafe { &*(&self.vect.borrow()[id.0] as &str as *const _) }
    }
}

fn get_interner() -> Rc<Interner> {
    thread_local!(static KEY: Rc<Interner> = {
        Rc::new(Interner::new())
    });
    KEY.with(|k| k.clone())
}
