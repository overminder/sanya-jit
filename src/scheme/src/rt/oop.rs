/// Ordinary object pointers and memory layouts.
/// (I did try to come up with a better name but failed.)

use std::boxed::Box;
use std::slice;
use std::ptr;
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::mem::transmute;

pub type Oop = usize;

pub const NULL_OOP: Oop = 0;

pub fn sizeof_ptrs(nptrs: usize) -> usize {
    nptrs * 8
}

#[repr(C)]
pub struct InfoTable<A> {
    ptr_payloads: u16, // GC ptrs
    word_payloads: u16, // Unmanaged qwords
    arity: u16,
    callable: u8,
    has_vararg: u8,
    name: *const u8,
    phantom_data: PhantomData<A>,
    entry: [u8; 0],
}

impl<A> InfoTable<A> {
    pub fn sizeof_instance(&self) -> usize {
        sizeof_ptrs(self.ptr_payloads as usize + self.word_payloads as usize + 1)
    }
}

fn mk_infotable_for_data<A>(nptrs: u16, nwords: u16, name: &'static str) -> InfoTable<A> {
    InfoTable {
        ptr_payloads: nptrs,
        word_payloads: nwords,
        arity: 0,
        callable: 0,
        has_vararg: 0,
        name: name.as_ptr(),
        phantom_data: PhantomData,
        entry: [],
    }
}

pub fn infotable_for_pair() -> InfoTable<Pair> {
    mk_infotable_for_data(2, 0, "<Pair>")
}

pub fn infotable_for_fixnum() -> InfoTable<Fixnum> {
    mk_infotable_for_data(0, 1, "<Fixnum>")
}

/// A Closure is not exactly an ordinary callable object in the narrow sense -
/// it's a generic heap object, with a info table and some payloads (fields).
/// Yes, we are using Haskell's nomenclature here.
#[repr(C)]
pub struct Closure {
    info: *const InfoTable<Closure>,
    payloads: [Oop; 0],
}

impl Closure {
    pub unsafe fn from_raw<'a>(oop: Oop) -> &'a mut Self {
        &mut *transmute::<Oop, *mut Self>(oop)
    }

    pub unsafe fn info(&self) -> &InfoTable<Self> {
        &*self.info
    }

    pub unsafe fn info_word(&self) -> &mut usize {
        &mut *(&self.info as *const _ as *mut _)
    }

    pub unsafe fn payload_start(&self) -> usize {
        self.payloads.as_ptr() as usize
    }

    pub unsafe fn ptr_payloads(&self) -> &mut [Oop] {
        let base = self.payload_start() as *mut _;
        let len = self.info().ptr_payloads as usize;
        slice::from_raw_parts_mut(base, len)
    }

    pub unsafe fn word_payloads(&self) -> &mut [usize] {
        let base = self.payload_start() + sizeof_ptrs(self.info().ptr_payloads as usize);
        let len = self.info().word_payloads as usize;
        slice::from_raw_parts_mut(base as *mut _, len)
    }

    pub unsafe fn as_word(&self) -> usize {
        transmute(self)
    }

    pub unsafe fn entry(&self) -> *const () {
        self.info().entry.as_ptr() as *const _
    }
}

#[repr(C)]
pub struct Fixnum {
    info: *const InfoTable<Fixnum>,
    pub value: isize,
}

#[repr(C)]
pub struct Pair {
    info: *const InfoTable<Fixnum>,
    pub car: Oop,
    pub cdr: Oop,
}

// Doubly-linked list of oops. Used to manage root of stacks.

pub struct Handle<A> {
    oop: Oop,
    prev: *mut OopHandle,
    next: *mut OopHandle,
    phantom_data: PhantomData<A>,
}

pub type OopHandle = Handle<Oop>;

pub struct HandleBlock(OopHandle);

// Just a marker.
pub trait IsOop : Sized {
    fn as_oop(&self) -> Oop {
        unsafe { transmute(self) }
    }
}

impl IsOop for Closure {}
impl IsOop for Fixnum {}
impl IsOop for Pair {}

impl HandleBlock {
    pub fn new() -> Box<HandleBlock> {
        let mut thiz = Box::new(HandleBlock(Handle {
            oop: NULL_OOP,
            prev: ptr::null_mut(),
            next: ptr::null_mut(),
            phantom_data: PhantomData,
        }));
        (*thiz).0.prev = (*thiz).0.as_ptr();
        (*thiz).0.next = (*thiz).0.as_ptr();
        thiz
    }

    pub fn head(&self) -> &OopHandle {
        &(*self).0
    }

    pub fn len(&self) -> usize {
        unsafe {
            let mut res = 0;
            self.head().foreach_oop(|_| res += 1);
            res
        }
    }

    pub fn new_handle<A: IsOop>(&self, a: *mut A) -> Box<Handle<A>> {
        unsafe { Handle::new(a, self.head()) }
    }

    pub fn new_ref_handle<A: IsOop>(&self, a: &A) -> Box<Handle<A>> {
        unsafe { Handle::new_ref(a, &self.0) }
    }
}


impl<A> Handle<A> {
    pub unsafe fn oop(&self) -> &mut Oop {
        &mut *(&self.oop as *const _ as *mut _)
    }

    unsafe fn same_ptr(&self, rhs: &OopHandle) -> bool {
        self.as_ptr() == rhs.as_ptr()
    }

    pub unsafe fn foreach_oop<F: FnMut(&mut Oop)>(&self, mut f: F) {
        let mut curr = self.next();
        loop {
            if self.same_ptr(curr) {
                break;
            }
            let next = curr.next();
            f(curr.oop());
            curr = next;
        }
    }

    unsafe fn next<'a, 'b>(&'a self) -> &'b mut OopHandle {
        &mut *self.next
    }

    #[allow(unused)]
    unsafe fn set_next(&self, next: *mut OopHandle) {
        (&mut *(self as *const _ as *mut Self)).next = next;
    }

    unsafe fn prev(&self) -> &mut OopHandle {
        &mut *self.prev
    }

    unsafe fn set_prev(&self, prev: *mut OopHandle) {
        (&mut *(self as *const _ as *mut Self)).prev = prev;
    }

    pub fn as_ptr(&self) -> *mut OopHandle {
        self as *const _ as *const _ as *mut _
    }

    unsafe fn new(oop: *mut A, head: &OopHandle) -> Box<Self> {
        let thiz = Box::new(Handle {
            oop: oop as Oop,
            prev: head.prev,
            next: head.as_ptr(),
            phantom_data: PhantomData,
        });
        head.prev().next = Handle::<A>::as_ptr(&*thiz);
        (*head).set_prev(thiz.as_ptr());
        thiz
    }

    unsafe fn new_ref(oop_ref: &A, head: &OopHandle) -> Box<Self> {
        Handle::new(oop_ref as *const _ as *mut A, head)
    }
}

impl<A> Drop for Handle<A> {
    fn drop(&mut self) {
        unsafe {
            self.next().prev = self.prev().as_ptr();
            self.prev().next = self.next().as_ptr();
        }
    }
}

impl<A> Deref for Handle<A> {
    type Target = A;
    fn deref(&self) -> &A {
        unsafe { &*(self.oop as *const A) }
    }
}

impl<A> DerefMut for Handle<A> {
    fn deref_mut(&mut self) -> &mut A {
        unsafe { &mut *(self.oop as *mut A) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn test_handle() {
        unsafe {
            let block = HandleBlock::new();
            {
                let mut oop = mem::zeroed::<Fixnum>();
                oop.value = 42;
                let foo1 = block.new_ref_handle(&oop);
                let _foo2 = block.new_ref_handle(&oop);
                let _foo3 = block.new_ref_handle(&oop);
                assert_eq!(&oop as *const _ as Oop, *foo1.oop());
                assert_eq!(oop.value, foo1.value);
                assert_eq!(3, block.len());
            }
            assert_eq!(0, block.len());
        }
    }
}
