/// Ordinary object pointers and memory layouts.
/// (I did try to come up with a better name but failed.)

use std::boxed::Box;
use std::slice;
use std::ptr;
use std::ffi::CStr;
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::mem::{size_of, transmute};
use std::fmt::{self, Formatter, Debug};

pub type Oop = usize;

pub const NULL_OOP: Oop = 0;

pub fn sizeof_ptrs(nptrs: usize) -> usize {
    nptrs * 8
}

#[repr(u16)]
#[derive(Eq, PartialEq)]
pub enum OopKind {
    Plain,
    Callable,
    OopArray,
}

#[repr(C)]
pub struct InfoTable<A> {
    ptr_payloads: u16, // GC ptrs
    word_payloads: u16, // Unmanaged qwords
    arity: u16,
    kind: OopKind,
    name: *const u8,
    entry: [u8; 0],
    phantom_data: PhantomData<A>,
}

impl<A> Debug for InfoTable<A> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "<InfoTable {}>", self.name())
    }
}

impl<A> InfoTable<A> {
    pub fn new(ptr_payloads: u16, word_payloads: u16, arity: u16,
               kind: OopKind, name: &'static str) -> Self {
        InfoTable {
            ptr_payloads: ptr_payloads,
            word_payloads: word_payloads,
            arity: arity,
            kind: kind,
            name: name.as_ptr(),
            entry: [],
            phantom_data: PhantomData,
        }
    }

    pub fn sizeof_instance(&self) -> usize {
        assert!(!self.is_array());
        sizeof_ptrs(self.ptr_payloads as usize + self.word_payloads as usize + 1)
    }

    pub fn sizeof_array_instance(&self, size: usize) -> usize {
        assert!(self.is_array());
        sizeof_ptrs(size + 2)
    }

    pub fn is_array(&self) -> bool {
        self.kind == OopKind::OopArray
    }

    pub fn name(&self) -> &'static str {
        unsafe { CStr::from_ptr(self.name as *const _).to_str().unwrap() }
    }

    pub unsafe fn from_entry<'a>(entry: usize) -> &'a Self {
        &*((entry - size_of::<Self>()) as *const _)
    }

    pub fn entry_word(&self) -> usize {
        self.entry.as_ptr() as usize
    }
}

fn mk_infotable_for_data<A>(nptrs: u16, nwords: u16, name: &'static str, kind: OopKind) -> InfoTable<A> {
    InfoTable {
        ptr_payloads: nptrs,
        word_payloads: nwords,
        arity: 0,
        kind: kind,
        name: name.as_ptr(),
        phantom_data: PhantomData,
        entry: [],
    }
}

pub fn infotable_for_pair() -> InfoTable<Pair> {
    mk_infotable_for_data(2, 0, "<Pair>", OopKind::Plain)
}

pub fn infotable_for_fixnum() -> InfoTable<Fixnum> {
    mk_infotable_for_data(0, 1, "<Fixnum>", OopKind::Plain)
}

pub fn infotable_for_ooparray() -> InfoTable<OopArray> {
    mk_infotable_for_data(0, 0, "<OopArray>", OopKind::OopArray)
}

/// A Closure is not exactly an ordinary callable object in the narrow sense -
/// it's a generic heap object, with a info table and some payloads (fields).
/// Yes, we are using Haskell's nomenclature here.
#[repr(C)]
pub struct Closure {
    // *info points directly to the entry field in the InfoTable.
    // See Haskell's `tables-next-to-code` trick.
    info: *const (),
    payloads: [Oop; 0],
}

impl Closure {
    pub unsafe fn info_is<A>(&self, info: &InfoTable<A>) -> bool {
        *self.entry_word() == info.entry_word()
    }

    pub unsafe fn from_raw<'a>(oop: Oop) -> &'a mut Self {
        &mut *transmute::<Oop, *mut Self>(oop)
    }

    pub unsafe fn info(&self) -> &InfoTable<Self> {
        InfoTable::<Self>::from_entry(*self.entry_word())
    }

    pub unsafe fn entry_word(&self) -> &mut usize {
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
}

#[repr(C)]
pub struct Fixnum {
    info: *const (),
    pub value: isize,
}

#[repr(C)]
pub struct Pair {
    info: *const (),
    pub car: Oop,
    pub cdr: Oop,
}

#[repr(C)]
pub struct OopArray {
    info: *const (),
    pub len: usize,
    pub content: [Oop; 0],
}

impl OopArray {
    pub fn len(&self) -> usize {
        self.len
    }

    pub unsafe fn content(&self) -> &mut [Oop] {
        slice::from_raw_parts_mut(self.content.as_ptr() as *mut _, self.len)
    }
}

// Doubly-linked list of oops. Used to manage root of stacks.

pub struct RawHandle<A> {
    oop: Oop,
    prev: *mut OopHandle,
    next: *mut OopHandle,
    phantom_data: PhantomData<A>,
}

pub type OopHandle = RawHandle<Oop>;

pub type Handle<A> = Box<RawHandle<A>>;

pub struct HandleBlock(OopHandle);

// Just a marker.
pub trait IsOop : Sized {
    unsafe fn as_oop(&self) -> Oop {
        transmute(self)
    }

    unsafe fn oop_cast<A: IsOop>(&self) -> &A {
        transmute(self)
    }
}

impl IsOop for Closure {}
impl IsOop for Fixnum {}
impl IsOop for Pair {}
impl IsOop for OopArray {}

impl HandleBlock {
    pub fn new() -> Box<HandleBlock> {
        let mut thiz = Box::new(HandleBlock(RawHandle {
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

    pub fn new_handle<A: IsOop>(&self, a: *mut A) -> Box<RawHandle<A>> {
        unsafe { RawHandle::new(a, self.head()) }
    }

    pub fn new_ref_handle<A: IsOop>(&self, a: &A) -> Box<RawHandle<A>> {
        unsafe { RawHandle::new_ref(a, &self.0) }
    }
}


impl<A> RawHandle<A> {
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
        let thiz = Box::new(RawHandle {
            oop: oop as Oop,
            prev: head.prev,
            next: head.as_ptr(),
            phantom_data: PhantomData,
        });
        head.prev().next = RawHandle::<A>::as_ptr(&*thiz);
        (*head).set_prev(thiz.as_ptr());
        thiz
    }

    unsafe fn new_ref(oop_ref: &A, head: &OopHandle) -> Box<Self> {
        RawHandle::new(oop_ref as *const _ as *mut A, head)
    }
}

impl<A> Drop for RawHandle<A> {
    fn drop(&mut self) {
        unsafe {
            self.next().prev = self.prev().as_ptr();
            self.prev().next = self.next().as_ptr();
        }
    }
}

impl<A> Deref for RawHandle<A> {
    type Target = A;
    fn deref(&self) -> &A {
        unsafe { &*(self.oop as *const A) }
    }
}

impl<A> DerefMut for RawHandle<A> {
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

    #[test]
    fn test_info_offsets() {
        unsafe {
            let info = infotable_for_pair();
            let entry = info.entry_word();
            assert_eq!(InfoTable::<Pair>::from_entry(entry).entry_word(), entry);
        }
    }
}
