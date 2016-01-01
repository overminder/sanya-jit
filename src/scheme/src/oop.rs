/// Ordinary object pointers and memory layouts.
/// (I did try to come up with a better name but failed.)

use std::boxed::Box;
use std::slice;
use std::ptr;
use std::marker::PhantomData;

pub type Oop = usize;

pub const NULL_OOP: Oop = 0;

#[repr(C)]
pub struct InfoTable {
    ptr_payloads: u16, // GC ptrs
    word_payloads: u16, // Unmanaged qwords
    arity: u16,
    callable: u8,
    has_vararg: u8,
    name: *const u8,
    entry: [u8; 0],
}

#[repr(C)]
pub struct Closure {
    info: *const InfoTable,
    payloads: [Oop; 0],
}

impl Closure {
    pub unsafe fn ptr_payloads(&self) -> &mut [Oop] {
        let base = self.payloads.as_ptr() as *mut _;
        let len = self.info().ptr_payloads as usize;
        slice::from_raw_parts_mut(base, len)
    }

    pub unsafe fn info(&self) -> &InfoTable {
        &*self.info
    }

    pub unsafe fn word_payloads(&self) -> &mut [usize] {
        let base = (self.payloads.as_ptr() as *const _ as *mut usize)
                       .offset(self.info().ptr_payloads as isize);
        let len = self.info().word_payloads as usize;
        slice::from_raw_parts_mut(base, len)
    }

    pub unsafe fn entry(&self) -> *const () {
        self.info().entry.as_ptr() as *const _
    }
}

#[repr(C)]
pub struct Int {
    info: *const InfoTable,
    value: isize,
}

#[repr(C)]
pub struct Pair {
    info: *const InfoTable,
    car: Oop,
    cdr: Oop,
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
pub trait IsOop {}
impl IsOop for Closure {}
impl IsOop for Int {}
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

    fn head(&self) -> &OopHandle {
        &(*self).0
    }

    pub fn len(&self) -> usize {
        unsafe {
            let mut res = 0;
            self.head().foreach(|_| res += 1);
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

    pub unsafe fn same_ptr(&self, rhs: &OopHandle) -> bool {
        self.as_ptr() == rhs.as_ptr()
    }

    pub unsafe fn foreach<F: FnMut(&OopHandle)>(&self, mut f: F) {
        let mut curr = self.next();
        loop {
            if self.same_ptr(curr) {
                break;
            }
            let next = curr.next();
            f(curr);
            curr = next;
        }
    }

    pub unsafe fn next<'a, 'b>(&'a self) -> &'b mut OopHandle {
        &mut *self.next
    }

    pub unsafe fn set_next(&self, next: *mut OopHandle) {
        (&mut *(self as *const _ as *mut Self)).next = next;
    }

    pub unsafe fn prev(&self) -> &mut OopHandle {
        &mut *self.prev
    }

    pub fn as_ptr(&self) -> *mut OopHandle {
        self as *const _ as *const _ as *mut _
    }

    unsafe fn new(oop: *mut A, head: &OopHandle) -> Box<Self> {
        let thiz = Box::new(Handle {
            oop: oop as Oop,
            prev: head.as_ptr(),
            next: head.next,
            phantom_data: PhantomData,
        });
        head.next().prev = thiz.as_ptr();
        (*head).set_next(thiz.as_ptr());
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn test_handle() {
        unsafe {
            let block = HandleBlock::new();
            {
                let oop = mem::zeroed::<Int>();
                let foo1 = block.new_ref_handle(&oop);
                let _foo2 = block.new_ref_handle(&oop);
                let _foo3 = block.new_ref_handle(&oop);
                assert_eq!(&oop as *const _ as Oop, *foo1.oop());
                assert_eq!(3, block.len());
            }
            assert_eq!(0, block.len());
        }
    }
}
