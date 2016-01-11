/// Shared runtime data model and functions.

pub mod oop;
pub mod gc;
pub mod inlinesym;
pub mod stackmap;

use self::oop::*;
use self::gc::GcState;
use self::stackmap::{FrameIterator, StackMapTable};

use std::cell::UnsafeCell;
use std::mem::transmute;

// XXX: Use offsetof after https://github.com/rust-lang/rfcs/issues/1144 is implemented.
pub const OFFSET_OF_UNIVERSE_SAVED_RBP: i32 = 0 * 8;
pub const OFFSET_OF_UNIVERSE_BASE_RBP: i32 = 1 * 8;
pub const OFFSET_OF_UNIVERSE_SAVED_RIP: i32 = 2 * 8;
pub const OFFSET_OF_UNIVERSE_ALLOC_PTR: i32 = 3 * 8;
pub const OFFSET_OF_UNIVERSE_ALLOC_LIMIT: i32 = 4 * 8;

#[repr(C)]
pub struct Universe {
    // The caller's rbp, used for traversing the stack.
    saved_rbp: usize,

    // The frame below the compiled main function. Stack traversal stops here.
    base_rbp: usize,

    // The caller's rip, used together with rip-to-stackmap to find
    // the caller's stackmap.
    saved_rip: usize,

    pub gc: UnsafeCell<GcState>,

    // ^ The above fields should be changed with caution. Namely,
    // the field offsets (UNIVERSE_OFFSET_OF_*) need to be recalculated
    // after each change.
    pub smt: Option<StackMapTable>,

    handle_block: Box<HandleBlock>,

    // Well-known info tables.
    pub pair_info: InfoTable<Pair>,
    pub fixnum_info: InfoTable<Fixnum>,
    pub ooparray_info: InfoTable<OopArray>,
    pub i64array_info: InfoTable<I64Array>,
}

// XXX: Those should be unsafe.
impl Universe {
    pub fn new(heap_size: usize) -> Self {
        Universe {
            saved_rbp: 0,
            base_rbp: 0,
            saved_rip: 0,
            gc: unsafe { UnsafeCell::new(GcState::new(heap_size)) },
            smt: None,

            handle_block: HandleBlock::new(),

            pair_info: infotable_for_pair(),
            fixnum_info: infotable_for_fixnum(),
            ooparray_info: infotable_for_ooparray(),
            i64array_info: infotable_for_i64array(),
        }
    }

    pub unsafe fn gc_mut(&self) -> &mut GcState {
        &mut *self.gc.get()
    }

    pub fn iter_frame<'a, 'b>(&'b self,
                              smt: &'a Option<StackMapTable>)
                              -> Option<FrameIterator<'a>> {
        if self.saved_rbp == 0 {
            None
        } else {
            Some(FrameIterator::new(self.saved_rbp,
                                    self.saved_rip,
                                    smt.as_ref().unwrap(),
                                    self.base_rbp))
        }
    }

    pub fn as_ptr(&self) -> *const Self {
        self as *const _
    }

    pub fn oop_is_pair(&self, oop: Oop) -> bool {
        unsafe { Closure::from_raw(oop).info_is(&self.pair_info) }
    }

    pub fn oop_is_fixnum(&self, oop: Oop) -> bool {
        unsafe { Closure::from_raw(oop).info_is(&self.fixnum_info) }
    }

    pub fn oop_is_ooparray(&self, oop: Oop) -> bool {
        unsafe { Closure::from_raw(oop).info_is(&self.ooparray_info) }
    }

    pub fn new_pair(&self, car: Oop, cdr: Oop) -> Handle<Pair> {
        unsafe {
            let native_frames = self.iter_frame(&self.smt);
            let mut res = self.gc_mut().alloc(&self.pair_info, &self.handle_block, native_frames);
            res.car = car;
            res.cdr = cdr;
            res
        }
    }

    pub fn new_fixnum(&self, value: isize) -> Handle<Fixnum> {
        unsafe {
            let native_frames = self.iter_frame(&self.smt);
            let mut res = self.gc_mut().alloc(&self.fixnum_info, &self.handle_block, native_frames);
            res.value = value;
            res
        }
    }

    pub fn new_closure(&self, info: &InfoTable<Closure>) -> Handle<Closure> {
        unsafe {
            let native_frames = self.iter_frame(&self.smt);
            let mut res = self.gc_mut().alloc(&info, &self.handle_block, native_frames);
            res
        }
    }

    pub fn new_ooparray<A: IsOop>(&self, len: usize, fill: &Handle<A>) -> Handle<OopArray> {
        unsafe {
            let native_frames = self.iter_frame(&self.smt);
            let mut res = self.gc_mut().alloc_array(&self.ooparray_info,
                                                    len,
                                                    &self.handle_block,
                                                    native_frames);
            res.len = len;
            for ptr in res.content() {
                *ptr = fill.as_oop();
            }
            res
        }
    }

    pub fn new_i64array(&self, len: usize, fill: i64) -> Handle<I64Array> {
        unsafe {
            let native_frames = self.iter_frame(&self.smt);
            let mut res = self.gc_mut().alloc_array(&self.i64array_info,
                                                    len,
                                                    &self.handle_block,
                                                    native_frames);
            res.len = len;
            for ptr in res.content() {
                *ptr = fill;
            }
            res
        }
    }

    pub fn oop_handle<A: IsOop>(&self, oop: Oop) -> Handle<A> {
        unsafe { self.handle_block.new_handle(transmute(oop)) }
    }

    pub fn full_gc(&self, alloc_size: usize) -> usize {
        unsafe {
            let native_frames = self.iter_frame(&self.smt);
            self.gc_mut().full_gc(alloc_size, &self.handle_block, native_frames);
            self.gc_mut().unsafe_alloc(alloc_size)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::oop::*;

    #[test]
    fn test_gc_alloc() {
        let mut u = Universe::new(0x180);
        assert_eq!(u.pair_info.sizeof_instance(), 0x18);
        for _ in 0..0x10000 {
            let _h = u.new_pair(NULL_OOP, NULL_OOP);
        }
        unsafe {
            assert_eq!(u.gc_mut().available_spaces(), 0);
            assert_eq!(u.gc_mut().full_gc_count, 0x1000 - 1);
        }
    }

    #[test]
    fn test_gc_scavenge() {
        let mut u = Universe::new(0x200);
        let mut oops = vec![];
        assert_eq!(u.fixnum_info.sizeof_instance(), 0x10);

        for i in 0..0x10 {
            oops.push(u.new_fixnum(i));
        }
        unsafe {
            assert_eq!(oops.len(), 0x10);
            assert_eq!(u.gc_mut().available_spaces(), 0x100);

            for i in 0..0x10000 {
                let _h = u.new_fixnum(i);
                assert!(u.gc_mut().available_spaces() <= 0x100);
            }
            assert_eq!(u.gc_mut().available_spaces(), 0);
            assert_eq!(u.gc_mut().full_gc_count, 0x1000 - 1);
            assert_eq!(u.gc_mut().scavenged_ptr_count, 0x10 * (0x1000 - 1));
        }
    }

    #[test]
    fn test_ooparray() {
        let mut available_spaces = 0x200;
        let mut u = Universe::new(available_spaces);

        let fxn = u.new_fixnum(0);
        available_spaces -= 0x10;

        available_spaces -= 0x10 + (0x8 * 10);
        let arr = u.new_ooparray(10, &fxn);
        unsafe {
            assert_eq!(u.gc_mut().available_spaces(), available_spaces);

            for i in 0..0x1000 {
                let _h = u.new_ooparray(10, &fxn);
                assert!(u.gc_mut().available_spaces() <= available_spaces);
            }
        }
    }
}
