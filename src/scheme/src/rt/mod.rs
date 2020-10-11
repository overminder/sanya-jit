/// Shared runtime data model and functions.

pub mod oop;
pub mod oop_utils;
pub mod gc;
pub mod inlinesym;
pub mod stackmap;

use ast::id::Id;
use self::oop::*;
use self::gc::{GcState, FullGcArgs};
use self::stackmap::{NativeInvocationChain, FrameIterator, StackMapTable};

use std::ptr;
use std::cell::UnsafeCell;
use std::mem::transmute;

// XXX: Use offsetof after https://github.com/rust-lang/rfcs/issues/1144 is implemented.
pub const OFFSET_OF_UNIVERSE_INVOCATION_CHAIN: i32 = 0 * 8;
pub const OFFSET_OF_UNIVERSE_ALLOC_PTR: i32 = 1 * 8;
pub const OFFSET_OF_UNIVERSE_ALLOC_LIMIT: i32 = 2 * 8;

#[repr(C)]
pub struct Universe {
    // The current list of cross-runtime calls.
    invocation_chain: *const NativeInvocationChain,

    pub gc: UnsafeCell<GcState>,

    // ^ The above fields should be changed with caution. Namely,
    // the field offsets (UNIVERSE_OFFSET_OF_*) need to be recalculated
    // after each change.
    smt: *const StackMapTable,
    empty_smt: Box<StackMapTable>,

    handle_block: Box<HandleBlock>,

    // Well-known info tables.
    pub pair_info: InfoTable<Pair>,
    pub fixnum_info: InfoTable<Fixnum>,
    pub symbol_info: InfoTable<Symbol>,
    pub ooparray_info: InfoTable<OopArray>,
    pub i64array_info: InfoTable<I64Array>,
    pub box_info: InfoTable<MutBox>,

    compiled_infos: *mut Vec<*const ClosureInfo>,
    empty_compiled_infos: Box<Vec<*const ClosureInfo>>,
}

// XXX: Those should be unsafe.
impl Universe {
    pub fn new(heap_size: usize) -> Box<Self> {
        let empty_smt = Box::new( Default::default());
        let empty_compiled_infos = Box::new( Default::default());

        let res = Box::new( Universe {
            invocation_chain: ptr::null(),
            gc: unsafe { UnsafeCell::new(GcState::new(heap_size)) },
            smt: &*empty_smt,
            empty_smt: empty_smt,

            handle_block: HandleBlock::new(),

            pair_info: infotable_for_pair(),
            fixnum_info: infotable_for_fixnum(),
            symbol_info: infotable_for_symbol(),
            ooparray_info: infotable_for_ooparray(),
            i64array_info: infotable_for_i64array(),
            box_info: infotable_for_box(),

            compiled_infos: &*empty_compiled_infos as *const _ as *mut _,
            empty_compiled_infos: empty_compiled_infos,
        });
        unsafe {
            res.gc_mut().set_universe(&*res);
        }
        res
    }

    pub unsafe fn gc_mut(&self) -> &mut GcState {
        &mut *self.gc.get()
    }

    pub fn iter_frame<'a, 'b>(&'b self, smt: &'a StackMapTable) -> FrameIterator<'a> {
        FrameIterator::new(smt, self.invocation_chain)
    }

    pub fn set_smt(&mut self, smt: &StackMapTable) {
        self.smt = smt as *const _;
    }

    pub fn smt<'a, 'b>(&'a self) -> &'b StackMapTable {
        unsafe { &*self.smt }
    }

    pub fn set_compiled_infos(&mut self, infos: &mut Vec<*const ClosureInfo>) {
        self.compiled_infos = infos as *mut _;
    }

    pub fn compiled_infos(&self) -> &mut Vec<*const ClosureInfo> {
        unsafe { &mut *self.compiled_infos }
    }

    fn gcargs(&self) -> FullGcArgs {
        FullGcArgs {
            handle_block: &*self.handle_block,
            native_frames: Some(self.iter_frame(self.smt())),
            compiled_infos: Some(self.compiled_infos()),
        }
    }

    pub fn as_ptr(&self) -> *const Self {
        self as *const _
    }

    pub fn oop_is_ptr(&self, oop: Oop) -> bool {
        !Singleton::is_singleton(oop)
    }

    pub fn oop_is_pair(&self, oop: Oop) -> bool {
        self.oop_is_ptr(oop) && unsafe { Closure::from_raw(oop).info_is(&self.pair_info) }
    }

    pub fn oop_is_mutbox(&self, oop: Oop) -> bool {
        self.oop_is_ptr(oop) && unsafe { Closure::from_raw(oop).info_is(&self.box_info) }
    }

    pub fn oop_is_fixnum(&self, oop: Oop) -> bool {
        self.oop_is_ptr(oop) && unsafe { Closure::from_raw(oop).info_is(&self.fixnum_info) }
    }

    pub fn oop_is_symbol(&self, oop: Oop) -> bool {
        self.oop_is_ptr(oop) && unsafe { Closure::from_raw(oop).info_is(&self.symbol_info) }
    }

    pub fn oop_is_ooparray(&self, oop: Oop) -> bool {
        self.oop_is_ptr(oop) && unsafe { Closure::from_raw(oop).info_is(&self.ooparray_info) }
    }

    pub fn oop_is_i64array(&self, oop: Oop) -> bool {
        self.oop_is_ptr(oop) && unsafe { Closure::from_raw(oop).info_is(&self.i64array_info) }
    }

    pub fn oop_is_closure(&self, oop: Oop) -> bool {
        self.oop_is_ptr(oop) && unsafe { Closure::from_raw(oop).info().is_closure() }
    }

    pub fn new_pair(&self, car: Oop, cdr: Oop) -> Handle<Pair> {
        unsafe {
            let mut res = self.gc_mut().alloc(&self.pair_info, &mut self.gcargs());
            res.car = car;
            res.cdr = cdr;
            res
        }
    }

    pub fn new_fixnum(&self, value: isize) -> Handle<Fixnum> {
        unsafe {
            let mut res = self.gc_mut().alloc(&self.fixnum_info, &mut self.gcargs());
            res.set_value(value);
            res
        }
    }

    pub fn new_symbol(&self, value: Id) -> Handle<Symbol> {
        unsafe {
            let mut res = self.gc_mut().alloc(&self.symbol_info, &mut self.gcargs());
            res.set_value(value);
            res
        }
    }

    pub fn new_closure(&self, info: &InfoTable<Closure>) -> Handle<Closure> {
        unsafe { self.gc_mut().alloc(&info, &mut self.gcargs()) }
    }

    pub fn new_ooparray<A: IsOop>(&self, len: usize, fill: &Handle<A>) -> Handle<OopArray> {
        unsafe {
            let mut res = self.gc_mut().alloc_array(&self.ooparray_info, len, &mut self.gcargs());
            res.len = len;
            for ptr in res.content() {
                *ptr = fill.as_oop();
            }
            res
        }
    }

    pub fn new_i64array(&self, len: usize, fill: i64) -> Handle<I64Array> {
        unsafe {
            let mut res = self.gc_mut().alloc_array(&self.i64array_info, len, &mut self.gcargs());
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
            self.gc_mut().full_gc(alloc_size, &mut self.gcargs());
            self.gc_mut().unsafe_alloc(alloc_size)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    

    #[test]
    fn test_gc_alloc() {
        let u = Universe::new(0x180);
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
        let u = Universe::new(0x200);
        let mut oops = vec![];
        assert_eq!(u.fixnum_info.sizeof_instance(), 0x10);

        unsafe {
            for i in 0..0x10 {
                oops.push(u.new_fixnum(i).dup());
            }
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
        let u = Universe::new(available_spaces);

        let fxn = u.new_fixnum(0);
        available_spaces -= 0x10;

        available_spaces -= 0x10 + (0x8 * 10);
        let _arr = u.new_ooparray(10, &fxn);
        unsafe {
            assert_eq!(u.gc_mut().available_spaces(), available_spaces);

            for _i in 0..0x1000 {
                let _h = u.new_ooparray(10, &fxn);
                assert!(u.gc_mut().available_spaces() <= available_spaces);
            }
        }
    }
}
