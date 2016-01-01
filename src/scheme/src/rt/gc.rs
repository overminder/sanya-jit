/// Naive semi-space copying GC.

use super::oop::{NULL_OOP, Oop, Closure, InfoTable, IsOop};

use std::ptr;
use std::mem::{swap, transmute};

pub const INFO_TAG_MASK: usize = 0x7;
pub const INFO_UNTAG_MASK: usize = !INFO_TAG_MASK;
pub const INFO_SCAVANGED_TAG: usize = 1;

pub struct GcState {
    _heap: Vec<u8>,
    space_size: usize,
    from_space: *mut u8,
    to_space: *mut u8,

    // Used during execution
    alloc_ptr: *mut u8,
    alloc_limit: *mut u8,

    // Used during scavaging.
    copy_ptr: *mut u8,
}

unsafe fn was_scavanged_to(oop: &Closure) -> Option<Oop> {
    let info_word = *oop.info_word();
    if (info_word & INFO_TAG_MASK) == INFO_SCAVANGED_TAG {
        Some(info_word & INFO_UNTAG_MASK)
    } else {
        None
    }
}

impl GcState {
    pub unsafe fn new(space_size: usize) -> Self {
        let heap = vec![0; space_size * 2];
        let from_space = heap.as_ptr() as *mut _;
        let to_space = from_space.offset(space_size as isize);
        GcState {
            _heap: heap,
            space_size: space_size,
            from_space: from_space,
            to_space: to_space,

            alloc_ptr: from_space,
            alloc_limit: to_space,

            copy_ptr: ptr::null_mut(),
        }
    }

    unsafe fn copy<'a>(&mut self, oop: &Closure) -> &'a Closure {
        let count = oop.info().sizeof_instance();
        ptr::copy(transmute(oop), self.copy_ptr, count);
        let copied_to = Closure::from_raw(self.copy_ptr as Oop);
        self.copy_ptr = self.copy_ptr.offset(count as isize);
        copied_to
    }

    pub unsafe fn scavange(&mut self, oop: &mut Oop) {
        if *oop == NULL_OOP {
            return;
        }

        let closure = Closure::from_raw(*oop);
        if let Some(ptr) = was_scavanged_to(closure) {
            *oop = ptr;
            return;
        }

        let copied_to = self.copy(closure);
        *closure.info_word() = copied_to.as_word() + INFO_SCAVANGED_TAG;

        for ptr in copied_to.ptr_payloads() {
            self.scavange(ptr);
        }
    }

    pub unsafe fn prepare_collection(&mut self) {
        self.copy_ptr = self.to_space;
    }

    pub unsafe fn finish_collection(&mut self) {
        swap(&mut self.to_space, &mut self.from_space);
        self.alloc_ptr = self.copy_ptr;
        self.alloc_limit = self.alloc_ptr.offset(self.space_size as isize);
    }

    pub unsafe fn try_alloc<'a, A: IsOop>(&mut self, info: &InfoTable) -> Option<&'a mut A> {
        let ptr = self.alloc_ptr;
        let size = info.sizeof_instance();
        let advanced_to = self.alloc_ptr.offset(size as isize);
        if advanced_to >= self.alloc_limit {
            None
        } else {
            self.alloc_ptr = advanced_to;
            let oop = Closure::from_raw(ptr as usize);
            *oop.info_word() = transmute(info);
            Some(transmute(oop))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rt::oop::*;

    #[test]
    fn test_alloc() {
        unsafe {
            let mut gc = GcState::new(0x10000);
            let pair_info = InfoTable::of_pair();
            let fixnum_info = InfoTable::of_fixnum();

            let i1: &mut Fixnum = gc.try_alloc(&fixnum_info).unwrap();
            i1.value = 999;
            let i2: &mut Fixnum = gc.try_alloc(&fixnum_info).unwrap();
            i2.value = 888;
            let p1: &mut Pair = gc.try_alloc(&pair_info).unwrap();
            p1.car = i1.as_oop();
            p1.cdr = i2.as_oop();

            gc.prepare_collection();
            gc.finish_collection();
        }
    }
}
