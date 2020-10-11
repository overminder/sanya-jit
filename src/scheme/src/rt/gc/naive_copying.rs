/// Naive semi-space copying GC.

use super::{INFO_MARKED_TAG, INFO_FRESH_TAG};
use rt::Universe;
use rt::oop::*;
use rt::oop_utils::*;
use rt::stackmap::FrameIterator;

use std::ptr;
use std::mem::{swap, transmute};

pub const OOP_INFO_TAG_MASK: usize = 0x7;
pub const OOP_INFO_UNTAG_MASK: usize = !OOP_INFO_TAG_MASK;
pub const OOP_INFO_SCAVENGED_TAG: usize = 1;

#[repr(C)]
pub struct GcState {
    // Used during execution, accessed through generated code.
    pub alloc_ptr: *mut u8,
    pub alloc_limit: *mut u8,

    _heap: Vec<u8>,
    space_size: usize,
    from_space: *mut u8,
    to_space: *mut u8,

    // Used during scavenging.
    copy_ptr: *mut u8,

    // Just used for debugging. FullGcArgs are expected to be passed by
    // the calling universe.
    universe: *const Universe,

    // Statistics.
    pub full_gc_count: usize,
    pub scavenged_ptr_count: usize,
}

unsafe fn was_scavenged_to(oop: &Closure) -> Option<Oop> {
    let entry_word = *oop.entry_word();
    if (entry_word & OOP_INFO_TAG_MASK) == OOP_INFO_SCAVENGED_TAG {
        Some(entry_word & OOP_INFO_UNTAG_MASK)
    } else {
        None
    }
}

impl GcState {
    pub unsafe fn new(space_size: usize) -> Self {
        let heap = vec![0; space_size * 2];
        let from_space = heap.as_ptr() as *mut u8;
        let to_space = from_space.offset(space_size as isize);
        GcState {
            _heap: heap,
            space_size: space_size,
            from_space: from_space,
            to_space: to_space,

            alloc_ptr: from_space,
            alloc_limit: to_space,

            copy_ptr: ptr::null_mut(),
            universe: ptr::null_mut(),

            full_gc_count: 0,
            scavenged_ptr_count: 0,
        }
    }

    pub fn set_universe(&mut self, u: &Universe) {
        self.universe = u as *const _;
    }

    unsafe fn universe(&self) -> &Universe {
        &*self.universe
    }

    unsafe fn copy<'a>(&mut self, oop: &Closure) -> &'a mut Closure {
        let info = oop.info();
        let count = if info.is_array() {
            // Assumes all array have the same layout.
            info.sizeof_array_instance(oop.oop_cast::<OopArray>().len())
        } else {
            info.sizeof_instance()
        };
        ptr::copy(transmute(oop), self.copy_ptr, count);
        let copied_to = Closure::from_raw(self.copy_ptr as Oop);
        self.copy_ptr = self.copy_ptr.offset(count as isize);
        copied_to
    }

    unsafe fn scavenge_info(&mut self, info: &mut InfoTable<Closure>) {
        trace!("scavenge_info: {:?}, is_closure = {}, mark_word = {}",
               info,
               info.is_closure(),
               *info.gc_mark_word());
        if !info.is_closure() {
            return;
        }

        let mark_word = *info.gc_mark_word();
        if mark_word == INFO_MARKED_TAG {
            return;
        }

        assert_eq!(mark_word, INFO_FRESH_TAG);
        *info.gc_mark_word() = INFO_MARKED_TAG;

        if let Some(gcrefs) = info.gcrefs() {
            let entry_word = info.entry_word();
            for gcref in gcrefs {
                match *gcref {
                    GcRef::OopConst(offset) => {
                        let dst = entry_word + offset as usize;
                        trace!("scavenge_oopconst({:?}): [*{:?}] -> {:#x} (={:#x})",
                               info,
                               gcref,
                               dst,
                               *(dst as *const Oop));
                        self.scavenge(transmute(dst));
                    }
                    GcRef::PcRelInfoEntry(offset) => {
                        let loc = entry_word + offset as usize;
                        let dst_entry = (loc + 4) as isize + *(loc as *const i32) as isize;
                        let dst = InfoTable::<Closure>::from_entry(dst_entry as usize);
                        trace!("scavenge_info({:?}): [*{:?}] -> {:?}", info, gcref, dst);
                        self.scavenge_info(dst);
                    }
                }
            }
        } else {
            // Shouldn't happen.
            panic!("scavenge_info: infotable {:?} doesn't have gcrefs?", info);
        }
    }

    unsafe fn scavenge(&mut self, oop: &mut Oop) {
        if !is_heap_ptr(*oop) {
            return;
        }

        let closure = Closure::from_raw(*oop);
        if let Some(ptr) = was_scavenged_to(closure) {
            *oop = ptr;
            return;
        }

        let copied_to = self.copy(closure);
        self.scavenged_ptr_count += 1;
        trace!("  Scavenge: {} @{:#x} -> {} @{:#x}",
               FmtOop(closure.as_oop(), self.universe()),
               closure as *const _ as usize,
               FmtOop(copied_to.as_oop(), self.universe()),
               copied_to as *const _ as usize);
        // Tag the old object with a redirection to the copied one.
        *closure.entry_word() = copied_to.as_word() + OOP_INFO_SCAVENGED_TAG;
        // And mutate the location.
        *oop = copied_to.as_oop();

        // Scavenge constants in the code.
        self.scavenge_info(copied_to.info_mut());

        // Scavenge the interior pointers.
        if copied_to.info().is_ooparray() {
            for ptr in copied_to.oop_cast::<OopArray>().content() {
                self.scavenge(ptr);
            }
        } else {
            for ptr in copied_to.ptr_payloads() {
                self.scavenge(ptr);
            }
        }
    }

    unsafe fn prepare_collection(&mut self) {
        self.copy_ptr = self.to_space;
    }

    unsafe fn finish_collection(&mut self,
                                compiled_infos: &mut Option<&mut Vec<*const ClosureInfo>>) {

        // Remove unreachable infotables.
        if let &mut Some(ref mut compiled_infos) = compiled_infos {
            let mut i = 0;
            while i < compiled_infos.len() {
                let shall_remove = {
                    let info = &*compiled_infos[i];
                    let mark_word = info.gc_mark_word();
                    if *mark_word == INFO_FRESH_TAG {
                        trace!("finish_collection: removing {:?}", info);
                        true
                    } else {
                        *mark_word = INFO_FRESH_TAG;
                        trace!("finish_collection: keeping {:?}", *info);
                        false
                    }
                };
                if shall_remove {
                    {
                        let _info = compiled_infos.get_mut(i).unwrap();
                        // XXX: Finalize this infotable.
                    }
                    compiled_infos.swap_remove(i);
                } else {
                    i += 1;
                }
            }
        }
        swap(&mut self.to_space, &mut self.from_space);
        self.alloc_ptr = self.copy_ptr;
        self.alloc_limit = self.from_space.offset(self.space_size as isize);
        self.full_gc_count += 1;
    }

    pub unsafe fn unsafe_alloc(&mut self, alloc_size: usize) -> usize {
        let ptr = self.alloc_ptr;
        let advanced_to = self.alloc_ptr.offset(alloc_size as isize);
        assert!(advanced_to <= self.alloc_limit);
        self.alloc_ptr = advanced_to;
        ptr as usize
    }

    pub unsafe fn try_alloc<A: IsOop>(&mut self,
                                      info: &InfoTable<A>,
                                      alloc_size: usize,
                                      handle_block: &HandleBlock)
                                      -> Option<Handle<A>> {
        let ptr = self.alloc_ptr;
        let advanced_to = self.alloc_ptr.offset(alloc_size as isize);
        // Overflow? Should not really happen here...
        if advanced_to > self.alloc_limit {
            None
        } else {
            self.alloc_ptr = advanced_to;
            let oop = Closure::from_raw(ptr as usize);
            *oop.entry_word() = info.entry_word();
            Some(handle_block.new_ref_handle(oop.oop_cast()))
        }
    }

    pub unsafe fn full_gc(&mut self, alloc_size: usize, args: &mut FullGcArgs) {
        trace!("full_gc.");
        self.prepare_collection();

        // Scavenge managed handles.
        args.handle_block.head().foreach_oop(|oop| self.scavenge(oop));

        // Scavenge native frames.
        if let Some(native_frames) = args.native_frames.as_mut() {
            for (frame_no, frame) in native_frames.enumerate() {
                for (oop_ix, oop_slot) in frame.iter_oop().enumerate() {
                    trace!("  frame[{}].oop[{}]: *{:#x} -> {:#x}",
                           frame_no,
                           oop_ix,
                           oop_slot as *const _ as usize,
                           *oop_slot);
                    self.scavenge(oop_slot);
                }
            }
        }
        // Assumes no other root exists.
        self.finish_collection(&mut args.compiled_infos);
        if self.available_spaces() < alloc_size {
            panic!("GcState: failed to alloc {} bytes.", alloc_size);
        }
    }

    unsafe fn alloc_with_size<A: IsOop>(&mut self,
                                        info: &InfoTable<A>,
                                        alloc_size: usize,
                                        args: &mut FullGcArgs)
                                        -> Handle<A> {
        if let Some(h) = self.try_alloc(info, alloc_size, args.handle_block) {
            h
        } else {
            self.full_gc(alloc_size, args);
            self.try_alloc(info, alloc_size, args.handle_block).unwrap()
        }
    }

    pub unsafe fn alloc<A: IsOop>(&mut self,
                                  info: &InfoTable<A>,
                                  args: &mut FullGcArgs)
                                  -> Handle<A> {
        self.alloc_with_size(info, info.sizeof_instance(), args)
    }

    pub unsafe fn alloc_array<A: IsOop>(&mut self,
                                        info: &InfoTable<A>,
                                        len: usize,
                                        args: &mut FullGcArgs)
                                        -> Handle<A> {
        let alloc_size = info.sizeof_array_instance(len);
        self.alloc_with_size(info, alloc_size, args)
    }

    pub fn available_spaces(&self) -> usize {
        (self.alloc_limit as usize) - (self.alloc_ptr as usize)
    }

    pub fn log_stat(&self) {
        info!("GcState: full_gc_count = {}, scavenged_ptr_count = {}",
              self.full_gc_count,
              self.scavenged_ptr_count);
    }
}

pub struct FullGcArgs<'a> {
    // GC Roots
    pub handle_block: &'a HandleBlock,
    pub native_frames: Option<FrameIterator<'a>>,

    // Not necessary roots, just a vec of compiled closures.
    // This is needed since we don't move compiled code and thus
    // need to mark unreachable codes.
    pub compiled_infos: Option<&'a mut Vec<*const ClosureInfo>>,
}

impl<'a> FullGcArgs<'a> {
    pub fn handle_only(hb: &'a HandleBlock) -> Self {
        FullGcArgs {
            handle_block: hb,
            native_frames: None,
            compiled_infos: None,
        }
    }
}

fn is_heap_ptr(oop: Oop) -> bool {
    if oop == NULL_OOP {
        false
    } else if Singleton::is_singleton(oop) {
        false
    } else {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    

    #[test]
    fn test_alloc() {
        unsafe {
            let heap_size = 0x10000;
            let mut gc = GcState::new(heap_size);
            let pair_info = infotable_for_pair();
            let fixnum_info = infotable_for_fixnum();

            let handle_block = HandleBlock::new();
            let mut fga = FullGcArgs::handle_only(&handle_block);

            // (In-order scavenging).
            let mut p1 = gc.alloc(&pair_info, &mut fga);
            let mut i1 = gc.alloc(&fixnum_info, &mut fga);
            let mut i2 = gc.alloc(&fixnum_info, &mut fga);

            i1.set_value(999);
            i2.set_value(888);
            p1.car = i1.as_oop();
            p1.cdr = i2.as_oop();

            let i1_loc = *i1.oop();
            let i2_loc = *i2.oop();
            let p1_loc = *p1.oop();
            gc.full_gc(0, &mut fga);
            assert_eq!(*i1.oop() - i1_loc, heap_size);
            assert_eq!(*i2.oop() - i2_loc, heap_size);
            assert_eq!(*p1.oop() - p1_loc, heap_size);
        }
    }
}
