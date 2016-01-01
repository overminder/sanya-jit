/// Shared runtime data model and functions.

pub mod oop;
pub mod gc;

use self::oop::*;
use self::gc::GcState;

pub struct Universe {
    gc: GcState,
    handle_block: Box<HandleBlock>,

    // Well-known info tables.
    pair_info: InfoTable<Pair>,
    fixnum_info: InfoTable<Fixnum>,
    infotables: Vec<InfoTable<Closure>>,
}

impl Universe {
    fn new() -> Self {
        Universe {
            gc: unsafe { GcState::new(0x10000) },
            handle_block: HandleBlock::new(),

            pair_info: infotable_for_pair(),
            fixnum_info: infotable_for_fixnum(),
            infotables: vec![],
        }
    }

    fn alloc<A: IsOop>(&mut self, info: &InfoTable<A>) -> Box<Handle<A>> {
        unsafe {
            if let Some(h) = self.gc.try_alloc(info, &self.handle_block) {
                h
            } else {
                let size = info.sizeof_instance();
                self.gc.prepare_collection(&self.handle_block);
                // Assumes no other root exists.
                self.gc.finish_collection();
                if self.gc.available_spaces() < size {
                    panic!("Univerise: failed to alloc {} bytes for {:?}", size, info);
                } else {
                    self.gc.try_alloc(info, &self.handle_block).unwrap()
                }
            }
        }
    }
}
