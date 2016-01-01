/// Shared runtime data model and functions.

pub mod oop;
pub mod gc;

use self::oop::*;
use self::gc::GcState;

pub struct Universe {
    pub gc: GcState,
    handle_block: Box<HandleBlock>,

    // Well-known info tables.
    pub pair_info: InfoTable<Pair>,
    pub fixnum_info: InfoTable<Fixnum>,
}

impl Universe {
    pub fn new(heap_size: usize) -> Self {
        Universe {
            gc: unsafe { GcState::new(heap_size) },
            handle_block: HandleBlock::new(),

            pair_info: infotable_for_pair(),
            fixnum_info: infotable_for_fixnum(),
        }
    }

    pub fn oop_is_pair(&self, oop: &Closure) -> bool {
        unsafe { oop.info_is(&self.pair_info) }
    }

    pub fn oop_is_fixnum(&self, oop: &Closure) -> bool {
        unsafe { oop.info_is(&self.fixnum_info) }
    }

    pub fn new_pair(&mut self, car: Oop, cdr: Oop) -> Handle<Pair> {
        unsafe {
            let mut res = self.gc.alloc(&self.pair_info, &self.handle_block);
            res.car = car;
            res.cdr = cdr;
            res
        }
    }

    pub fn new_fixnum(&mut self, value: isize) -> Handle<Fixnum> {
        unsafe {
            let mut res = self.gc.alloc(&self.fixnum_info, &self.handle_block);
            res.value = value;
            res
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
        assert_eq!(u.gc.available_spaces(), 0);
        assert_eq!(u.gc.full_gc_count, 0x1000 - 1);
    }

    #[test]
    fn test_gc_scavenge() {
        let mut u = Universe::new(0x200);
        let mut oops = vec![];
        assert_eq!(u.fixnum_info.sizeof_instance(), 0x10);

        for i in 0..0x10 {
            oops.push(u.new_fixnum(i));
        }
        assert_eq!(oops.len(), 0x10);
        assert_eq!(u.gc.available_spaces(), 0x100);

        for i in 0..0x10000 {
            let _h = u.new_fixnum(i);
            assert!(u.gc.available_spaces() <= 0x100);
        }
        assert_eq!(u.gc.available_spaces(), 0);
        assert_eq!(u.gc.full_gc_count, 0x1000 - 1);
        assert_eq!(u.gc.scavenged_count, 0x10 * (0x1000 - 1));
    }
}
