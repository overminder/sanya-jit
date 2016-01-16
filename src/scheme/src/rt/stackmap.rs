use super::oop::Oop;

use fnv::FnvHasher;
use std::mem::transmute;
use std::collections::HashMap;
use std::collections::hash_state::DefaultState;

/// Frame Layout:
///
/// Higher Addr
/// ^
/// | ix                         | value
/// +----------------------------+------------
/// | rbp + 8                    | ret addr
/// | rbp                        | saved rbp
/// | rbp - 8                    | current closure ptr
/// | rbp - (16 + 8 * local_ix)  | local variable slots
/// | rsp + N ~ rsp              | local tmps (might contain an alignment slot)
///
/// Stack Map:
/// A stackmap contains all the local variables and tmps.
/// rbp - stackmap.len() * 8 points to the saved stackmap.

// Number of slots between saved_rbp and local variable slots.
// Used by c0::codegen to emit local variable slots.
pub const EXTRA_CALLER_SAVED_FRAME_SLOTS: usize = 1;

// Could also use contain-rs's bit-set crate, but this impl is sufficient now.
fn bitset_get(bs: &[u8], ix: usize) -> bool {
    ((bs[ix >> 3] >> (ix & 0x7)) & 1) == 1
}

fn bitset_set(bs: &mut [u8], ix: usize, value: bool) {
    if value {
        bs[ix >> 3] |= 1 << (ix & 0x7);
    } else {
        bs[ix >> 3] &= !(1 << (ix & 0x7));
    }
}

unsafe fn read_word(ptr: usize, offset: isize) -> usize {
    *(((ptr as isize) + offset) as *const usize)
}

// Still, the impact on the performance is quite minor. It seems that using a
// BTreeMap could sometimes be faster than using a HashMap<FnvHasher>...
type PcRelToStackMap = HashMap<usize, StackMap, DefaultState<FnvHasher>>;

// Maps the rip offsets for return addresses to stackmaps.
#[derive(Debug, Default)]
pub struct StackMapTable {
    start: Option<usize>,
    offsets: PcRelToStackMap,
}

impl StackMapTable {
    pub fn new() -> Self {
        StackMapTable {
            start: None,
            offsets: Default::default(),
        }
    }

    pub fn insert(&mut self, offset: usize, sm: StackMap) {
        self.offsets.insert(offset, sm);
    }

    pub fn set_start(&mut self, start: usize) {
        self.start = Some(start);
    }

    pub fn get(&self, rip: usize) -> Option<&StackMap> {
        self.offsets.get(&(rip - self.start.unwrap()))
    }
}

// Could also simply use boxed closures to reduce LoC but the performance
// might suffer.
pub struct StackMapIterator<'a> {
    ix: usize,
    inner: &'a StackMap,
}

impl<'a> Iterator for StackMapIterator<'a> {
    // (ix, isgcptr)
    type Item = (usize, bool);

    fn next(&mut self) -> Option<Self::Item> {
        if self.ix >= self.inner.length as usize {
            return None;
        }

        let ix = self.ix;
        self.ix += 1;
        Some((ix, self.inner.get_at(ix)))
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StackMap {
    length: u8,
    encoding: [u8; 7],
}

impl StackMap {
    pub fn new() -> Self {
        StackMap {
            length: 0,
            encoding: [0; 7],
        }
    }

    pub fn len(&self) -> usize {
        self.length as usize
    }

    pub fn iter(&self) -> StackMapIterator {
        StackMapIterator {
            inner: self,
            ix: 0,
        }
    }

    pub fn as_word(self) -> usize {
        unsafe { transmute(self) }
    }

    pub fn reify_word(w: usize) -> Self {
        unsafe { transmute(w) }
    }

    pub fn push(&mut self, is_gcptr: bool) {
        let ix = self.length;
        self.length += 1;
        self.set_at(ix as usize, is_gcptr);
    }

    pub fn push_gcptr(&mut self) {
        self.push(true);
    }

    pub fn push_word(&mut self) {
        self.push(false);
    }

    fn set_at(&mut self, ix: usize, value: bool) {
        assert!(ix < 7 * 8, "Index out of bound");
        bitset_set(&mut self.encoding, ix, value);
    }

    fn get_at(&self, ix: usize) -> bool {
        assert!(ix < 7 * 8, "Index out of bound");
        bitset_get(&self.encoding, ix)
    }

    pub fn pop(&mut self) {
        self.popn(1);
    }

    pub fn popn(&mut self, n: usize) {
        assert!(self.length as usize >= n);
        self.length -= n as u8;
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Frame {
    rbp: usize,
    rip: usize,
    stackmap: StackMap,
}

impl Frame {
    pub fn new(rbp: usize, rip: usize, stackmap: StackMap) -> Self {
        Frame {
            rbp: rbp,
            rip: rip,
            stackmap: stackmap,
        }
    }

    unsafe fn prev(&self, smt: &StackMapTable, until_rbp: usize) -> Option<Self> {
        let prev_rbp = read_word(self.rbp, 0);
        let prev_rip = read_word(self.rbp, 8);
        if prev_rbp >= until_rbp {
            assert_eq!(prev_rbp, until_rbp);
            None
        } else {
            let next_stackmap = smt.get(prev_rip).unwrap();
            Some(Frame::new(prev_rbp, prev_rip, *next_stackmap))
        }
    }

    pub fn iter_oop(&self) -> FrameOopSlotIterator {
        FrameOopSlotIterator {
            frame: self,
            inner: self.stackmap.iter(),
        }
    }
}

pub struct FrameIterator<'a> {
    frame: Option<Frame>,
    until_rbp: usize,
    smt: &'a StackMapTable,
}

impl<'a> FrameIterator<'a> {
    pub fn new(rbp: usize, rip: usize, smt: &'a StackMapTable, until_rbp: usize) -> Self {
        let stackmap = smt.get(rip).unwrap();
        FrameIterator {
            frame: Some(Frame::new(rbp, rip, *stackmap)),
            until_rbp: until_rbp,
            smt: smt,
        }
    }
}

impl<'a> Iterator for FrameIterator<'a> {
    type Item = Frame;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(frame) = self.frame.take() {
            self.frame = unsafe { frame.prev(self.smt, self.until_rbp) };
            Some(frame)
        } else {
            None
        }
    }
}

pub struct FrameOopSlotIterator<'a> {
    frame: &'a Frame,
    inner: StackMapIterator<'a>,
}

impl<'a> Iterator for FrameOopSlotIterator<'a> {
    type Item = &'a mut Oop;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((ix, is_gcptr)) = self.inner.next() {
                if is_gcptr {
                    let slot = self.frame.rbp - (1 + ix) * 8;
                    unsafe {
                        return Some(transmute(slot));
                    }
                }
            } else {
                return None;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stackmap() {
        let mut m = StackMap::new();
        assert_eq!(0, m.as_word());

        m.push_gcptr();
        m.push_word();
        m.push_word();
        m.push_gcptr();
        m.push_word();
        let m2 = StackMap::reify_word(m.as_word());

        assert_eq!(m2.len(), 5);
        let items: Vec<_> = m2.iter().filter(|x| x.1).map(|x| x.0).collect();
        assert_eq!(items, &[0, 3]);

        m.popn(2);
        let items: Vec<_> = m.iter().filter(|x| x.1).map(|x| x.0).collect();
        assert_eq!(items, &[0]);
        assert_eq!(m.len(), 3);
    }
}
