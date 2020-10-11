use super::oop::Oop;

use fnv::{FnvBuildHasher};

use std::mem::transmute;
use std::collections::HashMap;

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

// (absolute return address, StackMap)
//
// Still, the impact on the performance is quite minor. It seems that using a
// BTreeMap could sometimes be faster than using a HashMap<FnvHasher>...
type PcToStackMap = HashMap<usize, StackMap, FnvBuildHasher>;

// (return address relative to the closure entry, StackMap)
pub type OopStackMapOffsets = HashMap<usize, StackMap>;

// Maps the rip offsets for return addresses to stackmaps.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct StackMapTable {
    offsets: PcToStackMap,
}

impl StackMapTable {
    pub fn new() -> Self {
        StackMapTable { offsets: Default::default() }
    }

    pub fn insert(&mut self, offset: usize, sm: StackMap) {
        self.offsets.insert(offset, sm);
    }

    pub fn get(&self, rip: usize) -> Option<&StackMap> {
        self.offsets.get(&rip)
    }
}

pub struct StackMapTableInserter {
    start: usize,
}

impl StackMapTableInserter {
    pub fn new(start: usize) -> Self {
        StackMapTableInserter { start: start }
    }

    pub fn insert(&self, smt: &mut StackMapTable, offset: usize, sm: StackMap) {
        smt.insert(self.start + offset, sm);
    }

    pub fn extend_with_smo(&self,
                           smt: &mut StackMapTable,
                           entry_offset: usize,
                           smo: &OopStackMapOffsets) {
        for (retaddr_offset, stackmap) in smo {
            self.insert(smt, entry_offset + retaddr_offset, stackmap.to_owned())
        }
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

    pub fn set_local_slot(&mut self, ix: usize, value: bool) {
        self.set_at(ix + 1 /* closure ptr */, value);
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
    base_rbp: usize,
    stackmap: StackMap,
}

impl Frame {
    pub fn new(rbp: usize, rip: usize, base_rbp: usize, stackmap: StackMap) -> Self {
        Frame {
            rbp: rbp,
            rip: rip,
            base_rbp: base_rbp,
            stackmap: stackmap,
        }
    }

    pub fn new_read_smt(rbp: usize, rip: usize, base_rbp: usize, smt: &StackMapTable) -> Self {
        Frame::new(rbp, rip, base_rbp, smt.get(rip).unwrap().to_owned())
    }

    unsafe fn prev(&self, smt: &StackMapTable) -> Option<Self> {
        let prev_rbp = read_word(self.rbp, 0);
        let prev_rip = read_word(self.rbp, 8);
        if prev_rbp >= self.base_rbp {
            assert_eq!(prev_rbp, self.base_rbp);
            None
        } else {
            Some(Frame::new_read_smt(prev_rbp, prev_rip, self.base_rbp, smt))
        }
    }

    pub fn iter_oop(&self) -> FrameOopSlotIterator {
        FrameOopSlotIterator {
            frame: self,
            inner: self.stackmap.iter(),
        }
    }
}

pub const OFFSET_OF_ICHAIN_NEXT: i32 = 0;
mod consts {
    #![allow(non_upper_case_globals)]
    pub const OFFSET_OF_ICHAIN_BASE_rbp: i32 = 8;
    pub const OFFSET_OF_ICHAIN_TOP_rbp: i32 = 16;
    pub const OFFSET_OF_ICHAIN_TOP_rip: i32 = 24;
}
pub use self::consts::*;

// A linked list of Rust->Native->Rust calls.
// This is used to traverse the native stacks and thus ensure the
// reentrancy for both runtimes.
#[repr(C)]
pub struct NativeInvocationChain {
    next: *const NativeInvocationChain,

    // The first native frame. Stack traversal stops here.
    pub base_rbp: usize,

    // The caller's rbp.
    pub top_rbp: usize,

    // The caller's rip, used together with the StackMapTable to find
    // the caller's stackmap.
    pub top_rip: usize,
}

impl NativeInvocationChain {
    unsafe fn next(&self) -> &NativeInvocationChain {
        &*self.next
    }
}

// Used to manually construct a generator...
enum FrameIteratorLabel {
    CheckChain,
    Finished,
    NextChain,
    CheckAndYieldFrame,
    NextFrame,
}

pub struct FrameIterator<'a> {
    frame: Option<Frame>,
    chain: *const NativeInvocationChain,
    smt: &'a StackMapTable,

    // Used for iteration.
    reentry_label: FrameIteratorLabel,
}

impl<'a> FrameIterator<'a> {
    pub fn new(smt: &'a StackMapTable, chain: *const NativeInvocationChain) -> Self {
        FrameIterator {
            frame: None,
            chain: chain,
            smt: smt,
            reentry_label: FrameIteratorLabel::CheckChain,
        }
    }

    unsafe fn deref_nic(&self) -> &NativeInvocationChain {
        &*self.chain
    }
}

impl<'a> Iterator for FrameIterator<'a> {
    type Item = Frame;

    fn next(&mut self) -> Option<Self::Item> {
        use self::FrameIteratorLabel::*;

        unsafe {
            loop {
                match self.reentry_label {
                    CheckChain => {
                        self.reentry_label = if self.chain.is_null() {
                            Finished
                        } else {
                            NextChain
                        };
                    }
                    Finished => {
                        return None;
                    }
                    NextChain => {
                        self.frame = Some(Frame::new_read_smt(self.deref_nic().top_rbp,
                                                              self.deref_nic().top_rip,
                                                              self.deref_nic().base_rbp,
                                                              self.smt));
                        self.chain = self.deref_nic().next();
                        self.reentry_label = CheckAndYieldFrame;
                    }
                    CheckAndYieldFrame => {
                        match self.frame {
                            Some(f) => {
                                self.reentry_label = NextFrame;
                                return Some(f);
                            }
                            None => {
                                self.reentry_label = CheckChain;
                            }
                        }
                    }
                    NextFrame => {
                        self.frame = self.frame.unwrap().prev(self.smt);
                        self.reentry_label = CheckAndYieldFrame;
                    }
                }
            }
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
