use std::mem::transmute;

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

#[repr(C)]
#[derive(Copy, Clone)]
pub struct StackMap {
    length: u8,
    encoding: [u8; 7],
}

pub struct StackMapIterator<'a> {
    ix: usize,
    inner: &'a StackMap,
}

impl<'a> Iterator for StackMapIterator<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.ix >= self.inner.length as usize {
                return None;
            }

            let ix = self.ix;
            self.ix += 1;
            if self.inner.get_at(ix) {
                return Some(ix);
            }
        }
    }
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
        let items: Vec<_> = m2.iter().collect();
        assert_eq!(items, &[0, 3]);

        m.popn(2);
        let items: Vec<_> = m.iter().collect();
        assert_eq!(items, &[0]);
        assert_eq!(m.len(), 3);
    }
}
