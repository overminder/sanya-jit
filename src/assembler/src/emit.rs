use byteorder::{ByteOrder, NativeEndian};

use std::mem::{size_of, replace};

pub struct Emit(Vec<u8>);

impl Emit {
    pub fn new() -> Self {
        Emit(vec![])
    }

    pub fn to_vec(inner: Vec<u8>) -> Self {
        Emit(inner)
    }

    pub fn write_byte(&mut self, b: u8) {
        self.0.push(b)
    }

    pub fn write_bytes(&mut self, bs: &[u8]) {
        // A simple loop suits well for instruction sequences (bs.len() < 8).
        for b in bs {
            self.0.push(*b);
        }
    }

    pub fn write_i32(&mut self, i: i32) {
        let mut buf = [0; 4];
        NativeEndian::write_i32(&mut buf, i);
        self.write_bytes(&buf);
    }

    pub fn here(&self) -> usize {
        self.0.len()
    }

    pub unsafe fn alloc<'a, A: Sized>(&mut self, a: A) -> &'a mut A {
        let size = size_of::<A>();
        let here = self.0.as_mut_ptr();
        self.0.reserve(size);
        for _ in 0..size {
            self.0.push(0);
        }
        let res = &mut *(here as *mut _);
        replace(res, a);
        res
    }

    pub fn patch_i32(&mut self, ix: usize, value: i32) {
        NativeEndian::write_i32(&mut self.0[ix..ix + 4], value);
    }

    pub fn write_i64(&mut self, i: i64) {
        let mut buf = [0; 8];
        NativeEndian::write_i64(&mut buf, i);
        self.write_bytes(&buf);
    }

    pub fn bind(&mut self, label: &mut Label) -> &mut Self {
        label.bind(self);
        self
    }
}

impl Into<Vec<u8>> for Emit {
    fn into(self) -> Vec<u8> {
        self.0
    }
}

impl AsMut<Vec<u8>> for Emit {
    fn as_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl AsRef<[u8]> for Emit {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

// Labels

#[derive(Debug)]
pub enum Label {
    Unbound {
        patch_ixs: Vec<usize>,
    },
    Bound {
        offset: usize,
    },
}

impl Label {
    pub fn bind(&mut self, emit: &mut Emit) {
        *self = match self {
            &mut Label::Unbound { ref mut patch_ixs } => {
                let here = emit.here();
                for patch_ix in patch_ixs.drain(..) {
                    emit.patch_i32(patch_ix - 4, (here - patch_ix) as i32);
                }
                Label::Bound { offset: here }
            }
            &mut Label::Bound { .. } => {
                panic!("Binding a bound label: {:?}", self);
            }
        };
    }

    pub fn offset(&self) -> Option<usize> {
        match self {
            &Label::Unbound { .. } => None,
            &Label::Bound { offset } => Some(offset),
        }
    }

    pub fn patch_last_i32(&mut self, emit: &mut Emit) {
        let here = emit.here();
        match self {
            &mut Label::Unbound { ref mut patch_ixs } => {
                patch_ixs.push(here);
            }
            &mut Label::Bound { offset } => {
                emit.patch_i32(here - 4, -((here - offset) as i32));
            }
        }
    }

    pub fn new() -> Self {
        Label::Unbound { patch_ixs: vec![] }
    }
}

// Safety check against using labels without binding them.
#[cfg(debug_assertions)]
impl Drop for Label {
    fn drop(&mut self) {
        use std::mem::swap;

        match self {
            &mut Label::Unbound { ref mut patch_ixs } => {
                if patch_ixs.len() > 0 {
                    // Clear the label before panicking, since the label
                    // will be dropped again in the panic handler (huh?).
                    let mut ixs_copy = vec![];
                    swap(&mut ixs_copy, patch_ixs);
                    panic!("Unbound label {:?} went out of scope.", ixs_copy);
                }
            }
            _ => {}
        }
    }
}
