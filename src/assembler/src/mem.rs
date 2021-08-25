use std::slice;
use std::mem::transmute;

use mmap::MemoryMap;
use mmap::MapOption::{MapReadable, MapWritable, MapExecutable};

pub struct JitMem {
    inner: MemoryMap,
}

impl JitMem {
    pub fn new(bs: &[u8]) -> Self {
        // TODO: Port this to Apple Silicon and respect W^X
        // See: https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon
        let inner = MemoryMap::new(bs.len(), &[MapReadable, MapWritable, MapExecutable]).unwrap();
        let mut thiz = JitMem { inner: inner };

        {
            let inner_slice = thiz.as_mut();
            for (dst, src) in inner_slice.iter_mut().zip(bs.iter()) {
                *dst = *src;
            }
        }
        thiz
    }

    pub fn as_word(&self) -> usize {
        unsafe { transmute(self.inner.data()) }
    }

    pub fn start(&self) -> usize {
        self.as_word()
    }

    pub unsafe fn call_ptr_ptr(&self, arg: isize) -> isize {
        let as_fn: extern "C" fn(isize) -> isize = transmute(self.as_word());
        as_fn(arg)
    }

    pub unsafe fn call_ptr6_ptr(&self,
                                arg1: isize,
                                arg2: isize,
                                arg3: isize,
                                arg4: isize,
                                arg5: isize,
                                arg6: isize)
                                -> isize {
        let as_fn: extern "C" fn(isize, isize, isize, isize, isize, isize) -> isize =
            transmute(self.as_word());
        as_fn(arg1, arg2, arg3, arg4, arg5, arg6)
    }
}

impl AsRef<[u8]> for JitMem {
    fn as_ref(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.inner.data(), self.inner.len()) }
    }
}

impl AsMut<[u8]> for JitMem {
    fn as_mut(&mut self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.inner.data(), self.inner.len()) }
    }
}

