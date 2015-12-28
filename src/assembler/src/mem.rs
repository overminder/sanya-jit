use std::slice;
use std::mem::transmute;

use mmap::MemoryMap;
use mmap::MapOption::{MapReadable, MapWritable, MapExecutable};

pub struct JitMem<'a> {
    _inner: MemoryMap,
    inner_slice: &'a mut [u8],
}

impl<'a> JitMem<'a> {
    pub fn new(bs: &[u8]) -> Self {
        let inner = MemoryMap::new(bs.len(), &[MapReadable, MapWritable, MapExecutable]).unwrap();
        let inner_slice = unsafe { slice::from_raw_parts_mut(inner.data(), inner.len()) };
        for (dst, src) in inner_slice.iter_mut().zip(bs.iter()) {
            *dst = *src;
        }
        JitMem {
            _inner: inner,
            inner_slice: inner_slice,
        }
    }

    pub unsafe fn call_ptr_ptr(&self, arg: isize) -> isize {
        let as_fn: extern "C" fn(isize) -> isize = transmute(self.inner_slice.as_ptr());
        as_fn(arg)
    }

    pub unsafe fn call_ptr6_ptr(&self, arg1: isize, arg2: isize, arg3: isize,
                                arg4: isize, arg5: isize, arg6: isize) -> isize {
        let as_fn: extern "C" fn(isize, isize, isize, isize, isize, isize) -> isize = transmute(self.inner_slice.as_ptr());
        as_fn(arg1, arg2, arg3, arg4, arg5, arg6)
    }
}
