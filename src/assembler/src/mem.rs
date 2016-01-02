use std::slice;
use std::mem::transmute;

use mmap::MemoryMap;
use mmap::MapOption::{MapReadable, MapWritable, MapExecutable};

pub struct JitMem {
    inner: MemoryMap,
}

impl JitMem {
    pub fn new(bs: &[u8]) -> Self {
        let inner = MemoryMap::new(bs.len(), &[MapReadable, MapWritable, MapExecutable]).unwrap();
        let thiz = JitMem { inner: inner };

        unsafe {
            let inner_slice = thiz.inner_slice_mut();
            for (dst, src) in inner_slice.iter_mut().zip(bs.iter()) {
                *dst = *src;
            }
        }
        thiz
    }

    unsafe fn inner_slice_mut(&self) -> &mut [u8] {
        slice::from_raw_parts_mut(self.inner.data(), self.inner.len())
    }

    pub unsafe fn call_ptr_ptr(&self, arg: isize) -> isize {
        let as_fn: extern "C" fn(isize) -> isize = transmute(self.inner_slice_mut().as_ptr());
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
            transmute(self.inner_slice_mut().as_ptr());
        as_fn(arg1, arg2, arg3, arg4, arg5, arg6)
    }

    pub unsafe fn start(&self) -> usize {
        transmute(self.inner_slice_mut().as_ptr())
    }
}
