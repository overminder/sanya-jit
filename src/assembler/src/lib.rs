extern crate mmap;
extern crate byteorder;
extern crate num;

#[cfg(bench)]
extern crate test;

extern crate tempdir;

pub mod mem;
pub mod x64;
pub mod emit;
