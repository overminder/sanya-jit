#![feature(test)]
#![feature(zero_one)]
#![feature(drain)]

extern crate mmap;
extern crate byteorder;

#[cfg(test)]
extern crate test;

extern crate tempdir;

pub mod mem;
pub mod x64;
pub mod emit;
