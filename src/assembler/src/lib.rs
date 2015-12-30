#![feature(test)]
#![feature(zero_one)]

extern crate mmap;
extern crate byteorder;

#[cfg(test)]
extern crate test;

#[cfg(test)]
extern crate tempdir;

pub mod mem;
pub mod x64;
