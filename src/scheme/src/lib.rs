#![feature(convert)]
#![feature(slice_patterns)]
#![feature(box_syntax)]
#![feature(hashmap_hasher)]
#![feature(drain)]
#![feature(iter_arith)]

extern crate assembler;
extern crate byteorder;
#[macro_use]
extern crate log;
extern crate fnv;

pub mod ast;
pub mod rt;
pub mod c0;
pub mod c1;
