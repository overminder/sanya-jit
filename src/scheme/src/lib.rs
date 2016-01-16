#![feature(convert)]
#![feature(slice_patterns)]
#![feature(box_syntax)]

extern crate assembler;
extern crate byteorder;
#[macro_use]
extern crate log;

pub mod ast;
pub mod rt;
pub mod c0;
