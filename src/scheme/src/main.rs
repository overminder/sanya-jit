#![feature(slice_patterns)]
#![feature(intrinsics)]

extern crate scheme;
#[macro_use]
extern crate log;
extern crate env_logger;

use scheme::ast::sexpr::*;
use scheme::ast::nir::*;
use scheme::ast::sexpr_to_nir::*;
use scheme::ast::nir_lint::*;
use scheme::c0;
use scheme::rt::*;

use std::env;
use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};

extern "rust-intrinsic" {
    fn breakpoint();
}

fn run_file(path: &str) -> io::Result<()> {
    let break_before_main = env::var("SCM_CG_DEBUG").map(|_| true).unwrap_or(false);
    let heap_size = env::var("SCM_HEAP_SIZE").map(|x| x.parse().unwrap()).unwrap_or(0x10000);

    let mut src = String::new();
    {
        let mut f = try!(File::open(path));
        try!(f.read_to_string(&mut src));
    }

    let mut universe = Universe::new(heap_size);

    let es = parse_many(&src).unwrap();
    // println!("Parsed sexpr: {:?}", es);
    let mut scdefns: Vec<ScDefn> = compile_sc(&es);
    trace!("Compiled scdefns: {:?}", scdefns);

    lint_scdefns(&mut scdefns).unwrap();
    let mut linked = c0::link(c0::compile(&mut scdefns, &universe), &universe);
    // println!("smt = {:?}", linked.smt());
    unsafe {
        if break_before_main {
            breakpoint();
        }
        linked.call_entry(&mut universe);
        universe.gc_mut().log_stat();
    }

    Ok(())
}

fn repl() -> io::Result<()> {
    loop {
        let mut line = String::new();
        print!("{}", "> ");
        try!(stdout().flush());
        try!(stdin().read_line(&mut line));
        if line.len() == 0 {
            break;
        }

        let es = parse_many(&line).unwrap();
        println!("Parsed sexpr: {:?}", es);

        let node = compile_expr(&es[0]);
        println!("Compiled node: {:?}", node);
    }

    Ok(())
}

fn main() {
    env_logger::init().unwrap();

    let args: Vec<String> = env::args().collect();
    let args_ref: Vec<&str> = args.iter().skip(1).map(|e| e.as_ref()).collect();
    match args_ref.iter().as_slice() {
        [] | ["-r"] | ["--repl"] => repl(),
        [ref f] | ["-f", ref f] | ["--file", ref f] => run_file(f),
        _ => panic!("Doesn't know how to run args: {:?}", args),
    }
    .unwrap();
}
