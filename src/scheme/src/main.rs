#![feature(slice_patterns)]

extern crate scheme;

use scheme::ast::sexpr::*;
use scheme::ast::nir::*;
use scheme::ast::sexpr_to_nir::*;
use scheme::ast::nir_lint::*;
use scheme::c0::*;

use std::env;
use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};
use std::mem::transmute;

fn run_file(path: &str) -> io::Result<()> {
    let mut src = String::new();
    {
        let mut f = try!(File::open(path));
        try!(f.read_to_string(&mut src));
    }

    let es = parse_many(&src).unwrap();
    println!("Parsed sexpr: {:?}", es);
    let mut scdefns: Vec<ScDefn> = es.iter().map(|e| compile_scdefn(e)).collect();
    println!("Compiled scdefns: {:?}", scdefns);

    lint_scdefns(&mut scdefns).unwrap();
    let mut ctx = ModuleContext::new();
    for scdefn in scdefns {
        ctx.add_scdefn(scdefn);
    }
    let main_entry = ctx.compile_all();
    unsafe {
        transmute::<usize, fn()>(main_entry)();
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

        let mut frame = FrameDescr::new();
        let node = compile_expr(&es[0], &mut frame, true);
        println!("Compiled node: {:?}", node);
    }

    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let args_ref: Vec<&str> = args.iter().skip(1).map(|e| e.as_ref()).collect();
    match args_ref.iter().as_slice() {
        [] | ["-r"] | ["--repl"] => repl(),
        [ref f] | ["-f", ref f] | ["--file", ref f] => run_file(f),
        _ => panic!("Doesn't know how to run args: {:?}", args),
    }
    .unwrap();
}