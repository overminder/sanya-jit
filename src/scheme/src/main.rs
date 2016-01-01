extern crate scheme;

use scheme::ast::sexpr::*;
use scheme::ast::nir::*;
use scheme::ast::sexpr_to_nir::*;

use std::io::{self, stdin, stdout, Read, Write};

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
    repl().unwrap();
}
