extern crate assembler_examples;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let name: &str;
    let n: u8;
    if args.len() == 3 {
        name = &args[1];
        n = args[2].parse().unwrap();
    } else {
        name = "reg";
        n = 40;
    }

    let f: fn(u8) = match name.as_ref() {
        "stack" => assembler_examples::template_interp::stack_based::main,
        "reg" => assembler_examples::template_interp::reg_based::main,
        "trace" => assembler_examples::template_interp::stack_with_trace::main,
        _ => panic!("No impl for {}", name),
    };

    f(n);
}
