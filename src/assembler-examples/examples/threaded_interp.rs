extern crate assembler_examples;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let name = &args[1];
    let n = args[2].parse().unwrap();

    let f: fn(u8) = match name.as_ref() {
        "stack" => assembler_examples::template_interp::stack_based::main,
        "reg" => assembler_examples::template_interp::reg_based::main,
        _ => panic!("No impl for {}", name),
    };

    f(n);
}
