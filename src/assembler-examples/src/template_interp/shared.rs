use assembler::mem::JitMem;
use assembler::emit::{Emit, Label};
use assembler::x64::utils::objdump_disas_lines;

use std::fmt::Debug;
use std::env;

#[cfg(intrinsics)]
extern "rust-intrinsic" {
    pub fn breakpoint();
}

#[cfg(not(intrinsics))]
pub fn breakpoint() { }

pub trait Dispatchable<Opts>: Sized {
    fn build_interp_entry(emit: &mut Emit, opts: &Opts);
    fn build_dispatch_case(self, emit: &mut Emit, opts: &Opts);
    fn build_dispatch_with_pc_offset(emit: &mut Emit, opts: &Opts, offset: i32);
}

pub fn build_interp<Op, Opts>(last_op: Op, opts: &Opts) -> (Vec<usize>, JitMem)
    where Op: From<u8> + Dispatchable<Opts> + Debug, u8: From<Op>
{
    let mut emit = Emit::new();

    Op::build_interp_entry(&mut emit, opts);

    Op::build_dispatch_with_pc_offset(&mut emit, opts, 0);
    let mut offset_table = vec![0; 1 + u8::from(last_op) as usize];

    for (op_ix, offset_ref) in offset_table.iter_mut().enumerate() {
        let op = Op::from(op_ix as u8);
        let mut op_lbl = Label::new();
        //println!("{}: Building dispatch case for {:?}", op_ix, op);
        emit.bind(&mut op_lbl);
        *offset_ref = op_lbl.offset().unwrap();
        op.build_dispatch_case(&mut emit, opts);
    }

    let jm = JitMem::new(emit.as_ref());

    let entry = jm.as_word();
    let label_table: Vec<usize> = offset_table.iter().map(|o| entry + o).collect();

    if env::var("VERBOSE").is_ok() {
        for line in objdump_disas_lines(emit.as_ref()) {
            // Check for opcode offsets.
            for (op_ix, op_offset) in offset_table.iter().enumerate() {
                if line.trim().starts_with(&format!("{:x}:", op_offset)) {
                    println!(";; {:#x} case Op::{:?}:",
                            label_table[op_ix],
                            Op::from(op_ix as u8));
                }
            }
            println!("{}", line);
        }
        println!("jm.entry = {:#x} ; len = {}", entry, emit.as_ref().len());
    }

    (label_table, jm)
}
