use ast::nir::*;
use assembler::x64::Cond;

pub fn op_to_cond(op: PrimOpFF) -> Cond {
    match op {
        PrimOpFF::Lt => Cond::L,
        PrimOpFF::Eq => Cond::E,
        _ => panic!("Not a conditional op: {:?}", op),
    }
}
