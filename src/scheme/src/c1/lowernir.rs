use ast::id::Id;
use ast::nir::*;
use ast::nir::RawNode::*;
use c0::cgutil::op_to_cond;
use c1::lir::*;

use std::collections::HashMap;

pub enum UntypedLir {
    First(LBlockId),
    Mid(Lir),
    Last(LirB),
}

use self::UntypedLir::*;

pub struct Linearize {
    irs: Vec<UntypedLir>,
    vreg_gen: u32,
    vreg_map: HashMap<Id, u32>,
    bid_gen: usize,
}

impl Linearize {
    fn fresh_reg(&mut self) -> LReg {
        self.vreg_gen += 1;
        LReg::Virtual(self.vreg_gen)
    }

    fn fresh_label(&mut self) -> LBlockId {
        self.bid_gen += 1;
        LBlockId(
    }

    fn emit_mid(&mut self, ir: Lir) {
        self.irs.push(Mid(ir));
    }

    fn emit_first(&mut self, lbl: LBlockId) {
        self.irs.push(First(lbl));
    }

    fn emit_last(&mut self, b: LirB) {
        self.irs.push(Last(b));
    }

    fn sc(&mut self, sc: &ScDefn) {
        self.node(sc.body());
    }

    fn node(&mut self, n: &RawNode) -> LReg {
        match n {
            &NLit(ref lit) => {
                self.literal_node(lit)
            }
            &NPrimFF(op, ref n1, ref n2) => {
                let r1 = self.node(n1);
                let r2 = self.node(n2);
                match op {
                    PrimOpFF::Add => {
                        let dst = self.fresh_reg();
                        self.emit_mid(Lir::LeaRRR(dst, r1, r2));
                        dst
                    }
                    _ => panic!("Not implemented: {:?}", n),
                }
            }
            &NIf { ref cond, ref on_true, ref on_false } => {
                let label_true = self.fresh_label();
                let label_false = self.fresh_label();
                let label_done = self.fresh_label();
                let res = self.fresh_reg();

                match cond {
                    &NPrimFF(op, ref lhs, ref rhs) if op_is_cond(op) => {
                        let r1 = self.node(lhs);
                        let r2 = self.node(rhs);
                        self.emit_mid(Lir::CmpRR(r1, r2));
                        self.emit_last(LirB::Jcc(op_to_cond(op), label_true, label_false));
                    }
                    _ => panic!("Not implemented: {:?}", n);
                }

                self.emit_first(label_true);
                self.emit_mid(Lir::MovRR(res, self.node(on_true)));
                self.emit_last(LirB::J(label_done));

                self.emit_first(label_false);
                self.emit_mid(Lir::MovRR(res, self.node(on_false)));
                self.emit_last(LirB::J(label_done));

                self.emit_first(label_done);
                res
            }
            _ => panic!("Not implemented: {:?}", n),
        }
    }

    fn literal_node(&mut self, n: &LiteralNode) -> LReg {
        let i = n.as_int().unwrap();
        let r = self.fresh_reg();
        self.emit_mid(Lir::MovRI(r, i));
        return r;
    }
}

