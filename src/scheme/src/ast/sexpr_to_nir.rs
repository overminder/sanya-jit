use super::sexpr::*;
use super::sexpr::SExpr::*;
use super::nir::*;
use super::nir::RawNode::*;

use std::boxed::Box;

pub fn compile_expr(e: &SExpr, frame: &mut FrameDescr, is_tail: bool) -> RawNode {
    match e {
        &List(ref es) => {
            match es.as_slice() {
                [Sym(ref tag), Sym(ref name), ref form] if tag == "define" => {
                    let ix = frame.find_or_create_slot(name);
                    NWriteLocal(ix, box compile_expr(form, frame, is_tail))
                }
                [Sym(ref tag), ref e1, ref e2] => {
                    let n1 = compile_expr(e1, frame, false);
                    let n2 = compile_expr(e2, frame, false);
                    match tag.as_ref() {
                        "+#" => NPrimFixnumAdd(box n1, box n2),
                        "-#" => NPrimFixnumSub(box n1, box n2),
                        "<#" => NPrimFixnumLt(box n1, box n2),
                        _ => new_call(compile_expr(&es[0], frame, false), vec![n1, n2], is_tail),
                    }
                }
                [Sym(ref tag), ref e1, ref e2, ref e3] if tag == "if" => {
                    let n1 = compile_expr(e1, frame, false);
                    let n2 = compile_expr(e2, frame, is_tail);
                    let n3 = compile_expr(e3, frame, is_tail);
                    new_if(n1, n2, n3)
                }
                [] => {
                    panic!("compile_expr: nil");
                }
                _ => {
                    if let &Sym(ref tag) = &es[0] {
                        if tag == "begin" {
                            if es.len() == 1 {
                                // Unspecified
                                return NMkFixnum(-1);
                            }
                            let mut body: Vec<_> = es[1..es.len() - 1]
                                                       .iter()
                                                       .map(|e| compile_expr(e, frame, false))
                                                       .collect();
                            let last = compile_expr(es.last().unwrap(), frame, is_tail);
                            body.push(last);
                            return NSeq(body);
                        }
                    }
                    let func = compile_expr(&es[0], frame, false);
                    let args = es[1..].iter().map(|e| compile_expr(e, frame, false)).collect();
                    new_call(func, args, is_tail)
                }
            }
        }
        &Int(ref ival) => NMkFixnum(*ival as isize),
        &Sym(ref name) => {
            match frame.lookup_slot(name) {
                Some(ix) => NReadLocal(ix),
                None => NReadGlobal(name.to_owned()),
            }
        }
    }
}
