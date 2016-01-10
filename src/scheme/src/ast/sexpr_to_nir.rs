use super::sexpr::*;
use super::sexpr::SExpr::*;
use super::nir::*;
use super::nir::RawNode::*;
use rt::inlinesym::InlineSym;

fn unwrap_sym_list(e: &SExpr) -> Result<Vec<String>, String> {
    let es = try!(e.unwrap_list());
    let mut res = vec![];
    for e in es {
        res.push(try!(e.unwrap_sym()).to_owned());
    }
    Ok(res)
}

pub fn compile_scdefn(e: &SExpr) -> ScDefn {
    match e {
        &List(ref es) => {
            match es.as_slice() {
                [Sym(ref tag), Sym(ref name), List(ref form)] if tag == "define" &&
                                                                 form.len() >= 3 &&
                                                                 form[0] == sym("lambda") &&
                                                                 form[1].is_list() => {
                    let mut frame = FrameDescr::new();
                    let args = unwrap_sym_list(&form[1]).unwrap();
                    let read_arg_nodes: NodeList = args.iter()
                                                       .enumerate()
                                                       .map(|(nth_arg, name)| {
                                                           let local_ix =
                                                               frame.find_or_create_slot(name);
                                                           NWriteLocal(local_ix,
                                                                       box NReadArgument(nth_arg))
                                                       })
                                                       .collect();
                    let body: NodeList = form[2..form.len() - 1]
                                             .iter()
                                             .map(|e| compile_expr(e, &mut frame, false))
                                             .collect();
                    let last = box compile_expr(form.last().unwrap(), &mut frame, true);
                    ScDefn::new(name.to_owned(),
                                args,
                                frame,
                                NSeq(read_arg_nodes.into_iter().chain(body.into_iter()).collect(),
                                     last))
                }
                _ => panic!("Not a valid ScDefn: {:?}", e),
            }
        }
        _ => panic!("Not a valid ScDefn: {:?}", e),
    }
}

pub fn compile_expr(e: &SExpr, frame: &mut FrameDescr, is_tail: bool) -> RawNode {
    match e {
        &List(ref es) => {
            match es.as_slice() {
                [Sym(ref tag), Sym(ref name), ref form] if tag == "define" => {
                    let ix = frame.find_or_create_slot(name);
                    NWriteLocal(ix, box compile_expr(form, frame, is_tail))
                }
                [Sym(ref tag), ref arr, ref ix] if tag == "nth#" => {
                    NReadOopArray(box compile_expr(arr, frame, false),
                                  box compile_expr(ix, frame, false))
                }
                [Sym(ref tag), ref arr, ref ix] if tag == "nth-i64#" => {
                    NReadI64Array(box compile_expr(arr, frame, false),
                                  box compile_expr(ix, frame, false))
                }
                [Sym(ref tag), ref arr, ref ix, ref val] if tag == "set-nth!#" => {
                    NWriteOopArray(box compile_expr(arr, frame, false),
                                   box compile_expr(ix, frame, false),
                                   box compile_expr(val, frame, false))
                }
                [Sym(ref tag), ref arr, ref ix, ref val] if tag == "set-nth-i64!#" => {
                    NWriteI64Array(box compile_expr(arr, frame, false),
                                   box compile_expr(ix, frame, false),
                                   box compile_expr(val, frame, false))
                }
                [Sym(ref tag), ref arr] if tag == "len#" => {
                    // Generic array length.
                    NReadArrayLength(box compile_expr(arr, frame, false))
                }
                [Sym(ref tag), ref len, ref fill] if tag == "mk-arr#" => {
                    NMkOopArray(box compile_expr(len, frame, false),
                                box compile_expr(fill, frame, false))
                }
                [Sym(ref tag), ref len, ref fill] if tag == "mk-arr-i64#" => {
                    NMkI64Array(box compile_expr(len, frame, false),
                                box compile_expr(fill, frame, false))
                }
                [Sym(ref tag), ref e1] if tag == "display#" => {
                    NPrimO(PrimOpO::Display, box compile_expr(e1, frame, false))
                }
                [Sym(ref tag), ref e1] if tag == "fixnum?#" => {
                    NPrimO(PrimOpO::Fixnump, box compile_expr(e1, frame, false))
                }
                [Sym(ref tag), Sym(ref val)] if tag == "panic-inline-sym#" => {
                    NPrimO(PrimOpO::PanicInlineSym,
                           box NMkFixnum(InlineSym::from_str(&val).unwrap().as_word() as isize))
                }
                [Sym(ref tag), ref e1, ref e2] if is_prim_ff_op(tag) => {
                    let n1 = compile_expr(e1, frame, false);
                    let n2 = compile_expr(e2, frame, false);
                    let op = match tag.as_ref() {
                        "+#" => PrimOpFF::Add,
                        "-#" => PrimOpFF::Sub,
                        "<#" => PrimOpFF::Lt,
                        _ => panic!("{}: Not a PrimOpFF", tag),
                    };
                    NPrimFF(op, box n1, box n2)
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
                    // Special forms.
                    if let &Sym(ref tag) = &es[0] {
                        if tag == "begin" {
                            assert!(es.len() > 1);
                            let body: Vec<_> = es[1..es.len() - 1]
                                                   .iter()
                                                   .map(|e| compile_expr(e, frame, false))
                                                   .collect();
                            let last = box compile_expr(es.last().unwrap(), frame, is_tail);
                            return NSeq(body, last);
                        } else if tag == "and" {
                            assert!(es.len() == 3);
                            return NIf {
                                cond: box compile_expr(&es[1], frame, false),
                                on_true: box compile_expr(&es[2], frame, is_tail),
                                on_false: box NMkFixnum(0),
                            };
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

fn is_prim_ff_op(s: &str) -> bool {
    match s {
        "+#" | "-#" | "<#" => true,
        _ => false,
    }
}
