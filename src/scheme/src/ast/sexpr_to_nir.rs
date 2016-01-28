use super::sexpr::*;
use super::sexpr::SExpr::*;
use super::id::*;
use super::nir::*;
use super::nir::RawNode::*;
use rt::inlinesym::InlineSym;

fn unwrap_sym_list(e: &SExpr) -> Result<Vec<Id>, String> {
    let es = try!(e.unwrap_list());
    let mut res = vec![];
    for e in es {
        res.push(Id::named(try!(e.unwrap_sym())));
    }
    Ok(res)
}

pub fn compile_sc(es: &[SExpr]) -> Vec<ScDefn> {
    let mut ctx = Context::new();

    for e in es {
        let sc = ctx.compile_sc(e);
        ctx.add_sc(sc);
    }
    ctx.0
}

pub fn compile_expr(e: &SExpr) -> (RawNode, Vec<ScDefn>) {
    let mut ctx = Context::new();
    let node = ctx.compile_expr(e, &mut FrameDescrChain::new(), true);
    (node, ctx.0)
}

struct Context(Vec<ScDefn>);

impl Context {
    fn new() -> Self {
        Context(vec![])
    }

    fn add_sc(&mut self, sc: ScDefn) {
        self.0.push(sc)
    }

    fn compile_sc(&mut self, e: &SExpr) -> ScDefn {
        match e {
            &List(ref es) => {
                match es.as_slice() {
                    [Sym(ref tag), Sym(ref name), List(ref form)] if tag == "define" &&
                                                                     form.len() >= 3 &&
                                                                     form[0] == sym("lambda") &&
                                                                     form[1].is_list() => {
                        let mut fdc = FrameDescrChain::new();
                        self.compile_lambda(Id::named(name), &form[1], &form[2..], &mut fdc)
                            .into_sc(fdc.into_current())
                    }
                    _ => panic!("Not a valid ScDefn: {:?}", e),
                }
            }
            _ => panic!("Not a valid ScDefn: {:?}", e),
        }
    }

    fn compile_lambda(&mut self,
                      name: Id,
                      args: &SExpr,
                      body: &[SExpr],
                      fdc: &mut FrameDescrChain)
                      -> Lambda {
        let args = unwrap_sym_list(args).unwrap();
        let read_arg_nodes: NodeList = args.iter()
                                           .enumerate()
                                           .map(|(nth_arg, name)| {
                                               let local_ix =
                                                   fdc.create_local_slot(name.to_owned());
                                               NWriteLocal(local_ix, box NReadArgument(nth_arg))
                                           })
                                           .collect();
        let body = self.compile_expr_seq(body, fdc, true);
        Lambda(name, args, body.prepend_nodes(read_arg_nodes).into_nseq())
    }

    fn compile_expr(&mut self, e: &SExpr, fdc: &mut FrameDescrChain, tail: bool) -> RawNode {
        match e {
            &List(ref es) => {
                match es.as_slice() {
                    [Sym(ref tag), Sym(ref name), ref form] if tag == "define" => {
                        // Define is not recursive.
                        let form = box self.compile_expr(form, fdc, tail);
                        let ix = fdc.create_local_slot(Id::named(name));
                        NWriteLocal(ix, form)
                    }
                    [Sym(ref tag), ref arr, ref ix] if tag == "nth#" => {
                        NReadOopArray(box self.compile_expr(arr, fdc, false),
                                      box self.compile_expr(ix, fdc, false))
                    }
                    [Sym(ref tag), ref arr, ref ix] if tag == "nth-i64#" => {
                        NReadI64Array(box self.compile_expr(arr, fdc, false),
                                      box self.compile_expr(ix, fdc, false))
                    }
                    [Sym(ref tag), ref arr, ref ix, ref val] if tag == "set-nth!#" => {
                        NWriteOopArray(box self.compile_expr(arr, fdc, false),
                                       box self.compile_expr(ix, fdc, false),
                                       box self.compile_expr(val, fdc, false))
                    }
                    [Sym(ref tag), ref arr, ref ix, ref val] if tag == "set-nth-i64!#" => {
                        NWriteI64Array(box self.compile_expr(arr, fdc, false),
                                       box self.compile_expr(ix, fdc, false),
                                       box self.compile_expr(val, fdc, false))
                    }
                    [Sym(ref tag), ref arr] if tag == "len#" => {
                        // Generic array length.
                        NReadArrayLength(box self.compile_expr(arr, fdc, false))
                    }
                    [Sym(ref tag), ref len, ref fill] if tag == "mk-arr#" => {
                        NMkOopArray(box self.compile_expr(len, fdc, false),
                                    box self.compile_expr(fill, fdc, false))
                    }
                    [Sym(ref tag), ref len, ref fill] if tag == "mk-arr-i64#" => {
                        NMkI64Array(box self.compile_expr(len, fdc, false),
                                    box self.compile_expr(fill, fdc, false))
                    }
                    [Sym(ref tag), ref car, ref cdr] if tag == "cons#" => {
                        NMkPair(box self.compile_expr(car, fdc, false),
                                box self.compile_expr(cdr, fdc, false))
                    }
                    [Sym(ref tag), ref e1] if tag == "display#" => {
                        NPrimO(PrimOpO::Display, box self.compile_expr(e1, fdc, false))
                    }
                    [Sym(ref tag), ref e1] if tag == "fixnum?#" => {
                        NPrimO(PrimOpO::Fixnump, box self.compile_expr(e1, fdc, false))
                    }
                    [Sym(ref tag), Sym(ref val)] if tag == "panic-inline-sym#" => {
                        NPrimO(PrimOpO::PanicInlineSym,
                               box NMkFixnum(InlineSym::from_str(&val).unwrap().as_word() as isize))
                    }
                    [Sym(ref tag), ref val] if tag == "mk-box#" => {
                        NMkBox(box self.compile_expr(val, fdc, false))
                    }
                    [Sym(ref tag), ref loc] if tag == "unwrap-box#" => {
                        NReadBox(box self.compile_expr(loc, fdc, false))
                    }
                    [Sym(ref tag), ref loc, ref val] if tag == "set-box!#" => {
                        NWriteBox(box self.compile_expr(loc, fdc, false),
                                  box self.compile_expr(val, fdc, false))
                    }
                    [Sym(ref tag), ref e1, ref e2] if is_prim_ff_op(tag) => {
                        let n1 = self.compile_expr(e1, fdc, false);
                        let n2 = self.compile_expr(e2, fdc, false);
                        let op = match tag.as_ref() {
                            "+#" => PrimOpFF::Add,
                            "-#" => PrimOpFF::Sub,
                            "<#" => PrimOpFF::Lt,
                            _ => panic!("{}: Not a PrimOpFF", tag),
                        };
                        NPrimFF(op, box n1, box n2)
                    }
                    [Sym(ref tag), ref e1, ref e2, ref e3] if tag == "if" => {
                        let n1 = self.compile_expr(e1, fdc, false);
                        let n2 = self.compile_expr(e2, fdc, tail);
                        let n3 = self.compile_expr(e3, fdc, tail);
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
                                return self.compile_expr_seq(&es[1..], fdc, tail).into_nseq();
                            } else if tag == "and" {
                                assert!(es.len() == 3);
                                return NIf {
                                    cond: box self.compile_expr(&es[1], fdc, false),
                                    on_true: box self.compile_expr(&es[2], fdc, tail),
                                    on_false: box NMkFixnum(0),
                                };
                            } else if tag == "lambda" {
                                let (frame, lam) = fdc.new_inner(|mut new_chain| {
                                    self.compile_lambda(Id::fresh("lambda"),
                                                        &es[1],
                                                        &es[2..],
                                                        &mut new_chain)
                                });
                                let name = lam.name();
                                self.add_sc(lam.into_sc(frame));
                                return NMkClosure(name);
                            } else if tag == "letrec" {
                                let bindings = &es[1];
                                let body = &es[2];
                            }
                        }
                        let func = self.compile_expr(&es[0], fdc, false);
                        let args = es[1..]
                                       .iter()
                                       .map(|e| self.compile_expr(e, fdc, false))
                                       .collect();
                        new_call(func, args, tail)
                    }
                }
            }
            &Int(ref ival) => NMkFixnum(*ival as isize),
            &Sym(ref name) => {
                let name = Id::named(name);
                let slot = fdc.lookup_slot(&name).cloned().unwrap_or_else(|| Slot::Global(name));
                NReadSlot(slot)
            }
        }
    }

    fn compile_expr_seq(&mut self, es: &[SExpr], fdc: &mut FrameDescrChain, tail: bool) -> NSeq0 {
        assert!(es.len() >= 1);
        let body = es[..es.len() - 1]
                       .iter()
                       .map(|e| self.compile_expr(e, fdc, false))
                       .collect();
        let last = self.compile_expr(es.last().unwrap(), fdc, tail);
        NSeq0(body, last)
    }
}

// Some partial structures.

// ScDefn minus FrameDescr.
struct Lambda(Id, Vec<Id>, RawNode);

impl Lambda {
    fn name(&self) -> Id {
        self.0
    }

    fn into_sc(self, fd: FrameDescr) -> ScDefn {
        ScDefn::new(self.0, self.1, fd, self.2)
    }
}

struct NSeq0(NodeList, RawNode);

impl NSeq0 {
    fn prepend_nodes(mut self, mut ns: NodeList) -> Self {
        ns.extend(self.0.drain(..));
        self.0 = ns;
        self
    }

    fn into_nseq(self) -> RawNode {
        if self.0.len() == 0 {
            self.1
        } else {
            NSeq(self.0, box self.1)
        }
    }

    #[allow(unused)]
    fn into_nodes(mut self) -> NodeList {
        self.0.push(self.1);
        self.0
    }
}

fn is_prim_ff_op(s: &str) -> bool {
    match s {
        "+#" | "-#" | "<#" => true,
        _ => false,
    }
}
