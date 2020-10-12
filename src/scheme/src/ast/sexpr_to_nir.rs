use super::sexpr::*;
use super::sexpr::SExpr::*;
use super::id::*;
use super::nir::*;
use super::nir::RawNode::*;
use super::nir::AllocNode::*;
use super::nir::LiteralNode::*;

fn unwrap_sym_list(e: &SExpr) -> CompileResult<Vec<Id>> {
    let es = e.unwrap_list()?;
    let mut res = vec![];
    for e in es {
        res.push(Id::named(e.unwrap_sym()?));
    }
    Ok(res)
}

fn unwrap_bindings(e: &SExpr) -> CompileResult<Vec<(Id, &SExpr)>> {
    let es = e.unwrap_list()?;
    let mut res = vec![];
    for e in es {
        let b = e.unwrap_list()?;
        res.push((Id::named(b[0].unwrap_sym()?), &b[1]));
    }
    Ok(res)
}

pub fn compile_sc(es: &[SExpr]) -> CompileResult<Vec<ScDefn>> {
    let mut ctx = Context::new();

    for e in es {
        let sc = ctx.compile_sc(e)?;
        ctx.add_sc(sc);
    }
    Ok(ctx.0)
}

pub fn compile_expr(e: &SExpr) -> CompileResult<(RawNode, Vec<ScDefn>)> {
    let mut ctx = Context::new();
    let node = ctx.compile_expr(e, &mut FrameDescrChain::new(), true)?;
    Ok((node, ctx.0))
}

struct Context(Vec<ScDefn>);

pub type CompileResult<A> = Result<A, String>;

impl Context {
    fn new() -> Self {
        Context(vec![])
    }

    fn add_sc(&mut self, sc: ScDefn) {
        self.0.push(sc)
    }

    fn compile_sc(&mut self, e: &SExpr) -> CompileResult<ScDefn> {
        match e {
            &List(ref es) => {
                match es.as_slice() {
                    [Sym(ref tag), Sym(ref name), List(ref form)] if tag == "define" &&
                                                                     form.len() >= 3 &&
                                                                     form[0] == sym("lambda") &&
                                                                     form[1].is_list() => {
                        let mut fdc = FrameDescrChain::new();
                        Ok(self.compile_lambda(Id::named(name),
                                               &form[1],
                                               &form[2..],
                                               &mut fdc)?
                               .into_sc(fdc.into_current()))
                    }
                    _ => Err(format!("Not a valid ScDefn: {:?}", e)),
                }
            }
            _ => Err(format!("Not a valid ScDefn: {:?}", e)),
        }
    }

    fn compile_lambda(&mut self,
                      name: Id,
                      args: &SExpr,
                      body: &[SExpr],
                      fdc: &mut FrameDescrChain)
                      -> CompileResult<Lambda> {
        let args = unwrap_sym_list(args)?;
        let arg_nodes: Vec<(usize, RawNode)> = args.iter()
                                                   .enumerate()
                                                   .map(|(nth_arg, name)| {
                                                       let local_ix =
                                                           fdc.create_local_slot(name.to_owned());
                                                       (local_ix, NReadArgument(nth_arg))
                                                   })
                                                   .collect();
        let body = self.compile_expr_seq(body, fdc, true)?;
        Ok(Lambda(name, args, NBindLocal(arg_nodes, Box::new(body.into_nseq()))))
    }

    fn compile_expr(&mut self,
                    e: &SExpr,
                    fdc: &mut FrameDescrChain,
                    tail: bool)
                    -> CompileResult<RawNode> {
        Ok(match e {
            &List(ref es) => {
                match es.as_slice() {
                    // XXX: Disabled since we only have let now.
                    // [Sym(ref tag), Sym(ref name), ref form] if tag == "define" => {
                    //    // Define is not recursive.
                    //    let form = box self.compile_expr(form, fdc, tail);
                    //    let ix = fdc.create_local_slot(Id::named(name));
                    //    NWriteLocal(ix, form)
                    // }
                    [Sym(ref tag), ref arr, ref ix] if tag == "nth#" => {
                        NReadOopArray(Box::new(self.compile_expr(arr, fdc, false)?),
                                      Box::new(self.compile_expr(ix, fdc, false)?))
                    }
                    [Sym(ref tag), ref arr, ref ix] if tag == "nth-i64#" => {
                        NReadI64Array(Box::new(self.compile_expr(arr, fdc, false)?),
                                      Box::new(self.compile_expr(ix, fdc, false)?))
                    }
                    [Sym(ref tag), ref arr, ref ix, ref val] if tag == "set-nth!#" => {
                        NWriteOopArray(Box::new(self.compile_expr(arr, fdc, false)?),
                                       Box::new(self.compile_expr(ix, fdc, false)?),
                                       Box::new(self.compile_expr(val, fdc, false)?))
                    }
                    [Sym(ref tag), ref arr, ref ix, ref val] if tag == "set-nth-i64!#" => {
                        NWriteI64Array(Box::new(self.compile_expr(arr, fdc, false)?),
                                       Box::new(self.compile_expr(ix, fdc, false)?),
                                       Box::new(self.compile_expr(val, fdc, false)?))
                    }
                    [Sym(ref tag), ref arr] if tag == "len#" => {
                        // Generic array length.
                        NReadArrayLength(Box::new(self.compile_expr(arr, fdc, false)?))
                    }
                    [Sym(ref tag), ref len, ref fill] if tag == "mk-arr#" => {
                        NAlloc(MkOopArray(Box::new(self.compile_expr(len, fdc, false)?),
                                          Box::new(self.compile_expr(fill, fdc, false)?)))
                    }
                    [Sym(ref tag), ref len, ref fill] if tag == "mk-arr-i64#" => {
                        NAlloc(MkI64Array(Box::new(self.compile_expr(len, fdc, false)?),
                                          Box::new(self.compile_expr(fill, fdc, false)?)))
                    }
                    [Sym(ref tag), ref car, ref cdr] if tag == "cons#" => {
                        NAlloc(MkPair(Box::new(self.compile_expr(car, fdc, false)?),
                                      Box::new(self.compile_expr(cdr, fdc, false)?)))
                    }
                    [Sym(ref tag), ref e1] if as_prim_o_op(tag).is_some() => {
                        NPrimO(as_prim_o_op(tag).unwrap(),
                               Box::new(self.compile_expr(e1, fdc, false)?))
                    }
                    [Sym(ref tag), ref val] if tag == "mk-box#" => {
                        NAlloc(MkBox(Box::new(self.compile_expr(val, fdc, false)?)))
                    }
                    [Sym(ref tag), ref loc] if tag == "unwrap-box#" => {
                        NReadBox(Box::new(self.compile_expr(loc, fdc, false)?))
                    }
                    [Sym(ref tag), ref loc, ref val] if tag == "set-box!#" => {
                        NWriteBox(Box::new(self.compile_expr(loc, fdc, false)?),
                                  Box::new(self.compile_expr(val, fdc, false)?))
                    }
                    [Sym(ref tag), ref e1, ref e2] if as_prim_ff_op(tag).is_some() => {
                        let n1 = self.compile_expr(e1, fdc, false)?;
                        let n2 = self.compile_expr(e2, fdc, false)?;
                        let op = as_prim_ff_op(tag).unwrap();
                        NPrimFF(op, Box::new( n1), Box::new( n2))
                    }
                    [Sym(ref tag), ref e1, ref e2, ref e3] if tag == "if" => {
                        let n1 = self.compile_expr(e1, fdc, false)?;
                        let n2 = self.compile_expr(e2, fdc, tail)?;
                        let n3 = self.compile_expr(e3, fdc, tail)?;
                        new_if(n1, n2, n3)
                    }
                    [] => {
                        return Err("compile_expr: nil".to_owned());
                    }
                    _ => {
                        // Special forms.
                        if let &Sym(ref tag) = &es[0] {
                            if tag == "begin" {
                                return Ok(self.compile_expr_seq(&es[1..], fdc, tail)?
                                              .into_nseq());
                            } else if tag == "quote" && es.len() == 2 {
                                // XXX: Constant.
                                return Ok(NLit(LitAny(es[1].to_owned())));
                            } else if tag == "and" && es.len() == 3 {
                                return Ok(NIf {
                                    cond: Box::new(self.compile_expr(&es[1], fdc, false)?),
                                    on_true: Box::new(self.compile_expr(&es[2], fdc, tail)?),
                                    on_false: Box::new(NLit(LitAny(Bool(false)))),
                                });
                            } else if tag == "lambda" {
                                let (frame, mb_lam) = fdc.new_inner(|mut new_chain| {
                                    self.compile_lambda(Id::fresh("lambda"),
                                                        &es[1],
                                                        &es[2..],
                                                        &mut new_chain)
                                });
                                let lam = mb_lam?;
                                let name = lam.name();
                                self.add_sc(lam.into_sc(frame));
                                return Ok(NAlloc(MkClosure(name)));
                            } else if tag == "letrec" {
                                let bs = unwrap_bindings(&es[1])?;

                                // Make names.
                                let ixs: Vec<usize> = bs.iter()
                                                        .map(|&(name, _)| {
                                                            fdc.create_local_slot(name)
                                                        })
                                                        .collect();

                                // And allocate closures.
                                let mut ns = vec![];
                                for &(_, ref e) in &bs {
                                    let n = self.compile_expr(e, fdc, false)?;
                                    ns.push(match n {
                                        NAlloc(n) => n,
                                        _ => {
                                            return Err(format!("letrec expect alloc exprs, but \
                                                                got {:?}",
                                                               n))
                                        }
                                    });
                                }

                                let body = self.compile_expr_seq(&es[2..], fdc, tail)?
                                               .into_nseq();

                                return Ok(NRecBindLocal(ixs.into_iter()
                                                           .zip(ns.into_iter())
                                                           .collect(),
                                                        Box::new( body)));
                            } else if tag == "let*" {
                                // Just a sequence of defines.
                                let bs = unwrap_bindings(&es[1])?;
                                let mut bs_ = vec![];
                                for &(name, ref e) in &bs {
                                    let n = self.compile_expr(e, fdc, false)?;
                                    let ix = fdc.create_local_slot(name);
                                    bs_.push((ix, n));
                                }

                                let body = self.compile_expr_seq(&es[2..], fdc, tail)?
                                               .into_nseq();

                                return Ok(NBindLocal(bs_, Box::new( body)));
                            } else if tag == "let" {
                                let bs = unwrap_bindings(&es[1])?;
                                // Independently evaluate the bindees, and
                                // expand the env with all the names at once,
                                // before evaluating the body.
                                let mut ns = vec![];
                                for &(_, ref e) in &bs {
                                    let n = self.compile_expr(e, fdc, false)?;
                                    ns.push(n);
                                }

                                let bs_ = bs.iter()
                                            .map(|&(name, _)| fdc.create_local_slot(name))
                                            .zip(ns.into_iter())
                                            .collect();
                                let body = self.compile_expr_seq(&es[2..], fdc, tail)?
                                               .into_nseq();

                                return Ok(NBindLocal(bs_, Box::new( body)));
                            }
                        }
                        let func = self.compile_expr(&es[0], fdc, false)?;
                        let args = es[1..]
                                       .iter()
                                       .map(|e| self.compile_expr(e, fdc, false).unwrap())
                                       .collect();
                        new_call(func, args, tail)
                    }
                }
            }
            &Sym(ref name) => {
                let name = Id::named(name);
                let slot = fdc.lookup_slot(&name).unwrap_or_else(|| Slot::Global(name));
                NReadSlot(slot)
            }
            &Int(..) | &Bool(..) => NLit(LitAny(e.to_owned())),

        })
    }

    fn compile_expr_seq(&mut self,
                        es: &[SExpr],
                        fdc: &mut FrameDescrChain,
                        tail: bool)
                        -> CompileResult<NSeq0> {
        if es.len() < 1 {
            return Err(format!("empty expr seq"));
        }
        let mut body = vec![];
        for e in &es[..es.len() - 1] {
            body.push(self.compile_expr(e, fdc, false)?);
        }
        let last = self.compile_expr(es.last().unwrap(), fdc, tail)?;
        Ok(NSeq0(body, last))
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
    #[allow(unused)]
    fn prepend_nodes(mut self, mut ns: NodeList) -> Self {
        ns.extend(self.0.drain(..));
        self.0 = ns;
        self
    }

    fn into_nseq(self) -> RawNode {
        if self.0.len() == 0 {
            self.1
        } else {
            NSeq(self.0, Box::new( self.1))
        }
    }

    #[allow(unused)]
    fn into_nodes(mut self) -> NodeList {
        self.0.push(self.1);
        self.0
    }
}

fn as_prim_ff_op(s: &str) -> Option<PrimOpFF> {
    Some(match s {
        "+#" => PrimOpFF::Add,
        "-#" => PrimOpFF::Sub,
        "<#" => PrimOpFF::Lt,
        _ => return None,
    })
}

fn as_prim_o_op(s: &str) -> Option<PrimOpO> {
    Some(match s {
        "display#" => PrimOpO::Display,
        "panic!#" => PrimOpO::Panic,
        "fixnum?#" => PrimOpO::Fixnump,
        "compile-module#" => PrimOpO::CompileModule,
        _ => return None,
    })
}
