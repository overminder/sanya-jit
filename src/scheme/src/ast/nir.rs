/// NIR: AST Nodes as IR.
///
/// I choose to use them as the IR since I'd like to experiment with Truffle's
/// approach of specializing nodes by partial evaluation.

use self::RawNode::*;

use std::boxed::Box;
use std::collections::HashMap;

/// A supercombinator is a top-level function without free variables.
#[derive(Debug)]
pub struct ScDefn {
    name: String,
    args: Vec<String>,
    frame_descr: FrameDescr,
    body: RawNode,
}

impl ScDefn {
    pub fn new(name: String, args: Vec<String>, frame_descr: FrameDescr, body: RawNode) -> Self {
        ScDefn {
            name: name,
            args: args,
            frame_descr: frame_descr,
            body: body,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &RawNode {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut RawNode {
        &mut self.body
    }
}

#[derive(Debug)]
pub struct FrameDescr {
    next_ix: usize,
    name_to_ix: HashMap<String, usize>,
}

impl FrameDescr {
    pub fn new() -> Self {
        FrameDescr {
            next_ix: 0,
            name_to_ix: HashMap::new(),
        }
    }

    pub fn lookup_slot(&self, name: &str) -> Option<usize> {
        self.name_to_ix.get(name).cloned()
    }

    pub fn find_or_create_slot(&mut self, name: &str) -> usize {
        if let Some(ix) = self.lookup_slot(name) {
            return ix;
        }

        let ix = self.next_ix;
        self.next_ix += 1;
        self.name_to_ix.insert(name.to_owned(), ix);
        ix
    }
}

#[derive(Debug)]
pub enum RawNode {
    // Allocations.
    NMkFixnum(isize),
    NMkPair(Node, Node),

    // Control flows.
    NCall {
        func: Node,
        args: NodeList,
        is_tail: bool,
    },
    NIf {
        cond: Node,
        on_true: Node,
        on_false: Node,
    },
    NSeq(NodeList),

    // Storage manipulations.
    NReadArgument(usize),
    NReadGlobal(String),
    NReadLocal(usize),
    NWriteLocal(usize, Node),

    // PrimOps.
    NPrimFF(PrimOpFF, Node, Node), // Fixnum binary ops.
}

#[derive(Debug)]
pub enum PrimOpFF {
    Add,
    Lt,
    Sub,
}

impl RawNode {
    // Monadic node traversals. The fixpoint functor could greatly simplify
    // this process but Rust currently doesn't have that.
    //
    // It accepts a `&mut F` rather than a `F` because of https://stackoverflow.com/a/31197781
    pub fn foreach_readglobal<E, F: Fn(&mut String) -> Result<(), E>>(&mut self,
                                                                      f: &mut F)
                                                                      -> Result<(), E> {
        match self {
            &mut NMkPair(ref mut n1, ref mut n2) => {
                try!(n1.foreach_readglobal(f));
                try!(n2.foreach_readglobal(f));
            }
            &mut NCall { ref mut func, ref mut args, .. } => {
                try!(func.foreach_readglobal(f));
                for arg in args {
                    try!(arg.foreach_readglobal(f));
                }
            }
            &mut NIf { ref mut cond, ref mut on_true, ref mut on_false } => {
                try!(cond.foreach_readglobal(f));
                try!(on_true.foreach_readglobal(f));
                try!(on_false.foreach_readglobal(f));
            }
            &mut NSeq(ref mut ns) => {
                for n in ns {
                    try!(n.foreach_readglobal(f));
                }
            }
            &mut NReadGlobal(ref mut g) => try!(f(g)),
            &mut NWriteLocal(_, ref mut n) => try!(n.foreach_readglobal(f)),
            &mut NPrimFF(_, ref mut n1, ref mut n2) => {
                try!(n1.foreach_readglobal(f));
                try!(n2.foreach_readglobal(f));
            }
            _ => (),
        }

        Ok(())
    }
}

pub fn new_call(func: RawNode, args: NodeList, is_tail: bool) -> RawNode {
    NCall {
        func: Box::new(func),
        args: args,
        is_tail: is_tail,
    }
}

pub fn new_if(cond: RawNode, on_true: RawNode, on_false: RawNode) -> RawNode {
    NIf {
        cond: box cond,
        on_true: box on_true,
        on_false: box on_false,
    }
}

pub type Node = Box<RawNode>;
pub type NodeList = Vec<RawNode>;
