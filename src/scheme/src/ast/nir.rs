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

    pub fn args(&self) -> &[String] {
        &self.args
    }

    pub fn arity(&self) -> usize {
        self.args.len()
    }

    pub fn frame_descr(&self) -> &FrameDescr {
        &self.frame_descr
    }

    pub fn body(&self) -> &RawNode {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut RawNode {
        &mut self.body
    }
}

#[derive(Debug, Clone)]
pub enum Id {
    Named(String),
    Unnamed(usize),
}

#[derive(Debug)]
pub enum Slot {
    Local(usize),
    UpVal(usize),
}

#[derive(Debug)]
pub struct FrameDescr {
    next_local_ix: usize,
    next_upval_ix: usize,
    slot_map: SlotMap,
}

type SlotMap = HashMap<String, Slot>;

impl FrameDescr {
    pub fn new() -> Self {
        FrameDescr {
            next_local_ix: 0,
            next_upval_ix: 0,
            slot_map: Default::default(),
        }
    }

    pub fn local_slot_count(&self) -> usize {
        self.next_local_ix
    }

    pub fn lookup_slot(&self, name: &str) -> Option<&Slot> {
        self.slot_map.get(name)
    }

    fn create_slot(&mut self, name: &str, slot: Slot) -> &Slot {
        use std::collections::hash_map::Entry::*;

        match self.slot_map.entry(name.to_owned()) {
            Occupied(_) => panic!("Duplicated slot: {}", name),
            Vacant(v) => v.insert(slot),
        }
    }

    pub fn create_local_slot(&mut self, name: &str) -> usize {
        let ix = self.next_local_ix;
        self.next_local_ix += 1;
        self.create_slot(name, Slot::Local(ix));
        ix
    }

    fn create_upval_slot(&mut self, name: &str) -> &Slot {
        let ix = self.next_upval_ix;
        self.next_upval_ix += 1;
        let res = self.create_slot(name, Slot::UpVal(ix));
        res
    }
}

pub struct FrameDescrChain<'a> {
    fd: FrameDescr,
    outer: Option<&'a mut FrameDescrChain<'a>>,
}

impl<'a> FrameDescrChain<'a> {
    pub fn new() -> Self {
        FrameDescrChain {
            fd: FrameDescr::new(),
            outer: None,
        }
    }

    pub fn new_inner(&'a mut self) -> Self {
        FrameDescrChain {
            fd: FrameDescr::new(),
            outer: Some(self),
        }
    }

    // Oops, borrowck bug (rust-lang/rfcs#811)...
    // Fortunately Edward has a solution here:
    // http://blog.ezyang.com/2013/12/two-bugs-in-the-borrow-checker-every-rust-developer-should-know-about/
    pub fn lookup_slot<A, F: FnOnce(Option<&Slot>) -> A>(&mut self, name: &str, f: F) -> A {
        if let x @ Some(_) = self.fd.lookup_slot(name) {
            return f(x);
        }

        match self.outer.as_mut() {
            Some(outer) => {
                if outer.lookup_slot(name, |x| x.is_some()) {
                    return f(Some(self.fd.create_upval_slot(name)));
                }
            }
            _ => (),
        }

        f(None)
    }
}

#[derive(Debug)]
pub enum RawNode {
    // Allocations.
    NMkFixnum(isize),
    NMkPair(Node, Node),
    NMkOopArray(Node /* length */, Node /* fill */),
    NMkI64Array(Node /* length */, Node /* fill */),
    NMkClosure(Vec<String>, Node),

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
    NSeq(NodeList, Node),

    // Storage manipulations.
    NReadArgument(usize),
    NReadGlobal(String),
    // NReadClosure(usize),
    NReadLocal(usize),
    NReadUpVal(usize),
    NWriteLocal(usize, Node),

    NReadOopArray(Node, Node),
    NWriteOopArray(Node, Node, Node),

    NReadI64Array(Node, Node),
    NWriteI64Array(Node, Node, Node),

    // Generic
    NReadArrayLength(Node),

    // PrimOps.
    NPrimFF(PrimOpFF, Node, Node), // Fixnum binary ops.
    NPrimO(PrimOpO, Node), // Oop binary ops.
}

#[derive(Debug, Clone, Copy)]
pub enum PrimOpO {
    Display,
    PanicInlineSym,
    Fixnump,
}

#[derive(Debug, Clone, Copy)]
pub enum PrimOpFF {
    Add,
    Eq,
    Lt,
    Sub,
}

pub trait NodeTraverser<E> {
    fn before(&mut self, _node: &mut RawNode) -> Result<TraversalDirection, E> {
        Ok(TraversalDirection::Forward)
    }
}

/// Defines the way to traverse the children of a given node.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TraversalDirection {
    // Normal applicative order.
    Forward,

    // Don't look into the children.
    Skip,
}

impl RawNode {
    // Monadic node traversals. The fixpoint functor could greatly simplify
    // this process but Rust currently doesn't have that.
    pub fn traverse<E, T: NodeTraverser<E>>(&mut self, t: &mut T) -> Result<(), E> {
        use self::TraversalDirection::*;

        let direction = try!(t.before(self));
        match direction {
            Forward => {
                match self {
                    &mut NWriteLocal(_, ref mut n) |
                    &mut NReadArrayLength(ref mut n) |
                    &mut NPrimO(_, ref mut n) => {
                        try!(n.traverse(t));
                    }

                    &mut NMkPair(ref mut n1, ref mut n2) |
                    &mut NMkOopArray(ref mut n1, ref mut n2) |
                    &mut NMkI64Array(ref mut n1, ref mut n2) |
                    &mut NReadOopArray(ref mut n1, ref mut n2) |
                    &mut NReadI64Array(ref mut n1, ref mut n2) |
                    &mut NPrimFF(_, ref mut n1, ref mut n2) => {
                        try!(n1.traverse(t));
                        try!(n2.traverse(t));
                    }

                    &mut NWriteOopArray(ref mut n1, ref mut n2, ref mut n3) |
                    &mut NWriteI64Array(ref mut n1, ref mut n2, ref mut n3) => {
                        try!(n1.traverse(t));
                        try!(n2.traverse(t));
                        try!(n3.traverse(t));
                    }

                    &mut NCall { ref mut func, ref mut args, .. } => {
                        try!(func.traverse(t));
                        for arg in args {
                            try!(arg.traverse(t));
                        }
                    }
                    &mut NIf { ref mut cond, ref mut on_true, ref mut on_false } => {
                        try!(cond.traverse(t));
                        try!(on_true.traverse(t));
                        try!(on_false.traverse(t));
                    }
                    &mut NSeq(ref mut ns, ref mut n) => {
                        for n in ns {
                            try!(n.traverse(t));
                        }
                        try!(n.traverse(t));
                    }
                    _ => {}
                }
            }
            Skip => {}
        }
        Ok(())
    }
}

pub fn new_call(func: RawNode, args: NodeList, is_tail: bool) -> RawNode {
    assert!(args.len() <= 6, "argc > 6 not implemented");
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
