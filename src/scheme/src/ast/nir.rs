/// NIR: AST Nodes as IR.
///
/// I choose to use them as the IR since I'd like to experiment with Truffle's
/// approach of specializing nodes by partial evaluation.

use self::RawNode::*;
use super::id::*;
use super::sexpr::SExpr;

use std::ptr;
use std::boxed::Box;
use std::collections::HashMap;

/// A supercombinator is a top-level function without free variables.
#[derive(Debug)]
pub struct ScDefn {
    name: Id,
    args: Vec<Id>,
    frame_descr: FrameDescr,
    body: RawNode,
}

impl ScDefn {
    pub fn new(name: Id, args: Vec<Id>, frame_descr: FrameDescr, body: RawNode) -> Self {
        ScDefn {
            name: name,
            args: args,
            frame_descr: frame_descr,
            body: body,
        }
    }

    pub fn name(&self) -> Id {
        self.name
    }

    pub fn args(&self) -> &[Id] {
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
pub enum Slot {
    Local(usize),
    UpVal(usize),
    Global(Id),
}

#[derive(Debug)]
pub struct FrameDescr {
    next_local_ix: usize,
    next_upval_ix: usize,

    // List of upvals required to build this closure.
    upval_refs: Vec<Slot>,

    slot_map: SlotMap,
}

type SlotMap = HashMap<Id, Slot>;

impl FrameDescr {
    pub fn new() -> Self {
        FrameDescr {
            next_local_ix: 0,
            next_upval_ix: 0,
            upval_refs: vec![],
            slot_map: Default::default(),
        }
    }

    pub fn upval_refs(&self) -> &[Slot] {
        &self.upval_refs
    }

    pub fn local_slot_count(&self) -> usize {
        self.next_local_ix
    }

    pub fn lookup_slot(&self, name: &Id) -> Option<&Slot> {
        self.slot_map.get(name)
    }

    fn create_slot(&mut self, name: Id, slot: Slot) -> &Slot {
        use std::collections::hash_map::Entry::*;

        if self.slot_map.contains_key(&name) {
            panic!("Duplicated slot: {:?}", name);
        }

        match self.slot_map.entry(name) {
            Vacant(v) => v.insert(slot),
            _ => panic!("Shouldn't happen"),
        }
    }

    pub fn create_local_slot(&mut self, name: Id) -> usize {
        let ix = self.next_local_ix;
        self.next_local_ix += 1;
        self.create_slot(name, Slot::Local(ix));
        ix
    }

    fn create_upval_slot(&mut self, name: Id, outer_slot: Slot) -> &Slot {
        let ix = self.next_upval_ix;
        self.next_upval_ix += 1;
        self.upval_refs.push(outer_slot);
        let res = self.create_slot(name, Slot::UpVal(ix));
        res
    }
}

pub struct FrameDescrChain {
    fd: FrameDescr,
    outer: *mut FrameDescrChain,
}

impl FrameDescrChain {
    pub fn new() -> Self {
        FrameDescrChain {
            fd: FrameDescr::new(),
            outer: ptr::null_mut(),
        }
    }

    pub fn into_current(self) -> FrameDescr {
        self.fd
    }

    pub fn new_inner<A, F: FnMut(&mut Self) -> A>(&mut self, mut f: F) -> (FrameDescr, A) {
        let mut fdc = FrameDescrChain {
            fd: FrameDescr::new(),
            outer: self as *mut _,
        };
        let a = f(&mut fdc);
        (fdc.into_current(), a)
    }

    fn outer<'b>(&self) -> Option<&'b mut Self> {
        if self.outer.is_null() {
            None
        } else {
            Some(unsafe { &mut *self.outer })
        }
    }

    pub fn create_local_slot(&mut self, name: Id) -> usize {
        self.fd.create_local_slot(name)
    }

    pub fn lookup_slot(&mut self, name: &Id) -> Option<Slot> {
        if let x @ Some(_) = self.fd.lookup_slot(name) {
            return x.cloned();
        }

        if let Some(outer) = self.outer() {
            if let Some(outer_slot) = outer.lookup_slot(name) {
                return Some(self.fd.create_upval_slot(name.to_owned(), outer_slot.to_owned())).cloned();
            }
        }

        None
    }
}

impl Default for FrameDescrChain {
    fn default() -> Self {
        FrameDescrChain::new()
    }
}

#[derive(Debug)]
pub enum AllocNode {
    MkPair(Node, Node),
    MkBox(Node),
    MkOopArray(Node /* length */, Node /* fill */),
    MkI64Array(Node /* length */, Node /* fill */),
    MkClosure(Id),
}

#[derive(Debug)]
pub enum LiteralNode {
    LitAny(SExpr),
}

impl LiteralNode {
    pub fn as_int(&self) -> Option<i64> {
        use self::LiteralNode::*;
        match self {
            &LitAny(ref e) => {
                match e {
                    &SExpr::Int(i) => Some(i),
                    _ => None,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum RawNode {
    NLit(LiteralNode),
    NAlloc(AllocNode),

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
    NReadSlot(Slot),
    // NReadClosure(usize),
    NBindLocal(Vec<(usize, RawNode)> /* binding */, Node /* cont */),

    // Let's restrict letrec's binders to be lambdas only for now.
    // This simplifies things quite a bit, as this eliminates anonymous payloads.
    NRecBindLocal(Vec<(usize, AllocNode)> /* recursive binding */, Node /* cont */),

    NReadOopArray(Node, Node),
    NWriteOopArray(Node, Node, Node),

    NReadI64Array(Node, Node),
    NWriteI64Array(Node, Node, Node),

    // Generic
    NReadArrayLength(Node),

    NReadBox(Node),
    NWriteBox(Node, Node),

    // PrimOps.
    NPrimFF(PrimOpFF, Node, Node), // Fixnum binary ops.
    NPrimO(PrimOpO, Node), // Oop binary ops.
}

#[derive(Debug, Clone, Copy)]
pub enum PrimOpO {
    Display,
    Panic,
    Fixnump,
    CompileModule,
}

#[derive(Debug, Clone, Copy)]
pub enum PrimOpFF {
    Add,
    Eq,
    Lt,
    Sub,
}

pub fn op_is_cond(op: PrimOpFF) -> bool {
    match op {
        PrimOpFF::Lt | PrimOpFF::Eq => true,
        _ => false,
    }
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

pub trait Traverse : Sized {
    fn traverse<E, T: NodeTraverser<E>>(&mut self, t: &mut T) -> Result<(), E>;
}

impl Traverse for RawNode {
    // Monadic node traversals. The fixpoint functor could greatly simplify
    // this process but Rust currently doesn't have that.
    fn traverse<E, T: NodeTraverser<E>>(&mut self, t: &mut T) -> Result<(), E> {
        use self::TraversalDirection::*;

        let direction = t.before(self)?;
        match direction {
            Forward => {
                match self {
                    &mut NReadArrayLength(ref mut n) |
                    &mut NReadBox(ref mut n) |
                    &mut NPrimO(_, ref mut n) => {
                        n.traverse(t)?;
                    }

                    &mut NAlloc(ref mut n) => n.traverse(t)?,

                    &mut NWriteBox(ref mut n1, ref mut n2) |
                    &mut NReadOopArray(ref mut n1, ref mut n2) |
                    &mut NReadI64Array(ref mut n1, ref mut n2) |
                    &mut NPrimFF(_, ref mut n1, ref mut n2) => {
                        n1.traverse(t)?;
                        n2.traverse(t)?;
                    }

                    &mut NWriteOopArray(ref mut n1, ref mut n2, ref mut n3) |
                    &mut NWriteI64Array(ref mut n1, ref mut n2, ref mut n3) => {
                        n1.traverse(t)?;
                        n2.traverse(t)?;
                        n3.traverse(t)?;
                    }

                    &mut NCall { ref mut func, ref mut args, .. } => {
                        func.traverse(t)?;
                        for arg in args {
                            arg.traverse(t)?;
                        }
                    }
                    &mut NIf { ref mut cond, ref mut on_true, ref mut on_false } => {
                        cond.traverse(t)?;
                        on_true.traverse(t)?;
                        on_false.traverse(t)?;
                    }
                    &mut NSeq(ref mut ns, ref mut n) => {
                        for n in ns {
                            n.traverse(t)?;
                        }
                        n.traverse(t)?;
                    }

                    &mut NBindLocal(ref mut bs, ref mut n) => {
                        for &mut (_, ref mut b) in bs {
                            b.traverse(t)?;
                        }
                        n.traverse(t)?;
                    }

                    &mut NRecBindLocal(ref mut bs, ref mut n) => {
                        for &mut (_, ref mut b) in bs {
                            b.traverse(t)?;
                        }
                        n.traverse(t)?;
                    }

                    &mut NLit(..) |
                    &mut NReadArgument(..) |
                    &mut NReadSlot(..) => {}
                }
            }
            Skip => {}
        }
        Ok(())
    }
}

impl Traverse for AllocNode {
    fn traverse<E, T: NodeTraverser<E>>(&mut self, t: &mut T) -> Result<(), E> {
        use self::AllocNode::*;

        match self {
            &mut MkBox(ref mut n) => n.traverse(t),
            &mut MkPair(ref mut n1, ref mut n2) |
            &mut MkOopArray(ref mut n1, ref mut n2) |
            &mut MkI64Array(ref mut n1, ref mut n2) => {
                n1.traverse(t)?;
                n2.traverse(t)
            }
            &mut MkClosure(..) => Ok(()),
        }
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
        cond: Box::new(cond),
        on_true: Box::new(on_true),
        on_false: Box::new(on_false),
    }
}

pub type Node = Box<RawNode>;
pub type NodeList = Vec<RawNode>;
