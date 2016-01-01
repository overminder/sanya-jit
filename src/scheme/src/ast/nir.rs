/// NIR: AST Nodes as IR.
///
/// I choose to use them as the IR since I'd like to experiment with Truffle's
/// approach of specializing nodes by partial evaluation.

use self::RawNode::*;

use std::boxed::Box;
use std::collections::HashMap;

/// A supercombinator is a top-level function without free variables.
pub struct ScDefn {
    name: String,
    args: Vec<String>,
    frame_descr: FrameDescr,
    body: RawNode,
}

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
    // Allocations
    NMkFixnum(isize),
    NMkPair(Node, Node),

    // Flow controls.
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
    NReadGlobal(String),
    NReadLocal(usize),
    NWriteGlobal(String, Node),
    NWriteLocal(usize, Node),

    // PrimOps.
    NPrimFixnumAdd(Node, Node),
    NPrimFixnumLt(Node, Node),
    NPrimFixnumSub(Node, Node),
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
