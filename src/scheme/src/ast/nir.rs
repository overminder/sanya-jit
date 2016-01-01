/// NIR: AST Nodes as IR.
///
/// I choose to use them as the IR since I'd like to experiment with Truffle's
/// approach of specializing nodes by partial evaluation.

use std::boxed::Box;

use self::Node::*;

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
    NReadLocal(isize),
    NWriteGlobal(String, Node),
    NWriteLocal(isize, Node),

    // PrimOps.
    NPrimFixnumAdd(Node, Node),
    NPrimFixnumLt(Node, Node),
    NPrimFixnumSub(Node, Node),
}

pub type Node = Box<RawNode>;
pub type NodeList = Vec<RawNode>;
