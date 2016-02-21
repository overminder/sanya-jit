use bit_vec::BitVec;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct NodeIx(usize);

impl NodeIx {
    fn advance(self) -> Self {
        NodeIx(self.0 + 1)
    }
}

// XXX: Might not be enough.
const MAX_NODES: usize = 16;

pub struct Graph<N> {
    nodes: Vec<N>,
    small_matrix: BitVec,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Direction {
    In,
    Out,
}

impl<N> Graph<N> {
    pub fn new() -> Self {
        Graph {
            nodes: vec![],
            small_matrix: BitVec::from_elem(MAX_NODES.pow(2), false),
        }
    }

    fn next_node_ix(&self) -> NodeIx {
        NodeIx(self.nodes.len())
    }

    pub fn add_node(&mut self, n: N) -> NodeIx {
        let nid = self.next_node_ix();
        self.nodes.push(n);
        nid
    }

    pub fn get(&self, v: NodeIx) -> Option<&N> {
        self.nodes.get(v.0)
    }

    pub fn get_mut(&mut self, v: NodeIx) -> Option<&mut N> {
        self.nodes.get_mut(v.0)
    }

    pub fn add_edge(&mut self, src: NodeIx, dst: NodeIx) {
        self.small_matrix.set(edge_to_ix(src, dst), true);
    }

    pub fn has_edge(&self, src: NodeIx, dst: NodeIx) -> bool {
        self.small_matrix[edge_to_ix(src, dst)]
    }

    pub fn adjacent<'a>(&'a self, pov: NodeIx, dir: Direction) -> AdjacentIterator<'a, N> {
        AdjacentIterator::new(self, pov, dir)
    }
}

fn edge_to_ix(src: NodeIx, dst: NodeIx) -> usize {
    let src = src.0;
    let dst = dst.0;
    assert!(src <= MAX_NODES);
    assert!(dst <= MAX_NODES);

    src * MAX_NODES + dst
}

pub struct AdjacentIterator<'a, N: 'a> {
    g: &'a Graph<N>,
    pov: NodeIx,
    dir: Direction,
    ix: NodeIx,
}

impl<'a, N> AdjacentIterator<'a, N> {
    fn new(g: &'a Graph<N>, pov: NodeIx, dir: Direction) -> Self {
        AdjacentIterator {
            g: g,
            pov: pov,
            dir: dir,
            ix: NodeIx(0),
        }
    }
}

impl<'a, N> Iterator for AdjacentIterator<'a, N> {
    type Item = NodeIx;

    fn next(&mut self) -> Option<NodeIx> {
        while self.ix.0 < MAX_NODES {
            let ix = self.ix;
            self.ix = self.ix.advance();
            match self.dir {
                Direction::In => {
                    if self.g.has_edge(ix, self.pov) {
                        return Some(ix);
                    }
                }
                Direction::Out => {
                    if self.g.has_edge(self.pov, ix) {
                        return Some(ix);
                    }
                }
            }
        }
        return None;
    }
}

#[test]
fn test_add_node() {
    let mut g = Graph::new();
    let ixs: Vec<(usize, NodeIx)> = (1..MAX_NODES).map(|i| (i, g.add_node(i))).collect();
    for (n, ix) in ixs {
        assert_eq!(n, *g.get(ix).unwrap());
    }
}

#[test]
fn test_add_edge() {
    let mut g = Graph::new();
    let ixs: Vec<(usize, NodeIx)> = (1..MAX_NODES).map(|i| (i, g.add_node(i))).collect();
    for &(_, ix0) in &ixs {
        for &(_, ix1) in &ixs {
            g.add_edge(ix0, ix1);
        }
    }
    for &(_, ix0) in &ixs {
        for &(_, ix1) in &ixs {
            assert!(g.has_edge(ix0, ix1));
        }
    }
}

#[test]
fn test_iter_adjacent() {
    let mut g = Graph::new();
    let ixs: Vec<(usize, NodeIx)> = (1..MAX_NODES).map(|i| (i, g.add_node(i))).collect();
    for &(_, ix0) in &ixs {
        for &(_, ix1) in &ixs {
            g.add_edge(ix0, ix1);
        }
    }
    let ixs0: Vec<NodeIx> = ixs.iter().map(|&(_, ix)| ix).collect();
    for &(_, ix0) in &ixs {
        let from_here: Vec<NodeIx> = g.adjacent(ix0, Direction::Out).collect();
        assert_eq!(from_here, ixs0);
    }
}
