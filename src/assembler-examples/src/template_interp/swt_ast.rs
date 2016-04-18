use super::stack_with_trace::Op;

enum Node {
    Add(Box<Node>, Box<Node>),
    ReadLocal(u8),
    LitI8(i8),
}

fn ops_to_node(ops: &[u8]) -> Node {
    let mut i = 0;
    let mut stack = vec![];
    let res;
    loop {
        match Op::from(ops[i]) {
            Op::LoadI8 => {
                stack.push(Node::LitI8(ops[i + 1] as i8));
                i += 2;
            }
            Op::Ret => {
                res = stack.pop().unwrap();
                assert_eq!(stack.len(), ops[i + 1] as usize);
                break;
            }
            _ => {
                panic!();
            }
        }
    }
    res
}
