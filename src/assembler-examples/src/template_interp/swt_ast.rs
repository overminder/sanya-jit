use super::stack_with_trace::Op;

pub enum Node {
    Add(Box<Node>, Box<Node>),
    ReadLocal(u8),
    LitI8(i8),
}

pub enum Stmt {
    BranchLt(Node, Node, i8 /* bc offset */, bool /* taken */),
}

pub fn trace_to_node(ops: &[*const u8]) -> Node {
    let mut i = 0;
    let mut stack = vec![];
    let mut stmts = vec![];
    let res;

    fn next_op(ops: &[*const u8], i: &mut usize) -> Op {
        let op = Op::from(unsafe { *ops[*i] });
        *i += 1;
        op
    }

    fn next_oparg(ops: &[*const u8], i: &mut usize) -> u8 {
        let oparg = unsafe { *ops[*i] };
        *i += 1;
        oparg
    }

    fn figure_out_next_op_ptr(ops: &[*const u8], i: usize) -> *const u8 {
        if i >= ops.len() {
            ops[0]
        } else {
            ops[i]
        }
    }

    loop {
        match next_op(ops, &mut i) {
            Op::LoadI8 => {
                stack.push(Node::LitI8(next_oparg(ops, &mut i) as i8));
                i += 2;
            }
            Op::Ret => {
                res = stack.pop().unwrap();
                let nlocals = next_oparg(ops, &mut i) as usize;
                assert_eq!(stack.len(), nlocals);
                break;
            }
            Op::Add => {
                let r = stack.pop().unwrap();
                let l = stack.pop().unwrap();
                stack.push(Node::Add(box r, box l));
            }
            Op::BranchLt => {
                let op_ptr = ops[i - 1];
                let pc_incr = next_oparg(ops, &mut i) as isize;
                let r = stack.pop().unwrap();
                let l = stack.pop().unwrap();
                let branch_target = figure_out_next_op_ptr(ops, i);
                let taken = unsafe {
                    if branch_target == op_ptr.offset(2 + pc_incr) {
                        true
                    } else if branch_target == op_ptr.offset(2) {
                        false
                    } else {
                        panic!("invalid branch target");
                    }
                };
                stmts.push(Stmt::BranchLt(r, l, pc_incr as i8, taken));
            }
            _ => {
                panic!();
            }
        }
    }
    res
}
