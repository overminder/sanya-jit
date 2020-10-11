use super::stack_with_trace::Op;

#[derive(Eq, PartialEq, Debug)]
pub enum Node {
    Add(Box<Node>, Box<Node>),
    ReadLocal(u8),
    LitI8(i8),
}

#[derive(Eq, PartialEq, Debug)]
pub enum Stmt {
    BranchLt(Node, Node, i8 /* bc offset */, bool /* taken */),
    Ret(Node, u8 /* nlocals */),
    Seq(Vec<Stmt>),
}

pub fn trace_to_node(ops: &[*const u8]) -> Stmt {
    let mut i = 0;
    let mut stack = vec![];
    let mut stmts = vec![];

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

    while i < ops.len() {
        match next_op(ops, &mut i) {
            Op::LoadI8 => {
                stack.push(Node::LitI8(next_oparg(ops, &mut i) as i8));
                i += 2;
            }
            Op::Ret => {
                // Call / Ret should be handled in two flavors:
                // - A ret paired with a call can be inlined
                // - An unpaired call or ret means recursive activation
                //   and should be treated differently.
                let nlocals = next_oparg(ops, &mut i) as usize;
                if stack.len() >= nlocals + 1 {
                    // Should be paired
                    for _ in 0..nlocals {
                        stack.pop();
                    }
                    let top = stack.pop().unwrap();
                    stmts.push(Stmt::Ret(top, nlocals as u8));
                } else if stack.len() >= 1 {
                    // Can only get the return value
                    let top = stack.pop().unwrap();
                    stmts.push(Stmt::Ret(top, nlocals as u8));
                } else {
                    // Nothing on the stack
                }
            }
            Op::Add => {
                let r = stack.pop().unwrap();
                let l = stack.pop().unwrap();
                stack.push(Node::Add(Box::new(r), Box::new(l)));
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
            Op::LoadL => {
                // Have the same problem: the value might not be pushed in this trace...
            }
            Op::Call => {
                let op_ptr = ops[i - 1];
                let pc_incr = next_oparg(ops, &mut i) as isize;
                // XXX: Consider using a saner calling convention...
            }
            Op::Enter => {
                let nargs = next_oparg(ops, &mut i) as isize;
            }
            Op::Halt => {
                panic!("Shouldn't happen in a trace");
            }
        }
    }
    Stmt::Seq(stmts)
}
