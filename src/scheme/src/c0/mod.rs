/// c0: The baseline compiler.

use ast::nir::*;
use ast::nir::RawNode::*;

use assembler::x64::*;
use assembler::x64::R64::*;
use assembler::emit::{Emit, Label};
use assembler::mem::JitMem;

use std::collections::HashMap;
use std::mem::transmute;

pub struct ModuleContext {
    functions: HashMap<String, FunctionContext>,
    function_labels: LabelMap,
    emit: Emit,
    jitmem: Option<JitMem>,
}

type LabelMap = HashMap<String, Label>;

impl ModuleContext {
    pub fn new() -> Self {
        ModuleContext {
            functions: HashMap::new(),
            function_labels: HashMap::new(),
            emit: Emit::new(),
            jitmem: None,
        }
    }

    pub fn add_scdefn(&mut self, sc: ScDefn) {
        self.function_labels.insert(sc.name().to_owned(), Label::new());
        self.functions.insert(sc.name().to_owned(), FunctionContext::new(sc));
    }

    pub fn compile_all(&mut self) -> usize {
        // 1. Compile each function.
        for (_, ref mut f) in &mut self.functions {
            f.compile(&mut self.emit, &mut self.function_labels);
        }

        // 2. Get main.
        let main_offset = self.function_labels.get("main").unwrap().offset().unwrap();
        let jitmem = JitMem::new(self.emit.as_ref());
        self.jitmem = Some(jitmem);
        let start = unsafe { self.jitmem.as_ref().unwrap().start() };
        println!("compile_all: start = {:#x}", start);

        start + main_offset
    }
}

pub struct FunctionContext {
    scdefn: ScDefn,
}

impl FunctionContext {
    fn new(sc: ScDefn) -> Self {
        FunctionContext { scdefn: sc }
    }

    fn compile(&mut self, emit: &mut Emit, labels: &mut LabelMap) {
        emit_nop_until_aligned(emit, 0x16);

        emit.bind(labels.get_mut(self.scdefn.name()).unwrap())
            .push(RBP)
            .mov(RBP, RSP)
            .add(RSP, -8 * (self.scdefn.frame_descr().slot_count() as i32));

        self.scdefn
            .body_mut()
            .traverse(&mut FunctionCompiler {
                emit: emit,
                labels: labels,
            })
            .unwrap();

        emit_epilogue(emit, true);
    }
}

fn emit_epilogue(emit: &mut Emit, want_ret: bool) {
    emit.mov(RSP, RBP)
        .pop(RBP);

    if want_ret {
        emit.ret();
    }
}

struct FunctionCompiler<'a> {
    emit: &'a mut Emit,
    labels: &'a mut LabelMap,
}

impl<'a> NodeTraverser<String> for FunctionCompiler<'a> {
    fn before(&mut self, node: &mut RawNode) -> Result<TraversalDirection, String> {
        match node {
            &mut NMkFixnum(i) => {
                self.emit.mov(RAX, i as i64);
            }
            &mut NCall { ref mut func, ref mut args, is_tail } => {
                for arg in args.iter_mut().rev() {
                    try!(arg.traverse(self));
                    self.emit.push(RAX);
                }
                try!(func.traverse(self));

                for (r, _) in [RDI, RSI, RDX, RCX, R8, R9].iter().zip(args.iter()) {
                    self.emit.pop(*r);
                }
                if is_tail {
                    emit_epilogue(self.emit, false);
                    self.emit.jmp(RAX);
                } else {
                    self.emit.call(RAX);
                }
            }
            &mut NIf { ref mut cond, ref mut on_true, ref mut on_false } => {
                let mut label_false = Label::new();
                let mut label_done = Label::new();

                let special_case = if let &mut NPrimFF(PrimOpFF::Lt, ref mut lhs, ref mut rhs) =
                                          cond.as_mut() {
                    if OPTIMIZE_IF_CMP {
                        try!(rhs.traverse(self));
                        self.emit.push(RAX);
                        try!(lhs.traverse(self));
                        self.emit
                            .pop(TMP)
                            .cmp(RAX, TMP)
                            .jge(&mut label_false);

                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                if !special_case {
                    try!(cond.traverse(self));

                    self.emit
                        .cmp(RAX, 0)
                        .je(&mut label_false);
                }

                try!(on_true.traverse(self));
                self.emit
                    .jmp(&mut label_done)
                    .bind(&mut label_false);

                try!(on_false.traverse(self));
                self.emit.bind(&mut label_done);
            }
            &mut NSeq(ref mut body, ref mut last) => {
                for n in body {
                    try!(n.traverse(self));
                }
                try!(last.traverse(self));
            }
            &mut NReadArgument(arg_ix) => {
                self.emit.mov(RAX, [RDI, RSI, RDX, RCX, R8, R9][arg_ix]);
            }
            &mut NReadGlobal(ref mut name) => {
                self.emit.mov(RAX, self.labels.get_mut(name).unwrap());
            }
            &mut NReadLocal(ix) => {
                self.emit.mov(RAX, &Addr::BD(RBP, -8 * (1 + ix as i32)));
            }
            &mut NWriteLocal(ix, ref mut n) => {
                try!(n.traverse(self));
                self.emit.mov(&Addr::BD(RBP, -8 * (1 + ix as i32)), RAX);
            }
            &mut NPrimFF(ref op, ref mut n1, ref mut n2) => {
                try!(n2.traverse(self));
                self.emit.push(RAX);
                try!(n1.traverse(self));

                match *op {
                    PrimOpFF::Add => {
                        self.emit
                            .add(RAX, &Addr::B(RSP))
                            .add(RSP, 8);
                    }
                    PrimOpFF::Sub => {
                        self.emit
                            .sub(RAX, &Addr::B(RSP))
                            .add(RSP, 8);
                    }
                    PrimOpFF::Lt => {
                        // XXX: Use cmovcc
                        let mut lbl_true = Label::new();
                        let mut lbl_done = Label::new();
                        self.emit
                            .pop(TMP)
                            .cmp(RAX, TMP)
                            .jl(&mut lbl_true)
                            .mov(RAX, 0)
                            .jmp(&mut lbl_done)
                            .bind(&mut lbl_true)
                            .mov(RAX, 1)
                            .bind(&mut lbl_done);
                    }
                }
            }
            &mut NPrimO(ref op, ref mut n1) => {
                try!(n1.traverse(self));
                match *op {
                    PrimOpO::Display => {
                        self.emit
                            .mov(RDI, RAX)
                            .mov(RAX, unsafe { transmute::<_, i64>(display_isize) })
                            .call(RAX);
                    }
                }
            }
            _ => panic!("Unimplemented node: {:?}", node),
        };

        Ok(TraversalDirection::Skip)
    }
}

// Misc

extern "C" fn display_isize(i: isize) {
    println!("display_isize: {}", i);
}

// Caller saved regs.
const TMP: R64 = R10;

const OPTIMIZE_IF_CMP: bool = true;
