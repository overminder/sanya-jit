/// c0: The baseline compiler.

use ast::nir::*;
use ast::nir::RawNode::*;
use rt::*;
use rt::stackmap::StackMap;
use rt::inlinesym::InlineSym;

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

    pub fn compile_all(&mut self, u: &mut Universe) -> JitEntry {
        // 1. Compile each function.
        for (_, ref mut f) in &mut self.functions {
            f.compile(&mut self.emit, &mut self.function_labels);
        }

        // 2. Make a rust -> jitted main entry
        unsafe { transmute::<usize, JitEntry>(self.make_rust_entry(u)) }
    }

    fn make_rust_entry(&mut self, u: &mut Universe) -> usize {
        // 1. Build up an entry.
        let mut entry = Label::new();
        let main = self.function_labels.get_mut("main").unwrap();
        emit_nop_until_aligned(&mut self.emit, 0x10);

        self.emit
            .bind(&mut entry)
            .push(RBP)
            .mov(RBP, RSP)
            .push(ALLOC_PTR)
            .push(UNIVERSE_PTR)
            .push(STACKMAP_PTR)
            .add(RSP, -8)
            .mov(&universe_base_rbp(), RBP)
            .mov(UNIVERSE_PTR, RDI)
            .mov(ALLOC_PTR, &universe_alloc_ptr())
            .mov(STACKMAP_PTR, 0_i64)
            .call(main)
            .mov(&universe_alloc_ptr(), ALLOC_PTR)
            .add(RSP, 8)
            .pop(STACKMAP_PTR)
            .pop(UNIVERSE_PTR)
            .pop(ALLOC_PTR)
            .mov(RSP, RBP)
            .pop(RBP)
            .ret();

        // 2. Get it.
        let entry_offset = entry.offset().unwrap();
        let jitmem = JitMem::new(self.emit.as_ref());
        self.jitmem = Some(jitmem);
        let start = unsafe { self.jitmem.as_ref().unwrap().start() };
        println!("compile_all: entry = {:#x}", start);

        start + entry_offset
    }
}

pub type JitEntry = unsafe extern "C" fn(*const Universe);

pub struct FunctionContext {
    scdefn: ScDefn,
}

impl FunctionContext {
    fn new(sc: ScDefn) -> Self {
        FunctionContext { scdefn: sc }
    }

    fn compile(&mut self, emit: &mut Emit, labels: &mut LabelMap) {
        // Align the function entry.
        // XXX: I'm not sure if this is correct - need to read the AMD64 ABI
        // doc for the correct alignment.
        emit_nop_until_aligned(emit, 0x10);

        // Bind the function entry. The unwrap here is safe the ModuleContext
        // has already initialized all the labels in the label map.
        emit.bind(labels.get_mut(self.scdefn.name()).unwrap());
        emit_prologue(emit, self.scdefn.frame_descr().slot_count());

        let stackmap = new_stackmap_with_frame_descr(self.scdefn.frame_descr());
        NodeCompiler::new(emit, labels).compile(self.scdefn.body_mut(), stackmap);

        emit_epilogue(emit, true);
    }
}


struct NodeCompiler<'a> {
    emit: &'a mut Emit,
    labels: &'a mut LabelMap,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum CallingConv {
    Internal,
    SyncUniverse,
}

impl<'a> NodeCompiler<'a> {
    fn new(emit: &'a mut Emit, labels: &'a mut LabelMap) -> Self {
        NodeCompiler {
            emit: emit,
            labels: labels,
        }
    }

    fn calling_out<F: FnMut(&mut Emit)>(&mut self,
                                        mut stackmap: StackMap,
                                        cconv: CallingConv,
                                        mut f: F) {
        // Ensure the stack is 16-byte aligned after the call.
        let sp_changed = if (stackmap.len() + 1) & 1 == 1 {
            self.emit.add(RSP, -8);
            stackmap.push_word();
            true
        } else {
            false
        };

        // And materialize the stackmap, since we are calling out.
        self.emit.mov(STACKMAP_PTR, stackmap.as_word() as i64);

        if cconv == CallingConv::SyncUniverse {
            // Sync the runtime regs with the universe.
            self.emit
                .mov(&universe_alloc_ptr(), ALLOC_PTR)
                .mov(&universe_saved_rbp(), RBP)
                .mov(&universe_stackmap_ptr(), STACKMAP_PTR);
        }

        // Do the actuall call.
        f(self.emit);

        if cconv == CallingConv::SyncUniverse {
            // Read back the runtime regs.
            self.emit
                .mov(ALLOC_PTR, &universe_alloc_ptr());
        }

        // And restore the stack.
        if sp_changed {
            self.emit.add(RSP, 8);
        }
    }

    fn push_oop(&mut self, r: R64, stackmap: &mut StackMap) {
        self.emit.push(r);
        stackmap.push_gcptr();
    }

    fn compile(&mut self, node: &mut RawNode, stackmap: StackMap) -> Result<(), String> {
        match node {
            &mut NMkFixnum(i) => {
                self.emit.mov(RAX, i as i64);
            }
            &mut NCall { ref mut func, ref mut args, is_tail } => {
                let mut precall_map = stackmap;
                for arg in args.iter_mut().rev() {
                    try!(self.compile(arg, precall_map));
                    self.push_oop(RAX, &mut precall_map);
                }
                try!(self.compile(func, precall_map));

                for (r, _) in [RDI, RSI, RDX, RCX, R8, R9].iter().zip(args.iter()) {
                    self.emit.pop(*r);
                }
                if is_tail {
                    emit_epilogue(self.emit, false);
                    self.emit.jmp(RAX);
                } else {
                    self.calling_out(stackmap, CallingConv::Internal, |emit| {
                        emit.call(RAX);
                    });
                }
            }
            &mut NIf { ref mut cond, ref mut on_true, ref mut on_false } => {
                let mut label_false = Label::new();
                let mut label_done = Label::new();

                let special_case = if let &mut NPrimFF(PrimOpFF::Lt, ref mut lhs, ref mut rhs) =
                                          cond.as_mut() {
                    if OPTIMIZE_IF_CMP {
                        let mut map0 = stackmap;
                        try!(self.compile(rhs, map0));
                        self.push_oop(RAX, &mut map0);
                        try!(self.compile(lhs, map0));
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
                    try!(self.compile(cond, stackmap));

                    self.emit
                        .cmp(RAX, 0)
                        .je(&mut label_false);
                }

                try!(self.compile(on_true, stackmap));
                self.emit
                    .jmp(&mut label_done)
                    .bind(&mut label_false);

                try!(self.compile(on_false, stackmap));
                self.emit.bind(&mut label_done);
            }
            &mut NSeq(ref mut body, ref mut last) => {
                for n in body {
                    try!(self.compile(n, stackmap));
                }
                try!(self.compile(last, stackmap));
            }
            &mut NReadArgument(arg_ix) => {
                self.emit.mov(RAX, [RDI, RSI, RDX, RCX, R8, R9][arg_ix]);
            }
            &mut NReadGlobal(ref mut name) => {
                self.emit.lea(RAX, self.labels.get_mut(name).unwrap());
            }
            &mut NReadLocal(ix) => {
                self.emit.mov(RAX, &frame_slot(ix));
            }
            &mut NWriteLocal(ix, ref mut n) => {
                try!(self.compile(n, stackmap));
                self.emit.mov(&frame_slot(ix), RAX);
            }
            &mut NPrimFF(ref op, ref mut n1, ref mut n2) => {
                let mut map0 = stackmap;
                try!(self.compile(n2, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(n1, map0));

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
                try!(self.compile(n1, stackmap));
                match *op {
                    PrimOpO::Display => {
                        self.emit
                            .mov(RDI, RAX)
                            .mov(RAX, unsafe { transmute::<_, i64>(display_isize) });
                        self.calling_out(stackmap, CallingConv::Internal, |emit| {
                            emit.call(RAX);
                        });
                    }
                    PrimOpO::PanicInlineSym => {
                        self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                            emit.mov(RDI, RAX)
                                .mov(RSI, UNIVERSE_PTR)
                                .mov(RAX, unsafe { transmute::<_, i64>(panic_inline_sym) })
                                .call(RAX);
                        });
                    }
                }
            }
            _ => panic!("Unimplemented node: {:?}", node),
        };

        Ok(())
    }
}

// Misc

/// Frame Layout:
///
/// Higher Addr
/// ^
/// | ix                         | value
/// +----------------------------+------------
/// | rbp + 8                    | ret addr
/// | rbp                        | saved rbp
/// | rbp - 8                    | saved StackMap.as_word
/// | rbp - (8 + 8 * local_ix)   | local variable slots
/// | rsp + N ~ rsp              | local tmps (might contain an alignment slot)
///
/// Stack Map:
/// A stackmap contains all the local variables and tmps.
/// rbp - stackmap.len() * 8 points to the saved stackmap.

fn universe_alloc_ptr() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_ALLOC_PTR)
}

fn universe_alloc_limit() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_ALLOC_LIMIT)
}

fn universe_stackmap_ptr() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_STACKMAP_PTR)
}

fn universe_saved_rbp() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_SAVED_RBP)
}

fn universe_base_rbp() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_BASE_RBP)
}

fn frame_slot(ix: usize) -> Addr {
    // rbp[ix*8-16] since rbp[-8] is used to save the caller's stackmap.
    Addr::BD(RBP, -8 * (2 + ix as i32))
}

fn saved_stackmap() -> Addr {
    Addr::BD(RBP, -8)
}

fn emit_prologue(emit: &mut Emit, frame_slots: usize) {
    emit.push(RBP)
        .mov(RBP, RSP)
        .push(STACKMAP_PTR);

    if frame_slots != 0 {
        emit.add(RSP, -8 * (frame_slots as i32));
    }
}

fn emit_epilogue(emit: &mut Emit, want_ret: bool) {
    emit.mov(STACKMAP_PTR, &saved_stackmap())
        .mov(RSP, RBP)
        .pop(RBP);

    if want_ret {
        emit.ret();
    }
}

fn new_stackmap_with_frame_descr(frame: &FrameDescr) -> StackMap {
    let mut m = StackMap::new();

    // XXX: Check for gcptr-ness when FrameDescr has got that.
    for _ in 0..frame.slot_count() {
        m.push_gcptr();
    }

    m
}

extern "C" fn display_isize(i: isize) {
    println!("display_isize: {}", i);
}

extern "C" fn panic_inline_sym(w: usize, universe: &Universe) {
    // Unwind the stack.
    // universe.

    panic!("panic_inline_sym: {}", InlineSym::from_word(w).as_str());
}

// Caller saved regs.
const TMP: R64 = R10;

// Callee saved regs.
const ALLOC_PTR: R64 = R12;
const UNIVERSE_PTR: R64 = R13;
const STACKMAP_PTR: R64 = R14;

const OPTIMIZE_IF_CMP: bool = true;
