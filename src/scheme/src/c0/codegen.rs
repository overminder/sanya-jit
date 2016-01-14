use super::shared::{Reloc, RelocTable};
use super::compiled_rt::*;
use ast::nir::*;
use ast::nir::RawNode::*;
use rt::*;
use rt::oop::*;
use rt::stackmap::{StackMap, StackMapTable, EXTRA_CALLER_SAVED_FRAME_SLOTS};

use assembler::x64::*;
use assembler::x64::R64::*;
use assembler::emit::{Emit, Label};

use std::collections::HashMap;
use std::mem::transmute;

pub struct CompiledModule {
    pub emit: Emit,
    pub smt: StackMapTable,
    pub functions: HashMap<String, CompiledFunction>,
}

pub struct CompiledFunction {
    pub entry_offset: usize,
    pub relocs: RelocTable,
}

pub struct ModuleCompiler {
    function_labels: LabelMap,
    smt: StackMapTable,
    emit: Emit,
    compiled_functions: HashMap<String, CompiledFunction>,
}

impl ModuleCompiler {
    pub fn new() -> Self {
        ModuleCompiler {
            function_labels: HashMap::new(),
            compiled_functions: HashMap::new(),
            smt: StackMapTable::new(),
            emit: Emit::new(),
        }
    }

    pub fn compile_scdefn(&mut self, sc: &mut ScDefn, u: &Universe) {
        let func = compile_function(&mut self.emit,
                                    sc,
                                    &mut self.function_labels,
                                    &mut self.smt,
                                    u);

        self.compiled_functions.insert(sc.name().to_owned(), func);
    }

    pub fn into_compiled_module(self) -> CompiledModule {
        CompiledModule {
            emit: self.emit,
            smt: self.smt,
            functions: self.compiled_functions,
        }
    }
}

type LabelMap = HashMap<String, Label>;

fn compile_function(emit: &mut Emit,
                    scdefn: &mut ScDefn,
                    labels: &mut LabelMap,
                    smt: &mut StackMapTable,
                    u: &Universe)
                    -> CompiledFunction {
    // Align the function entry.
    // XXX: I'm not sure if this is correct - need to read the AMD64 ABI
    // doc for the correct alignment.
    emit_nop_until_aligned(emit, 0x10);

    // Make space for infotable.
    let info = InfoTable::<Closure>::new(0,
                                         0,
                                         scdefn.arity() as u16,
                                         OopKind::Callable,
                                         "toplevel-closure(XXX)");
    unsafe {
        emit.alloc(info);
    }

    let entry_offset = {
        let label = find_or_make_label(labels, scdefn.name());
        emit.bind(label);
        label.offset().unwrap()
    };
    // println!("Function {}'s offset is {}",
    //         self.scdefn.name(),
    //         labels.get(self.scdefn.name()).unwrap().offset().unwrap());
    emit_prologue(emit, scdefn.frame_descr().slot_count());

    let mut relocs = vec![];

    let stackmap = new_stackmap_with_frame_descr(scdefn.frame_descr());
    {
        let mut nodecc = NodeCompiler::new(emit, labels, smt, u, &mut relocs);
        nodecc.compile(scdefn.body_mut(), stackmap).unwrap();
    }

    emit_epilogue(emit, true);

    CompiledFunction {
        entry_offset: entry_offset,
        relocs: relocs,
    }
}

fn find_or_make_label<'a>(m: &'a mut LabelMap, name: &str) -> &'a mut Label {
    use std::collections::hash_map::Entry::*;

    // XXX: HashMap.entry requires the key to be copied.  Hmm...
    match m.entry(name.to_owned()) {
        Vacant(v) => v.insert(Label::new()),
        Occupied(o) => o.into_mut(),
    }
}

struct NodeCompiler<'a> {
    emit: &'a mut Emit,
    labels: &'a mut LabelMap,
    universe: &'a Universe,
    smt: &'a mut StackMapTable,
    relocs: &'a mut RelocTable,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum CallingConv {
    Internal,
    SyncUniverse,
}

impl<'a> NodeCompiler<'a> {
    fn new(emit: &'a mut Emit,
           labels: &'a mut LabelMap,
           smt: &'a mut StackMapTable,
           universe: &'a Universe,
           relocs: &'a mut RelocTable)
           -> Self {
        NodeCompiler {
            emit: emit,
            labels: labels,
            universe: universe,
            smt: smt,
            relocs: relocs,
        }
    }

    // Might clobber anything except %rax and the arg registers.
    // `f` should really only just do the call.
    fn calling_out<F: FnMut(&mut Emit)>(&mut self,
                                        mut stackmap: StackMap,
                                        cconv: CallingConv,
                                        mut f: F) {
        // Ensure the stack is 16-byte aligned after the call.
        let sp_changed = if (stackmap.len() + EXTRA_CALLER_SAVED_FRAME_SLOTS) & 1 == 1 {
            self.emit.add(RSP, -8);
            stackmap.push_word();
            true
        } else {
            false
        };

        let mut label_ret_addr = Label::new();

        if cconv == CallingConv::SyncUniverse {
            // Sync the runtime regs with the universe.
            self.emit
                .mov(&universe_alloc_ptr(), ALLOC_PTR)
                .mov(&universe_saved_rbp(), RBP)
                .lea(TMP, &mut label_ret_addr)
                .mov(&universe_saved_rip(), TMP);
        }

        // Do the actuall call.
        f(self.emit);
        self.emit.bind(&mut label_ret_addr);

        self.smt.insert(label_ret_addr.offset().unwrap(), stackmap);

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

    fn push_oop<Op>(&mut self, src: Op, stackmap: &mut StackMap)
        where Emit: EmitPush<Op>
    {
        self.emit.push(src);
        stackmap.push_gcptr();
    }

    fn push_word<Op>(&mut self, src: Op, stackmap: &mut StackMap)
        where Emit: EmitPush<Op>
    {
        self.emit.push(src);
        stackmap.push_word();
    }

    fn load_reloc(&mut self, dst: R64, value: Reloc) {
        self.emit.mov(dst, 0_i64);
        let offset = self.emit.here() - 8;
        self.relocs.push((offset, value))
    }

    fn compile(&mut self, node: &mut RawNode, stackmap: StackMap) -> Result<(), String> {
        match node {
            &mut NMkFixnum(i) => {
                self.load_reloc(RAX, Reloc::Fixnum(i as i64));
                // self.emit_fixnum_allocation(stackmap);
                // self.emit
                //    .mov(TMP, i as i64)
                //    .mov(&(RAX + 8), TMP);
            }
            &mut NMkOopArray(ref mut len, ref mut fill) => {
                let mut precall_map = stackmap;
                try!(self.compile(fill, precall_map));
                self.push_oop(RAX, &mut precall_map);
                try!(self.compile(len, precall_map));
                self.emit
                    .mov(RDI, UNIVERSE_PTR)
                    .mov(RSI, RAX)
                    .pop(RDX);
                self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                    emit.mov(RAX, unsafe { transmute::<_, i64>(alloc_ooparray) })
                        .call(RAX);
                });
            }
            &mut NMkI64Array(ref mut len, ref mut fill) => {
                let mut precall_map = stackmap;
                try!(self.compile(fill, precall_map));
                // unbox and push `fill`
                self.push_word(&(RAX + 8), &mut precall_map);
                try!(self.compile(len, precall_map));
                // RSI: unboxed `len`
                // RDX: `fill`
                self.emit
                    .mov(RDI, UNIVERSE_PTR)
                    .mov(RSI, &(RAX + 8))
                    .pop(RDX);
                self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                    emit.mov(RAX, unsafe { transmute::<_, i64>(alloc_i64array) })
                        .call(RAX);
                });
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

                let special_case = if let &mut NPrimFF(op, ref mut lhs, ref mut rhs) =
                                          cond.as_mut() {
                    if op_is_cond(op) && OPTIMIZE_IF_CMP {
                        let mut map0 = stackmap;
                        try!(self.compile(rhs, map0));
                        self.push_oop(RAX, &mut map0);
                        try!(self.compile(lhs, map0));
                        self.emit
                            .mov(RAX, &(RAX + 8))
                            .pop(TMP)
                            .mov(TMP, &(TMP + 8))
                            .cmp(RAX, TMP)
                            .jcc(op_to_cond(op).inverse(), &mut label_false);

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
                        .mov(RAX, &(RAX + 8))
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
                self.load_reloc(RAX, Reloc::Global(name.to_owned()));
                // self.emit.lea(RAX, self.labels.get_mut(name).unwrap());
            }
            &mut NReadLocal(ix) => {
                self.emit.mov(RAX, &frame_slot(ix));
            }
            &mut NWriteLocal(ix, ref mut n) => {
                try!(self.compile(n, stackmap));
                self.emit.mov(&frame_slot(ix), RAX);
            }
            &mut NReadArrayLength(ref mut arr) => {
                let mut map0 = stackmap;
                try!(self.compile(arr, map0));
                self.emit.mov(RAX, &(RAX + 8));
                self.push_word(RAX, &mut map0);
                self.emit_fixnum_allocation(map0);
                self.emit
                    .pop(TMP)
                    .mov(&(RAX + 8), TMP);
            }
            &mut NReadOopArray(ref mut arr, ref mut ix) => {
                let mut map0 = stackmap;
                try!(self.compile(ix, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(arr, map0));
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .mov(RAX, &(RAX + TMP * 8 + 16));  // array indexing
            }
            &mut NReadI64Array(ref mut arr, ref mut ix) => {
                let mut map0 = stackmap;
                try!(self.compile(ix, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(arr, map0));
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .mov(RAX, &(RAX + TMP * 8 + 16));  // array indexing
            }
            &mut NWriteOopArray(ref mut arr, ref mut ix, ref mut val) => {
                let mut map0 = stackmap;
                try!(self.compile(val, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(ix, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(arr, map0));
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .lea(TMP, &(RAX + TMP * 8 + 16))
                    .pop(RAX)
                    .mov(&Addr::B(TMP), RAX);
            }
            &mut NPrimFF(op, ref mut n1, ref mut n2) => {
                let mut map0 = stackmap;
                try!(self.compile(n2, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(n1, map0));

                // Unbox the lhs and pop the rhs.
                self.emit
                    .mov(RAX, &(RAX + 8))
                    .pop(TMP);
                map0.pop();

                // Save the unboxed result to %rax
                match op {
                    PrimOpFF::Add => {
                        self.emit
                            .add(RAX, &(TMP + 8));
                    }
                    PrimOpFF::Sub => {
                        self.emit
                            .sub(RAX, &(TMP + 8));
                    }
                    PrimOpFF::Lt | PrimOpFF::Eq => {
                        self.emit
                            .cmp(RAX, &(TMP + 8))
                            .mov(TMP, 1)
                            .mov(RAX, 0)
                            .cmovcc(op_to_cond(op), RAX, TMP);
                    }

                }
                // Allocate a new fixnum to store the result.
                self.push_word(RAX, &mut map0);
                self.emit_fixnum_allocation(map0);
                self.emit
                    .pop(TMP)
                    .mov(&(RAX + 8), TMP);
            }
            &mut NPrimO(ref op, ref mut n1) => {
                try!(self.compile(n1, stackmap));
                match *op {
                    PrimOpO::Display => {
                        self.emit
                            .mov(RDI, RAX)
                            .mov(RSI, UNIVERSE_PTR);
                        self.calling_out(stackmap, CallingConv::Internal, |emit| {
                            emit.mov(RAX, unsafe { transmute::<_, i64>(display_oop) })
                                .call(RAX);
                        });
                    }
                    PrimOpO::PanicInlineSym => {
                        self.emit
                            .mov(RDI, RAX)
                            .mov(RSI, UNIVERSE_PTR);
                        self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                            emit.mov(RAX, unsafe { transmute::<_, i64>(panic_inline_sym) })
                                .call(RAX);
                        });
                    }
                    PrimOpO::Fixnump => {
                        self.emit
                            .mov(RAX, &Addr::B(RAX))
                            .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
                            .cmp(RAX, TMP)
                            .mov(TMP, 1)
                            .mov(RAX, 0)
                            .cmove(RAX, TMP);

                        let mut map0 = stackmap;
                        self.push_word(RAX, &mut map0);
                        self.emit_fixnum_allocation(map0);
                        self.emit
                            .pop(TMP)
                            .mov(&(RAX + 8), TMP);
                    }
                }
            }
            _ => panic!("Unimplemented node: {:?}", node),
        };

        Ok(())
    }

    // XXX: Currently all the allocation routines are inlined. Is this good?
    fn emit_fixnum_allocation(&mut self, stackmap: StackMap) {
        let mut label_alloc_success = Label::new();
        let alloc_size = self.universe.fixnum_info.sizeof_instance() as i32;

        // Check heap overflow.
        self.emit
            .mov(RAX, ALLOC_PTR)
            .add(ALLOC_PTR, alloc_size)
            .cmp(ALLOC_PTR, &universe_alloc_limit())
            .jle(&mut label_alloc_success);

        // Slow case: sync the runtime state and call out for GC.
        self.emit
            .sub(ALLOC_PTR, alloc_size)
            .mov(RDI, UNIVERSE_PTR)
            .mov(RSI, alloc_size as i64);
        self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
            emit.mov(RAX, unsafe { transmute::<_, i64>(full_gc) })
                .call(RAX);
        });
        // Falls through.

        // Fast case: we are done.
        self.emit
            .bind(&mut label_alloc_success)
            .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
            .mov(&Addr::B(RAX), TMP);
    }
}

// Instruction selection misc.

fn op_is_cond(op: PrimOpFF) -> bool {
    match op {
        PrimOpFF::Lt | PrimOpFF::Eq => true,
        _ => false,
    }
}

fn op_to_cond(op: PrimOpFF) -> Cond {
    match op {
        PrimOpFF::Lt => Cond::L,
        PrimOpFF::Eq => Cond::E,
        _ => panic!("Not a conditional op: {:?}", op),
    }
}

// Layout misc.

fn universe_alloc_ptr() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_ALLOC_PTR)
}

fn universe_alloc_limit() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_ALLOC_LIMIT)
}

fn universe_saved_rip() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_SAVED_RIP)
}

fn universe_saved_rbp() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_SAVED_RBP)
}

fn universe_base_rbp() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_BASE_RBP)
}

fn frame_slot(ix: usize) -> Addr {
    Addr::BD(RBP, -8 * ((1 + EXTRA_CALLER_SAVED_FRAME_SLOTS + ix) as i32))
}

fn emit_prologue(emit: &mut Emit, frame_slots: usize) {
    emit.push(RBP)
        .mov(RBP, RSP);

    if frame_slots != 0 {
        emit.add(RSP, -8 * (frame_slots as i32));
    }
}

fn emit_epilogue(emit: &mut Emit, want_ret: bool) {
    emit.mov(RSP, RBP)
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

fn make_rust_entry(emit: &mut Emit, main_label: &mut Label) -> usize {
    // 1. Build up an entry.
    let mut entry = Label::new();
    emit_nop_until_aligned(emit, 0x10);

    // Standard prologue.
    emit.bind(&mut entry)
        .push(RBP)
        .mov(RBP, RSP);

    // Save the callee-saved regs clobbered by our runtime.
    emit.push(ALLOC_PTR)
        .push(UNIVERSE_PTR);

    // Get the relevant regs from/to the runtime state.
    emit.mov(UNIVERSE_PTR, RDI)
        .mov(&universe_base_rbp(), RBP)
        .mov(ALLOC_PTR, &universe_alloc_ptr());

    // Enter the real main.
    emit.call(main_label);

    // Sync back the regs to the runtime state.
    emit.mov(&universe_alloc_ptr(), ALLOC_PTR)
        .mov(TMP, 0)
        .mov(&universe_saved_rbp(), TMP);

    // Restore the saved regs.
    emit.pop(UNIVERSE_PTR)
        .pop(ALLOC_PTR);

    // Standard epilogue.
    emit.mov(RSP, RBP)
        .pop(RBP)
        .ret();

    // 2. Get it.
    entry.offset().unwrap()
}

// Caller saved regs.
const TMP: R64 = R10;

// Callee saved regs.
const ALLOC_PTR: R64 = R12;
const UNIVERSE_PTR: R64 = R13;

const OPTIMIZE_IF_CMP: bool = true;
