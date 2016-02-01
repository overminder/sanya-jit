use super::shared::{Reloc, RelocTable, InfoRefs};
use super::compiled_rt::*;
use ast::nir::*;
use ast::id::*;
use ast::nir::RawNode::*;
use ast::nir::AllocNode::*;
use rt::*;
use rt::oop::*;
use rt::stackmap::*;

use assembler::x64::*;
use assembler::x64::R64::*;
use assembler::emit::{Emit, Label};

use std::collections::HashMap;
use std::mem::{transmute, size_of};

pub struct CompiledModule {
    pub emit: Emit,
    pub smt: StackMapTable,
    pub functions: HashMap<Id, CompiledFunction>,
}

pub struct CompiledFunction {
    pub entry_offset: usize,
    pub end_offset: usize,
    pub relocs: RelocTable,
    pub inforefs: InfoRefs,
}

pub struct ModuleCompiler<'a> {
    function_labels: LabelMap,
    smt: StackMapTable,
    emit: Emit,
    scdefns: ScDefns<'a>,
    compiled_functions: HashMap<Id, CompiledFunction>,
}

type ScDefns<'a> = HashMap<Id, &'a ScDefn>;

impl<'a> ModuleCompiler<'a> {
    pub fn new() -> Self {
        ModuleCompiler {
            function_labels: HashMap::new(),
            compiled_functions: HashMap::new(),
            smt: StackMapTable::new(),
            emit: Emit::new(),
            scdefns: Default::default(),
        }
    }

    pub fn add_sc(&mut self, sc: &'a ScDefn) {
        self.scdefns.insert(sc.name(), sc);
    }

    pub fn compile_all(&mut self, u: &Universe) {
        for sc in self.scdefns.values() {
            let func = compile_function(&mut self.emit,
                                        sc,
                                        &self.scdefns,
                                        &mut self.function_labels,
                                        &mut self.smt,
                                        u);
            self.compiled_functions.insert(sc.name(), func);
        }
    }

    pub fn into_compiled_module(self) -> CompiledModule {
        CompiledModule {
            emit: self.emit,
            smt: self.smt,
            functions: self.compiled_functions,
        }
    }
}

type LabelMap = HashMap<Id, Label>;

fn compile_function(emit: &mut Emit,
                    scdefn: &ScDefn,
                    scs: &ScDefns,
                    labels: &mut LabelMap,
                    smt: &mut StackMapTable,
                    u: &Universe)
                    -> CompiledFunction {
    // Align the function entry.
    // XXX: I'm not sure if this is correct - need to read the AMD64 ABI
    // doc for the correct alignment.
    emit_nop_until_aligned(emit, 0x10);

    // Make space for infotable.
    let info = InfoTable::<Closure>::new(scdefn.frame_descr().upval_refs().len() as u16,
                                         0,
                                         scdefn.arity() as u16,
                                         OopKind::Callable,
                                         &scdefn.name().to_string());
    unsafe {
        emit.alloc(info);
    }

    let entry_offset = {
        let label = find_or_make_label(labels, scdefn.name());
        emit.bind(label);
        label.offset().unwrap()
    };
    let (stackmap, bare_entry) = emit_prologue(emit, scdefn.frame_descr());
    let mut inforefs = Default::default();
    let mut relocs = vec![];
    {
        let mut nodecc = NodeCompiler {
            emit: emit,
            universe: u,
            smt: smt,
            relocs: &mut relocs,
            inforefs: &mut inforefs,
            labels: labels,
            sc_name: scdefn.name(),
            entry_offset: entry_offset,
            bare_entry_offset: bare_entry,
            scs: scs,
        };
        nodecc.compile(scdefn.body(), stackmap).unwrap();
    }

    emit_epilogue(emit, true);

    CompiledFunction {
        entry_offset: entry_offset,
        end_offset: emit.here(),
        relocs: relocs,
        inforefs: inforefs,
    }
}

fn find_or_make_label<'a>(m: &'a mut LabelMap, name: Id) -> &'a mut Label {
    use std::collections::hash_map::Entry::*;

    match m.entry(name) {
        Vacant(v) => v.insert(Label::new()),
        Occupied(o) => o.into_mut(),
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum CallingConv {
    Internal,
    SyncUniverse,
}

struct NodeCompiler<'a> {
    emit: &'a mut Emit,
    universe: &'a Universe,
    smt: &'a mut StackMapTable,
    relocs: &'a mut RelocTable,
    inforefs: &'a mut InfoRefs,
    labels: &'a mut LabelMap,
    sc_name: Id,
    entry_offset: usize,
    bare_entry_offset: usize,
    scs: &'a ScDefns<'a>,
}

type CgResult<A> = Result<A, String>;

impl<'a> NodeCompiler<'a> {
    // Might clobber anything except %rax and the arg registers.
    // `f` should really only just do the call.
    fn calling_out<F: FnMut(&mut Emit)>(&mut self,
                                        mut stackmap: StackMap,
                                        cconv: CallingConv,
                                        mut f: F) {
        // Ensure the stack is 16-byte aligned after the call.
        // Keep this in sync with the actual stack length.
        let sp_changed = if stackmap.len() & 1 == 1 {
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
                .mov(TMP, &universe_invocation_chain())
                .lea(TMP2, &mut label_ret_addr)
                .mov(&(TMP + OFFSET_OF_ICHAIN_TOP_RIP), TMP2)
                .mov(&(TMP + OFFSET_OF_ICHAIN_TOP_RBP), RBP);
        }

        // Do the actuall call.
        f(self.emit);
        self.emit.bind(&mut label_ret_addr);

        // And record the stackmap at this place.
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
        let offset = self.emit.here() - 8 - self.entry_offset;
        self.relocs.push((offset, value))
    }

    fn load_info(&mut self, dst: R64, name: Id) {
        self.emit.lea(dst, find_or_make_label(self.labels, name));
        let offset = self.emit.here() - 4 - self.entry_offset;
        self.inforefs.push(offset);
        debug!("load_info: {}[{:#x}] -> {}", self.sc_name, offset, name);
    }

    fn compile_alloc_node(&mut self, node: &AllocNode, stackmap: StackMap) -> CgResult<()> {
        match node {
            &MkFixnum(i) => {
                self.load_reloc(RAX, Reloc::Fixnum(i as i64));
                // self.emit_fixnum_allocation(stackmap);
                // self.emit
                //    .mov(TMP, i as i64)
                //    .mov(&(RAX + 8), TMP);
            }
            &MkPair(ref car, ref cdr) => {
                let mut map0 = stackmap;
                self.compile(cdr, map0);
                self.push_oop(RAX, &mut map0);
                self.compile(car, map0);
                self.emit.mov(TMP, RAX);
                self.emit_allocation(map0,
                                     self.universe.pair_info.sizeof_instance() as i32,
                                     &[(true, TMP)]);
                self.emit
                    .mov(&(RAX + 8), TMP)
                    .mov(TMP, self.universe.pair_info.entry_word() as i64)
                    .mov(&closure_info(RAX), TMP)
                    .pop(TMP)
                    .mov(&(RAX + 16), TMP);
            }
            &MkOopArray(ref len, ref fill) => {
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
            &MkI64Array(ref len, ref fill) => {
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
            &MkClosure(closure_id) => {
                let sc = self.scs[&closure_id];
                let mut map0 = stackmap;

                let npayloads = sc.frame_descr().upval_refs().len();

                if npayloads < 6 && OPTIMIZE_SMALL_MKCLOSURE {
                    let mut closure_ptr_loaded = false;
                    // XXX: Keep the size calculation in sync with rt::oop.
                    self.emit_allocation(stackmap, ((1 + npayloads) * 8) as i32, &[]);
                    self.load_info(TMP, closure_id);
                    self.emit.mov(&closure_info(RAX), TMP);
                    for (to_ix, slot) in sc.frame_descr().upval_refs().iter().enumerate() {
                        match slot {
                            &Slot::UpVal(from_ix) => {
                                if !closure_ptr_loaded {
                                    self.emit.mov(TMP2, &closure_ptr());
                                    closure_ptr_loaded = true;
                                }
                                self.emit.mov(TMP, &upval_slot(TMP2, from_ix));
                            }
                            &Slot::Local(from_ix) => {
                                self.emit.mov(TMP, &frame_slot(from_ix));
                            }
                            _ => panic!("NMkClosure: upval_refs contain {:?}", slot),
                        }
                        self.emit.mov(&upval_slot(RAX, to_ix), TMP);
                    }

                    return Ok(());
                }

                warn!("Closure {} too large ({} payloads).",
                      self.sc_name,
                      npayloads);

                // Preparing the payloads.
                for slot in sc.frame_descr().upval_refs().iter().rev() {
                    match slot {
                        &Slot::UpVal(ix) => {
                            self.emit.mov(RAX, &closure_ptr());
                            self.push_oop(&upval_slot(RAX, ix), &mut map0);
                        }
                        &Slot::Local(ix) => {
                            self.push_oop(&frame_slot(ix), &mut map0);
                        }
                        _ => panic!("NMkClosure: upval_refs contain {:?}", slot),
                    }
                }

                // Calling out to alloc the closure.
                self.emit.mov(RDI, UNIVERSE_PTR);
                self.load_info(RSI, closure_id);
                self.emit.mov(RDX, RSP);

                self.calling_out(map0, CallingConv::SyncUniverse, |emit| {
                    emit.mov(RAX, unsafe { transmute::<_, i64>(alloc_closure) })
                        .call(RAX);
                });

                // Restore the stack ptr.
                self.emit.add(RSP, 8 * npayloads as i32);
            }
            &MkBox(ref n) => {
                try!(self.compile(n, stackmap));
                self.emit.mov(TMP, RAX);
                self.emit_allocation(stackmap,
                                     self.universe.box_info.sizeof_instance() as i32,
                                     &[(true, TMP)]);
                self.emit
                    .mov(&(RAX + 8), TMP)
                    .mov(TMP, self.universe.box_info.entry_word() as i64)
                    .mov(&closure_info(RAX), TMP);
            }
        }
        Ok(())
    }

    fn compile(&mut self, node: &RawNode, stackmap: StackMap) -> CgResult<()> {
        match node {
            &NAlloc(ref alloc) => {
                try!(self.compile_alloc_node(alloc, stackmap));
            }
            &NReadBox(ref n) => {
                try!(self.compile(n, stackmap));
                self.emit.mov(RAX, &(RAX + 8));
            }
            &NWriteBox(ref n, ref v) => {
                let mut map0 = stackmap;
                try!(self.compile(v, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(n, map0));
                self.emit
                    .pop(TMP)
                    .mov(&(RAX + 8), TMP);
            }
            &NCall { ref func, ref args, is_tail } => {
                let mut precall_map = stackmap;
                // Eval args in the reverse order
                for arg in args.iter().rev() {
                    try!(self.compile(arg, precall_map));
                    self.push_oop(RAX, &mut precall_map);
                }
                // Eval func
                try!(self.compile(func, precall_map));
                self.emit.mov(CLOSURE_PTR, RAX);

                for (r, _) in ARG_REGS.iter().zip(args.iter()) {
                    self.emit.pop(*r);
                }
                if is_tail {
                    if let &NReadSlot(Slot::Global(name)) = func.as_ref() {
                        // XXX: Hmm... Is this good?
                        if name == self.sc_name && OPTIMIZE_KNOWN_SELF_CALL {
                            self.emit.jmp(&mut Label::from_offset(self.bare_entry_offset));
                            return Ok(());
                        }
                    }
                    emit_epilogue(self.emit, false);
                    self.emit.jmp(&closure_info(CLOSURE_PTR));
                } else {
                    self.calling_out(stackmap, CallingConv::Internal, |emit| {
                        emit.call(&closure_info(CLOSURE_PTR));
                    });
                }
            }
            &NIf { ref cond, ref on_true, ref on_false } => {
                let mut label_false = Label::new();
                let mut label_done = Label::new();

                let special_case = if OPTIMIZE_IF_CMP {
                    try!(self.emit_optimized_if_cmp(cond.as_ref(), &mut label_false, stackmap))
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
            &NSeq(ref body, ref last) => {
                for n in body {
                    try!(self.compile(n, stackmap));
                }
                try!(self.compile(last, stackmap));
            }
            &NReadArgument(arg_ix) => {
                self.emit.mov(RAX, ARG_REGS[arg_ix]);
            }
            &NReadSlot(Slot::Global(name)) => {
                self.load_reloc(RAX, Reloc::Global(name.to_owned()));
                // self.emit.lea(RAX, self.labels.get_mut(name).unwrap());
            }
            &NReadSlot(Slot::Local(ix)) => {
                self.emit.mov(RAX, &frame_slot(ix));
            }
            &NReadSlot(Slot::UpVal(ix)) => {
                self.emit.mov(RAX, &closure_ptr());
                self.emit.mov(RAX, &upval_slot(RAX, ix));
            }
            &NBindLocal(ref bs, ref n) => {
                let mut map0 = stackmap;
                for &(ix, ref bn) in bs {
                    try!(self.compile(bn, map0));
                    self.emit.mov(&frame_slot(ix), RAX);
                    map0.set_local_slot(ix, true);
                }
                self.compile(n, map0);
            }
            &NReadArrayLength(ref arr) => {
                let mut map0 = stackmap;
                try!(self.compile(arr, map0));
                self.emit.mov(TMP, &(RAX + 8));
                self.emit_fixnum_allocation(map0, TMP);
            }
            &NReadOopArray(ref arr, ref ix) => {
                let mut map0 = stackmap;
                try!(self.compile(ix, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(arr, map0));
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .mov(RAX, &(RAX + TMP * 8 + 16));  // array indexing
            }
            &NReadI64Array(ref arr, ref ix) => {
                let mut map0 = stackmap;
                try!(self.compile(ix, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(arr, map0));
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .mov(RAX, &(RAX + TMP * 8 + 16));  // array indexing
            }
            &NWriteOopArray(ref arr, ref ix, ref val) => {
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
            &NPrimFF(op, ref n1, ref n2) => {
                let mut map0 = stackmap;
                try!(self.compile(n2, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(n1, map0));

                // Unbox the lhs to RAX and pop the rhs to TMP.
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
                        self.emit.cmp(RAX, &(TMP + 8));
                        self.load_reloc(TMP, Reloc::Fixnum(1));
                        self.load_reloc(RAX, Reloc::Fixnum(0));
                        self.emit.cmovcc(op_to_cond(op), RAX, TMP);
                        return Ok(());
                    }
                }
                // Allocate a new fixnum to store the result.
                self.emit.mov(TMP, RAX);
                self.emit_fixnum_allocation(map0, TMP);
            }
            &NPrimO(ref op, ref n1) => {
                try!(self.compile(n1, stackmap));
                match *op {
                    PrimOpO::Display => {
                        self.emit
                            .mov(RDI, RAX)
                            .mov(RSI, UNIVERSE_PTR);
                        // Safe to use conv::internal since we don't alloc in display.
                        self.calling_out(stackmap, CallingConv::Internal, |emit| {
                            emit.mov(RAX, unsafe { transmute::<_, i64>(display_oop) })
                                .call(RAX);
                        });
                    }
                    PrimOpO::Panic => {
                        self.emit
                            .mov(RDI, UNIVERSE_PTR);
                        // Safe to use conv::internal since we don't alloc in display.
                        self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                            emit.mov(RAX, unsafe { transmute::<_, i64>(panic) })
                                .call(RAX);
                        });
                    }
                    PrimOpO::Fixnump => {
                        self.emit
                            .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
                            .cmp(TMP, &Addr::B(RAX));

                        self.load_reloc(TMP, Reloc::Fixnum(1));
                        self.load_reloc(RAX, Reloc::Fixnum(0));

                        self.emit.cmove(RAX, TMP);
                    }
                    PrimOpO::Eval => {
                        self.emit
                            .mov(RDI, RAX)
                            .mov(RSI, UNIVERSE_PTR);

                        // XXX
                        panic!();
                    }
                }
            }
            _ => panic!("Unimplemented node: {:?}", node),
        };

        Ok(())
    }

    // To RAX.
    fn emit_allocation(&mut self, stackmap: StackMap, alloc_size: i32, tmp_regs: &[(bool, R64)]) {
        let mut label_alloc_success = Label::new();
        // Check heap overflow.
        self.emit
            .mov(RAX, ALLOC_PTR)
            .add(ALLOC_PTR, alloc_size)
            .cmp(ALLOC_PTR, &universe_alloc_limit())
            .jle(&mut label_alloc_success);

        let mut map0 = stackmap;
        for &(is_oop, r) in tmp_regs {
            // Save regs.
            if is_oop {
                self.push_oop(r, &mut map0);
            } else {
                self.push_word(r, &mut map0);
            }
        }
        // Slow case: sync the runtime state and call out for GC.
        self.emit
            .mov(ALLOC_PTR, RAX)
            .mov(RDI, UNIVERSE_PTR)
            .mov(RSI, alloc_size as i64);
        self.calling_out(map0, CallingConv::SyncUniverse, |emit| {
            emit.mov(RAX, unsafe { transmute::<_, i64>(full_gc) })
                .call(RAX);
        });
        for &(_, r) in tmp_regs.iter().rev() {
            // Restore regs.
            self.emit.pop(r);
        }
        // Fast case: we are done.
        self.emit.bind(&mut label_alloc_success);
    }

    // XXX: Currently all the allocation routines are inlined. Is this good?
    fn emit_fixnum_allocation(&mut self, mut stackmap: StackMap, value_reg: R64) {
        let alloc_size = self.universe.fixnum_info.sizeof_instance() as i32;
        if OPTIMIZE_FIXNUM_ALLOC_FAST_PATH {
            self.emit_allocation(stackmap, alloc_size, &[(false, value_reg)]);
        } else {
            self.push_word(value_reg, &mut stackmap);
            self.emit_allocation(stackmap, alloc_size, &[]);
            self.emit.pop(value_reg);
        }
        self.emit
            .mov(&(RAX + 8), value_reg)
            .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
            .mov(&Addr::B(RAX), TMP);
    }

    // Try to select some better instructions.
    // XXX: Unify jcc and cmovcc generations.
    fn emit_optimized_if_cmp(&mut self,
                             node: &RawNode,
                             label_false: &mut Label,
                             mut map0: StackMap)
                             -> CgResult<bool> {
        Ok(match node {
            &NPrimFF(op, ref lhs, ref rhs) if op_is_cond(op) => {
                try!(self.compile(rhs, map0));
                self.push_oop(RAX, &mut map0);
                try!(self.compile(lhs, map0));
                self.emit
                    .mov(RAX, &(RAX + 8))
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .cmp(RAX, TMP)
                    .jcc(op_to_cond(op).inverse(), label_false);
                true
            }
            &NPrimO(PrimOpO::Fixnump, ref rand) => {
                try!(self.compile(rand, map0));
                self.emit
                    .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
                    .cmp(TMP, &Addr::B(RAX))
                    .jne(label_false);
                true
            }
            _ => false,
        })
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

fn universe_invocation_chain() -> Addr {
    Addr::BD(UNIVERSE_PTR, OFFSET_OF_UNIVERSE_INVOCATION_CHAIN)
}

fn frame_slot(ix: usize) -> Addr {
    Addr::BD(RBP, -8 * ((1 + EXTRA_CALLER_SAVED_FRAME_SLOTS + ix) as i32))
}

fn upval_slot(closure_ptr: R64, ix: usize) -> Addr {
    closure_ptr + (1 + ix as i32) * 8
}

fn closure_ptr() -> Addr {
    RBP + (-8)
}

fn closure_info(r: R64) -> Addr {
    Addr::B(r)
}

fn emit_prologue(emit: &mut Emit, frame_descr: &FrameDescr) -> (StackMap, usize) {
    emit.push(RBP)
        .mov(RBP, RSP)
        .push(CLOSURE_PTR);

    let frame_slots = frame_descr.local_slot_count();
    if frame_slots != 0 {
        emit.add(RSP, -8 * (frame_slots as i32));
    }

    let bare_entry = emit.here();

    (new_stackmap_with_frame_descr(frame_descr), bare_entry)
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

    // For closure ptr
    m.push_gcptr();

    // Treat uninitialized local slots as non-oops.
    for _ in 0..frame.local_slot_count() {
        m.push_word();
    }

    m
}

pub fn make_rust_entry(emit: &mut Emit) -> (usize, usize) {
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
    emit.mov(UNIVERSE_PTR, RSI)
        .mov(ALLOC_PTR, &universe_alloc_ptr());

    // Make space for the base rbp list node and
    // add the current rbp to the base rbp list.
    emit.sub(RSP, size_of::<NativeInvocationChain>() as i32);
    emit.mov(TMP, &universe_invocation_chain())
        .mov(&(RSP + OFFSET_OF_ICHAIN_NEXT), TMP)
        .mov(&(RSP + OFFSET_OF_ICHAIN_BASE_RBP), RBP)
        .mov(TMP, 0_i64)
        .mov(&(RSP + OFFSET_OF_ICHAIN_TOP_RBP), TMP)
        .mov(&(RSP + OFFSET_OF_ICHAIN_TOP_RIP), TMP)
        .mov(&universe_invocation_chain(), RSP);

    // Enter the real main.
    emit.call(&Addr::B(CLOSURE_PTR));

    // Sync back the regs to the runtime state,
    // And pop the ichain.
    emit.mov(&universe_alloc_ptr(), ALLOC_PTR)
        .mov(TMP, &universe_invocation_chain())
        .mov(TMP, &(TMP + OFFSET_OF_ICHAIN_NEXT))
        .mov(&universe_invocation_chain(), TMP);

    // Dealloc the rbp list node.
    emit.add(RSP, size_of::<NativeInvocationChain>() as i32);

    // Restore the saved regs.
    emit.pop(UNIVERSE_PTR)
        .pop(ALLOC_PTR);

    // Standard epilogue.
    emit.mov(RSP, RBP)
        .pop(RBP)
        .ret();

    // 2. Get it.
    let entry_offset = entry.offset().unwrap();
    (entry_offset, emit.here())
}

const CLOSURE_PTR: R64 = RDI;
const ARG_REGS: [R64; 5] = [RSI, RDX, RCX, R8, R9];

// Caller saved regs.
const TMP: R64 = R10;
const TMP2: R64 = R11;

// Callee saved regs.
const ALLOC_PTR: R64 = R12;
const UNIVERSE_PTR: R64 = R13;

const OPTIMIZE_IF_CMP: bool = true;
const OPTIMIZE_KNOWN_SELF_CALL: bool = true;
const OPTIMIZE_SMALL_MKCLOSURE: bool = true;
const OPTIMIZE_FIXNUM_ALLOC_FAST_PATH: bool = true;
