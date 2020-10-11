use super::shared::{Reloc, RelocTable, InfoRefs};
use super::compiled_rt::*;
use super::cgutil::*;
use ast::nir::*;
use ast::id::*;
use ast::nir::RawNode::*;
use ast::nir::AllocNode::*;
use ast::nir::LiteralNode::*;
use rt::*;
use rt::oop::*;
use rt::stackmap::*;

use assembler::x64::*;
use assembler::x64::R64::*;
use assembler::emit::{Emit, Label};

use std::collections::HashMap;
use std::mem::{size_of};

pub struct CompiledModule {
    pub emit: Emit,
    pub functions: HashMap<Id, CompiledFunction>,
}

pub struct CompiledFunction {
    pub entry_offset: usize,
    pub end_offset: usize,
    pub relocs: RelocTable,
    pub inforefs: InfoRefs,
    pub smo: OopStackMapOffsets,
}

pub struct ModuleCompiler<'a> {
    function_labels: LabelMap,
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
                                        u);
            self.compiled_functions.insert(sc.name(), func);
        }
    }

    pub fn into_compiled_module(self) -> CompiledModule {
        CompiledModule {
            emit: self.emit,
            functions: self.compiled_functions,
        }
    }
}

type LabelMap = HashMap<Id, Label>;

fn compile_function(emit: &mut Emit,
                    scdefn: &ScDefn,
                    scs: &ScDefns,
                    labels: &mut LabelMap,
                    u: &Universe)
                    -> CompiledFunction {
    // Align the function entry.
    // XXX: I'm not sure if this is correct - need to read the AMD64 ABI
    // doc for the correct alignment.
    emit_nop_until_aligned(emit, 0x10);

    // Make space for infotable.
    let info = InfoTable::<Closure>::new(
        scdefn.frame_descr().upval_refs().len() as u16,
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
    let mut smo = Default::default();
    let mut relocs = vec![];
    {
        let mut nodecc = NodeCompiler {
            emit: emit,
            universe: u,
            smo: &mut smo,
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
        smo: smo,
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
    smo: &'a mut OopStackMapOffsets,
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
            self.emit.add(rsp, -8);
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
                .mov(&(TMP + OFFSET_OF_ICHAIN_TOP_rip), TMP2)
                .mov(&(TMP + OFFSET_OF_ICHAIN_TOP_rbp), rbp);
        }

        // Do the actuall call.
        f(self.emit);
        self.emit.bind(&mut label_ret_addr);

        // And record the stackmap at this place.
        self.smo.insert(label_ret_addr.offset().unwrap() - self.entry_offset,
                        stackmap);

        if cconv == CallingConv::SyncUniverse {
            // Read back the runtime regs.
            self.emit
                .mov(ALLOC_PTR, &universe_alloc_ptr());
        }

        // And restore the stack.
        if sp_changed {
            self.emit.add(rsp, 8);
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

    fn sizeof_alloc(&self, node: &AllocNode) -> CgResult<usize> {
        Ok(match node {
            &MkPair(..) => self.universe.pair_info.sizeof_instance(),
            &MkOopArray(..) |
            &MkI64Array(..) => {
                return Err(format!("Not a sized allocation: {:?}", node));
            }
            &MkClosure(closure_id) => {
                let sc = self.scs[&closure_id];
                let npayloads = sc.frame_descr().upval_refs().len();
                (1 + npayloads) * 8
            }
            &MkBox(..) => 16,
        })
    }

    // XXX: What does this do exactly?
    fn compile_placement_init(&mut self, node: &AllocNode) -> CgResult<()> {
        Ok(match node {
            &MkClosure(closure_id) => {
                let sc = self.scs[&closure_id];
                let mut closure_ptr_loaded = false;
                // XXX: Keep the size calculation in sync with rt::oop.
                self.load_info(TMP, closure_id);
                self.emit.mov(&closure_info(rax), TMP);
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
                    self.emit.mov(&upval_slot(rax, to_ix), TMP);
                }
            }
            _ => return Err(format!("Not implemented: {:?}", node)),
        })
    }

    fn compile_literal_node(&mut self, node: &LiteralNode, _stackmap: StackMap) -> CgResult<()> {
        match node {
            &LitAny(ref e) => {
                self.load_reloc(rax, Reloc::Any(e.to_owned()));
            }
        }
        Ok(())
    }

    fn compile_alloc_node(&mut self, node: &AllocNode, stackmap: StackMap) -> CgResult<()> {
        match node {
            // &MkFixnum(i) => {
            // self.emit_fixnum_allocation(stackmap);
            // self.emit
            // .mov(TMP, i as i64)
            // .mov(&(rax + 8), TMP);
            // }
            //
            &MkPair(ref car, ref cdr) => {
                let mut map0 = stackmap;
                self.compile(cdr, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(car, map0)?;
                self.emit.mov(TMP, rax);
                self.emit_allocation(map0,
                                     self.universe.pair_info.sizeof_instance() as i32,
                                     &[(true, TMP)]);
                self.emit
                    .mov(&(rax + 8), TMP)
                    .mov(TMP, self.universe.pair_info.entry_word() as i64)
                    .mov(&closure_info(rax), TMP)
                    .pop(TMP)
                    .mov(&(rax + 16), TMP);
            }
            &MkOopArray(ref len, ref fill) => {
                let mut precall_map = stackmap;
                self.compile(fill, precall_map)?;
                self.push_oop(rax, &mut precall_map);
                self.compile(len, precall_map)?;
                self.emit
                    .mov(rdi, UNIVERSE_PTR)
                    .mov(rsi, rax)
                    .pop(rdx);
                self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                    emit.mov(rax, alloc_ooparray as i64 )
                        .call(rax);
                });
            }
            &MkI64Array(ref len, ref fill) => {
                let mut precall_map = stackmap;
                self.compile(fill, precall_map)?;
                // unbox and push `fill`
                self.push_word(&(rax + 8), &mut precall_map);
                self.compile(len, precall_map)?;
                // rsi: unboxed `len`
                // rdx: `fill`
                self.emit
                    .mov(rdi, UNIVERSE_PTR)
                    .mov(rsi, &(rax + 8))
                    .pop(rdx);
                self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                    emit.mov(rax, alloc_i64array as i64 )
                        .call(rax);
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
                    self.emit.mov(&closure_info(rax), TMP);
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
                        self.emit.mov(&upval_slot(rax, to_ix), TMP);
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
                            self.emit.mov(rax, &closure_ptr());
                            self.push_oop(&upval_slot(rax, ix), &mut map0);
                        }
                        &Slot::Local(ix) => {
                            self.push_oop(&frame_slot(ix), &mut map0);
                        }
                        _ => panic!("NMkClosure: upval_refs contain {:?}", slot),
                    }
                }

                // Calling out to alloc the closure.
                self.emit.mov(rdi, UNIVERSE_PTR);
                self.load_info(rsi, closure_id);
                self.emit.mov(rdx, rsp);

                self.calling_out(map0, CallingConv::SyncUniverse, |emit| {
                    emit.mov(rax, alloc_closure as i64 )
                        .call(rax);
                });

                // Restore the stack ptr.
                self.emit.add(rsp, 8 * npayloads as i32);
            }
            &MkBox(ref n) => {
                self.compile(n, stackmap)?;
                self.emit.mov(TMP, rax);
                self.emit_allocation(stackmap,
                                     self.universe.box_info.sizeof_instance() as i32,
                                     &[(true, TMP)]);
                self.emit
                    .mov(&(rax + 8), TMP)
                    .mov(TMP, self.universe.box_info.entry_word() as i64)
                    .mov(&closure_info(rax), TMP);
            }
        }
        Ok(())
    }

    fn compile(&mut self, node: &RawNode, stackmap: StackMap) -> CgResult<()> {
        match node {
            &NLit(ref lit) => {
                self.compile_literal_node(lit, stackmap)?;
            }
            &NAlloc(ref alloc) => {
                self.compile_alloc_node(alloc, stackmap)?;
            }
            &NReadBox(ref n) => {
                self.compile(n, stackmap)?;
                self.emit.mov(rax, &(rax + 8));
            }
            &NWriteBox(ref n, ref v) => {
                let mut map0 = stackmap;
                self.compile(v, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(n, map0)?;
                self.emit
                    .pop(TMP)
                    .mov(&(rax + 8), TMP);
            }
            &NCall { ref func, ref args, is_tail } => {
                let mut precall_map = stackmap;
                // Eval args in the reverse order
                for arg in args.iter().rev() {
                    self.compile(arg, precall_map)?;
                    self.push_oop(rax, &mut precall_map);
                }
                // Eval func
                self.compile(func, precall_map)?;
                self.emit.mov(CLOSURE_PTR, rax);

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
                    self.emit_optimized_if_cmp(cond.as_ref(), &mut label_false, stackmap)?
                } else {
                    false
                };

                if !special_case {
                    self.compile(cond, stackmap)?;

                    self.load_reloc(TMP, Reloc::of_bool(false));
                    self.emit
                        .cmp(rax, TMP)
                        .je(&mut label_false);
                }

                self.compile(on_true, stackmap)?;
                self.emit
                    .jmp(&mut label_done)
                    .bind(&mut label_false);

                self.compile(on_false, stackmap)?;
                self.emit.bind(&mut label_done);
            }
            &NSeq(ref body, ref last) => {
                for n in body {
                    self.compile(n, stackmap)?;
                }
                self.compile(last, stackmap)?;
            }
            &NReadArgument(arg_ix) => {
                self.emit.mov(rax, ARG_REGS[arg_ix]);
            }
            &NReadSlot(Slot::Global(name)) => {
                self.load_reloc(rax, Reloc::Global(name.to_owned()));
                // self.emit.lea(rax, self.labels.get_mut(name).unwrap());
            }
            &NReadSlot(Slot::Local(ix)) => {
                self.emit.mov(rax, &frame_slot(ix));
            }
            &NReadSlot(Slot::UpVal(ix)) => {
                self.emit.mov(rax, &closure_ptr());
                self.emit.mov(rax, &upval_slot(rax, ix));
            }
            &NBindLocal(ref bs, ref n) => {
                let mut map0 = stackmap;
                for &(ix, ref bn) in bs {
                    self.compile(bn, map0)?;
                    self.emit.mov(&frame_slot(ix), rax);
                    map0.set_local_slot(ix, true);
                }
                self.compile(n, map0)?;
            }
            &NRecBindLocal(ref bs, ref n) => {
                let mut map0 = stackmap;
                let alloc_sizes = sequence_v(bs.iter()
                    .map(|&(_, ref n)| self.sizeof_alloc(n))
                    .collect())?;
                let total_alloc_size: usize = alloc_sizes.iter().sum();
                // 1. Alloc all oops at once.
                self.emit_allocation(map0, total_alloc_size as i32, &[]);

                // 2. Bind the uninitialized oops to their local names.
                let mut alloc_ptr_offset = 0;
                for (ith_node, &(ix, ref _bn)) in bs.iter().enumerate() {
                    self.emit
                        .lea(TMP, &(rax + alloc_ptr_offset as i32))
                        .mov(&frame_slot(ix), TMP);
                    alloc_ptr_offset += alloc_sizes[ith_node];
                    map0.set_local_slot(ix, true);
                }

                // 3. Initialize the oops.
                for (ith_node, &(_ix, ref bn)) in bs.iter().enumerate() {
                    self.compile_placement_init(bn)?;

                    // Not the last node: increase the offset.
                    if ith_node != bs.len() - 1 {
                        self.emit.add(rax, alloc_sizes[ith_node] as i32);
                    }
                }
                self.compile(n, map0)?;
            }
            &NReadArrayLength(ref arr) => {
                let map0 = stackmap;
                self.compile(arr, map0)?;
                self.emit.mov(TMP, &(rax + 8));
                self.emit_fixnum_allocation(map0, TMP);
            }
            &NReadOopArray(ref arr, ref ix) => {
                let mut map0 = stackmap;
                self.compile(ix, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(arr, map0)?;
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .mov(rax, &(rax + TMP * 8 + 16));  // array indexing
            }
            &NReadI64Array(ref arr, ref ix) => {
                let mut map0 = stackmap;
                self.compile(ix, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(arr, map0)?;
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .mov(rax, &(rax + TMP * 8 + 16));  // array indexing
            }
            &NWriteOopArray(ref arr, ref ix, ref val) => {
                let mut map0 = stackmap;
                self.compile(val, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(ix, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(arr, map0)?;
                self.emit
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .lea(TMP, &(rax + TMP * 8 + 16))
                    .pop(rax)
                    .mov(&Addr::B(TMP), rax);
            }
            &NPrimFF(op, ref n1, ref n2) => {
                let mut map0 = stackmap;
                self.compile(n2, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(n1, map0)?;

                // Unbox the lhs to rax and pop the rhs to TMP.
                self.emit
                    .mov(rax, &(rax + 8))
                    .pop(TMP);
                map0.pop();

                // Save the unboxed result to %rax
                match op {
                    PrimOpFF::Add => {
                        self.emit
                            .add(rax, &(TMP + 8));
                    }
                    PrimOpFF::Sub => {
                        self.emit
                            .sub(rax, &(TMP + 8));
                    }
                    PrimOpFF::Lt | PrimOpFF::Eq => {
                        self.emit.cmp(rax, &(TMP + 8));
                        self.load_reloc(TMP, Reloc::of_bool(true));
                        self.load_reloc(rax, Reloc::of_bool(false));
                        self.emit.cmovcc(op_to_cond(op), rax, TMP);
                        return Ok(());
                    }
                }
                // Allocate a new fixnum to store the result.
                self.emit.mov(TMP, rax);
                self.emit_fixnum_allocation(map0, TMP);
            }
            &NPrimO(ref op, ref n1) => {
                self.compile(n1, stackmap)?;
                match *op {
                    PrimOpO::Display => {
                        self.emit
                            .mov(rdi, rax)
                            .mov(rsi, UNIVERSE_PTR);
                        // Safe to use conv::internal since we don't alloc in display.
                        self.calling_out(stackmap, CallingConv::Internal, |emit| {
                            emit.mov(rax, display_oop as i64 )
                                .call(rax);
                        });
                    }
                    PrimOpO::Panic => {
                        self.emit
                            .mov(rdi, UNIVERSE_PTR)
                            .mov(rsi, rax);
                        // Need to sync the universe since we will be unwinding
                        // the generated code's stack.
                        self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                            emit.mov(rax, panic as i64)
                                .call(rax);
                        });
                    }
                    PrimOpO::Fixnump => {
                        self.emit
                            .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
                            .cmp(TMP, &Addr::B(rax));

                        self.load_reloc(TMP, Reloc::of_bool(true));
                        self.load_reloc(rax, Reloc::of_bool(false));

                        self.emit.cmove(rax, TMP);
                    }
                    PrimOpO::CompileModule => {
                        self.emit
                            .mov(rdi, UNIVERSE_PTR)
                            .mov(rsi, rax);

                        self.calling_out(stackmap, CallingConv::SyncUniverse, |emit| {
                            emit.mov(rax, compile_module as i64)
                                .call(rax);
                        });
                    }
                }
            }
            _ => panic!("Unimplemented node: {:?}", node),
        };

        Ok(())
    }

    // To rax.
    fn emit_allocation(&mut self, stackmap: StackMap, alloc_size: i32, tmp_regs: &[(bool, R64)]) {
        let mut label_alloc_success = Label::new();
        // Check heap overflow.
        self.emit
            .mov(rax, ALLOC_PTR)
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
            .mov(ALLOC_PTR, rax)
            .mov(rdi, UNIVERSE_PTR)
            .mov(rsi, alloc_size as i64);
        self.calling_out(map0, CallingConv::SyncUniverse, |emit| {
            emit.mov(rax, full_gc as i64 )
                .call(rax);
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
            .mov(&(rax + 8), value_reg)
            .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
            .mov(&Addr::B(rax), TMP);
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
                self.compile(rhs, map0)?;
                self.push_oop(rax, &mut map0);
                self.compile(lhs, map0)?;
                self.emit
                    .mov(rax, &(rax + 8))
                    .pop(TMP)
                    .mov(TMP, &(TMP + 8))
                    .cmp(rax, TMP)
                    .jcc(op_to_cond(op).inverse(), label_false);
                true
            }
            &NPrimO(PrimOpO::Fixnump, ref rand) => {
                self.compile(rand, map0)?;
                self.emit
                    .mov(TMP, self.universe.fixnum_info.entry_word() as i64)
                    .cmp(TMP, &Addr::B(rax))
                    .jne(label_false);
                true
            }
            _ => false,
        })
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
    Addr::BD(rbp, -8 * ((1 + EXTRA_CALLER_SAVED_FRAME_SLOTS + ix) as i32))
}

fn upval_slot(closure_ptr: R64, ix: usize) -> Addr {
    closure_ptr + (1 + ix as i32) * 8
}

fn closure_ptr() -> Addr {
    rbp + (-8)
}

fn closure_info(r: R64) -> Addr {
    Addr::B(r)
}

fn emit_prologue(emit: &mut Emit, frame_descr: &FrameDescr) -> (StackMap, usize) {
    emit.push(rbp)
        .mov(rbp, rsp)
        .push(CLOSURE_PTR);

    let frame_slots = frame_descr.local_slot_count();
    if frame_slots != 0 {
        emit.add(rsp, -8 * (frame_slots as i32));
    }

    let bare_entry = emit.here();

    (new_stackmap_with_frame_descr(frame_descr), bare_entry)
}

fn emit_epilogue(emit: &mut Emit, want_ret: bool) {
    emit.mov(rsp, rbp)
        .pop(rbp);

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
        .push(rbp)
        .mov(rbp, rsp);

    // Save the callee-saved regs clobbered by our runtime.
    emit.push(ALLOC_PTR)
        .push(UNIVERSE_PTR);

    // Get the relevant regs from/to the runtime state.
    emit.mov(UNIVERSE_PTR, rsi)
        .mov(ALLOC_PTR, &universe_alloc_ptr());

    // Make space for the base rbp list node and
    // add the current rbp to the base rbp list.
    emit.sub(rsp, size_of::<NativeInvocationChain>() as i32);
    emit.mov(TMP, &universe_invocation_chain())
        .mov(&(rsp + OFFSET_OF_ICHAIN_NEXT), TMP)
        .mov(&(rsp + OFFSET_OF_ICHAIN_BASE_rbp), rbp)
        .mov(TMP, 0_i64)
        .mov(&(rsp + OFFSET_OF_ICHAIN_TOP_rbp), TMP)
        .mov(&(rsp + OFFSET_OF_ICHAIN_TOP_rip), TMP)
        .mov(&universe_invocation_chain(), rsp);

    // Enter the real main.
    emit.call(&Addr::B(CLOSURE_PTR));

    // Sync back the regs to the runtime state,
    // And pop the ichain.
    emit.mov(&universe_alloc_ptr(), ALLOC_PTR)
        .mov(TMP, &universe_invocation_chain())
        .mov(TMP, &(TMP + OFFSET_OF_ICHAIN_NEXT))
        .mov(&universe_invocation_chain(), TMP);

    // Dealloc the rbp list node.
    emit.add(rsp, size_of::<NativeInvocationChain>() as i32);

    // Restore the saved regs.
    emit.pop(UNIVERSE_PTR)
        .pop(ALLOC_PTR);

    // Standard epilogue.
    emit.mov(rsp, rbp)
        .pop(rbp)
        .ret();

    // 2. Get it.
    let entry_offset = entry.offset().unwrap();
    (entry_offset, emit.here())
}

fn sequence_v<A, E>(xs: Vec<Result<A, E>>) -> Result<Vec<A>, E> {
    let mut ys = vec![];
    for x in xs {
        ys.push(x?);
    }
    Ok(ys)
}

const CLOSURE_PTR: R64 = rdi;
const ARG_REGS: [R64; 5] = [rsi, rdx, rcx, r8, r9];

// Caller saved regs.
const TMP: R64 = r10;
const TMP2: R64 = r11;

// Callee saved regs.
const ALLOC_PTR: R64 = r12;
const UNIVERSE_PTR: R64 = r13;

const OPTIMIZE_IF_CMP: bool = true;
const OPTIMIZE_KNOWN_SELF_CALL: bool = true;
const OPTIMIZE_SMALL_MKCLOSURE: bool = true;
const OPTIMIZE_FIXNUM_ALLOC_FAST_PATH: bool = true;
