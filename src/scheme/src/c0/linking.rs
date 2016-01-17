use super::shared::*;
use super::codegen::{CompiledModule, make_rust_entry};
use ast::id::Id;
use rt::*;
use rt::oop::*;
use rt::stackmap::StackMapTable;
use assembler::mem::JitMem;

use std::collections::HashMap;
use std::mem::transmute;
use byteorder::{NativeEndian, ByteOrder};

pub type JitEntry = unsafe extern "C" fn(Oop, *const Universe);

pub struct LinkedModule {
    jitmem: JitMem,
    rust_entry_offset: usize,
    main_closure: Handle<Closure>,
    smt: StackMapTable,
    infotables: Vec<*const ClosureInfo>,
}

impl LinkedModule {
    pub unsafe fn new(mut cm: CompiledModule, u: &Universe) -> Self {
        let rust_entry_offset = make_rust_entry(&mut cm.emit);

        let mut jitmem = JitMem::new(cm.emit.as_ref());
        let start = jitmem.start();
        cm.smt.set_start(start);

        let mut global_closures: GlobalTable = HashMap::new();

        let mut infotables = vec![];

        // Make closures.
        for (func_name, ref func) in &cm.functions {
            let info = InfoTable::from_entry(start + func.entry_offset);
            infotables.push(info as *const _);
            let closure = u.new_closure(info);  // Allocates
            global_closures.insert(func_name.to_owned(), closure);

            debug!("Closure {:?}: [{:#x},{:#x})",
                   func_name,
                   start + func.entry_offset,
                   start + func.end_offset);
            let gcrefs = func.relocs
                             .iter()
                             .map(|&(reloc_offset, _)| GcRef::OopConst(reloc_offset as u32))
                             .chain(func.inforefs
                                        .iter()
                                        .map(|ix| GcRef::PcRelInfoEntry(*ix as u32)))
                             .collect();

            debug!("  gcrefs = {:?}", gcrefs);
            info.set_gcrefs(gcrefs);
        }

        // Make oop consts.
        for func in cm.functions.values() {
            let entry_offset = func.entry_offset;
            for &(reloc_offset, ref reloc) in &func.relocs {
                let val = reloc.reify(&global_closures, u);  // Allocates
                NativeEndian::write_u64(&mut jitmem.as_mut()[entry_offset + reloc_offset..],
                                        val as u64);
            }
        }

        infotables.shrink_to_fit();

        LinkedModule {
            jitmem: jitmem,
            rust_entry_offset: rust_entry_offset,
            main_closure: global_closures[&Id::named("main")].dup(),
            smt: cm.smt,
            infotables: infotables,
        }
    }

    pub fn infotables(&self) -> &[*const InfoTable<Closure>] {
        &self.infotables
    }

    pub fn smt(&self) -> &StackMapTable {
        &self.smt
    }

    pub unsafe fn call_entry(&mut self, u: &mut Universe) {
        u.set_smt(&self.smt);
        u.set_compiled_infos(&mut self.infotables);
        let entry = transmute::<_, JitEntry>(self.jitmem.start() + self.rust_entry_offset);
        entry(*self.main_closure.oop(), u as *const _);
    }
}
