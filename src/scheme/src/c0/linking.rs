use super::shared::*;
use super::codegen::{CompiledModule, make_rust_entry};
use ast::id::Id;
use rt::*;
use rt::oop::*;
use rt::stackmap::{StackMapTable, StackMapTableInserter};
use assembler::mem::JitMem;

use std::collections::HashMap;
use std::mem::transmute;
use byteorder::{NativeEndian, ByteOrder};

pub type JitEntry = unsafe extern "C" fn(Oop, *const Universe);

pub struct LinkedModule {
    jitmem: JitMem,
    rust_entry_offset: usize,
    global_closures: Option<GlobalTable>,
    smt: StackMapTable,
    infotables: Vec<*const ClosureInfo>,
}

impl LinkedModule {
    pub unsafe fn new(mut cm: CompiledModule, u: &Universe) -> Self {
        let (rust_entry_offset, rust_entry_end_offset) = make_rust_entry(&mut cm.emit);

        let mut jitmem = JitMem::new(cm.emit.as_ref());
        let start = jitmem.start();
        let smti = StackMapTableInserter::new(start);

        let mut global_closures: GlobalTable = HashMap::new();

        let mut infotables = vec![];

        let mut smt: StackMapTable = Default::default();

        // Make closures.
        for (func_name, ref func) in &cm.functions {
            let info = InfoTable::from_entry(start + func.entry_offset);
            smti.extend_with_smo(&mut smt, func.entry_offset, &func.smo);
            info.set_smo(func.smo.to_owned());
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
        debug!("RustEntry: [{:#x},{:#x})",
               start + rust_entry_offset,
               start + rust_entry_end_offset);

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
            global_closures: Some(global_closures),
            smt: smt,
            infotables: infotables,
        }
    }

    pub fn infotables(&self) -> &[*const InfoTable<Closure>] {
        &self.infotables
    }

    pub fn smt(&self) -> &StackMapTable {
        &self.smt
    }

    pub unsafe fn call_nullary(&mut self, u: &mut Universe, name: &str) {
        use std::collections::hash_map::Entry::Occupied;

        u.set_smt(&self.smt);
        u.set_compiled_infos(&mut self.infotables);
        let rust_entry = transmute::<_, JitEntry>(self.jitmem.start() + self.rust_entry_offset);
        let oop_entry = {
            let mut global_closures = self.global_closures.take().unwrap();
            match global_closures.entry(Id::named(name)) {
                Occupied(o) => o.remove(),
                _ => panic!("call_nullary: closure {} not defined.", name),
            }
        };
        rust_entry(*oop_entry.oop(), u as *const _);
    }
}
