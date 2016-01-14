use super::shared::*;
use super::codegen::CompiledModule;
use rt::*;
use rt::oop::*;
use rt::stackmap::StackMapTable;
use assembler::mem::JitMem;

use std::collections::HashMap;
use std::mem::transmute;
use byteorder::{NativeEndian, ByteOrder};

pub type JitEntry = unsafe extern "C" fn(*const Universe);

pub struct LinkedModule {
    jitmem: JitMem,
    entry_offset: usize,
    smt: StackMapTable,
}

impl LinkedModule {
    pub unsafe fn new(cm: CompiledModule, u: &Universe) -> Self {
        let mut jitmem = JitMem::new(cm.emit.as_ref());
        let start = jitmem.start();

        let mut global_closures: GlobalTable = HashMap::new();

        // Make closures.
        for (func_name, ref func) in &cm.functions {
            let info = InfoTable::from_entry(start + func.entry_offset);
            global_closures.insert(func_name.to_owned(), u.new_closure(info));
        }

        for (func_name, ref func) in &cm.functions {
            for (reloc_offset, ref reloc) in &func.relocs {
                let val = reloc.reify(&global_closures, u);
                NativeEndian::write_u64(&mut jitmem.as_mut()[reloc_offset..], val as u64);
            }
        }

        LinkedModule {
            jitmem: jitmem,
            entry_offset: panic!("make_rust_entry"),
            smt: cm.smt,
        }
    }

    pub unsafe fn call_entry(&self, u: &Universe) {
        let entry = transmute::<_, JitEntry>(self.jitmem.start() + self.entry_offset);
        entry(u as *const _);
    }
}
