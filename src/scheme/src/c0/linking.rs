use super::shared::*;
use super::codegen::{CompiledModule, make_rust_entry};
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
}

impl LinkedModule {
    pub unsafe fn new(mut cm: CompiledModule, u: &Universe) -> Self {
        let rust_entry_offset = make_rust_entry(&mut cm.emit);

        let mut jitmem = JitMem::new(cm.emit.as_ref());
        let start = jitmem.start();
        cm.smt.set_start(start);

        let mut global_closures: GlobalTable = HashMap::new();

        // Make closures.
        for (func_name, ref func) in &cm.functions {
            let info = InfoTable::from_entry(start + func.entry_offset);
            let closure = u.new_closure(info);  // Allocates
            global_closures.insert(func_name.to_owned(), closure);

            println!("Closure/{}: [{:#x},{:#x})",
                     func_name,
                     start + func.entry_offset,
                     start + func.end_offset);

            info.set_constant_offsets(func.relocs
                                          .iter()
                                          .map(|&(ref reloc_offset, _)| *reloc_offset)
                                          .collect());
        }

        for func in cm.functions.values() {
            for &(ref reloc_offset, ref reloc) in &func.relocs {
                let val = reloc.reify(&global_closures, u);  // Allocates
                NativeEndian::write_u64(&mut jitmem.as_mut()[*reloc_offset..], val as u64);
            }
        }

        LinkedModule {
            jitmem: jitmem,
            rust_entry_offset: rust_entry_offset,
            main_closure: global_closures["main"].dup(),
            smt: cm.smt,
        }
    }

    pub unsafe fn call_entry(&self, u: &mut Universe) {
        u.set_smt(&self.smt);
        let entry = transmute::<_, JitEntry>(self.jitmem.start() + self.rust_entry_offset);
        entry(*self.main_closure.oop(), u as *const _);
    }
}
