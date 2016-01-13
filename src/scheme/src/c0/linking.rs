use assembler::mem::JitMem;

pub type JitEntry = unsafe extern "C" fn(*const Universe);

pub struct LinkedModule {
    jitmem: JitMem,
    entry_offset: usize,
    smt: StackMapTable,
}

impl LinkedModule {
    pub unsafe fn new(cm: CompiledModule, u: &Universe) -> Self {
        let jitmem = JitMem::new(cm.emit.as_ref());
        let start = jitmem.start();

        let mut global_closures = HashMap::new();

        // Make closures.
        for (func_name, func) in cm.functions {
            let info = InfoTable::from_entry(start + func.entry_offset);
            global_closures.insert(func_name.to_owned(), u.alloc_closure(info));
        }

        for (func_name, func) in cm.functions {
            for (reloc_offset, reloc) in func.relocs {
                let val = reloc.reify(&global_closures, u);
                jitmem.write_i64(reloc_offset, val);
            }
        }

        LinkedModule {
            jitmem: jitmem,
            entry_offset: entry_offset,
            smt: cm.smt,
        }
    }

    pub unsafe fn call_entry(&self, u: &Universe) {
        let entry = transmute::<_, JitEntry>(jitmem.start() + self.entry_offset);
        entry(u as *const _);
    }
}

pub type RelocTable = Vec<(usize, Reloc)>;

#[derive(Debug)]
pub enum Reloc {
    Global(String),
    Bool(bool),
    Fixnum(i64),
}

type GlobalTable = HashMap<String, Handle<Closure>>;

impl Reloc {
    fn reify(&self, globals: &GlobalTable, u: &Universe) -> Oop {
        use self::Reloc::*;

        match self {
            &Global(name) -> globals[name].as_oop(),
            &Fixnum(v) -> u.alloc_fixnum(v).as_oop(),
            _ -> panic!("Not implemented: {:?}", self),
        }
    }
}

