
pub type JitEntry = unsafe extern "C" fn(*const Universe);

pub struct LinkedModule {
    jitmem: JitMem,
    entry_offset: usize,
    smt: StackMapTable,
}

impl LinkedModule {
    pub unsafe fn call_entry(&self, u: &Universe) {
        let entry = transmute::<_, JitEntry>(jitmem.start() + self.entry_offset);
        entry(u as *const _);
    }
}

pub type RelocTable = Vec<(usize, Reloc)>;

pub enum Reloc {
    Global(String),
    Bool(bool),
    Fixnum(i64),
}
