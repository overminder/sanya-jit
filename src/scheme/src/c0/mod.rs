/// c0: The baseline compiler.

pub mod codegen;
pub mod linking;
mod compiled_rt;
mod shared;
pub mod cgutil;

use self::codegen::{ModuleCompiler, CompiledModule};
use self::linking::LinkedModule;
use rt::Universe;
use ast::nir::ScDefn;

pub fn compile(scdefns: &[ScDefn], u: &Universe) -> CompiledModule {
    let mut mc = ModuleCompiler::new();
    for scdefn in scdefns {
        mc.add_sc(scdefn);
    }
    mc.compile_all(u);
    mc.into_compiled_module()
}

pub fn link(cm: CompiledModule, u: &Universe) -> LinkedModule {
    unsafe { LinkedModule::new(cm, u) }
}
