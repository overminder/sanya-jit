/// c0: The baseline compiler.

pub mod codegen;
pub mod linking;
mod compiled_rt;
mod shared;

use self::codegen::{ModuleCompiler, CompiledModule};
use rt::Universe;
use ast::nir::ScDefn;

pub fn compile(scdefns: &mut [ScDefn], u: &Universe) -> CompiledModule {
    let mut mc = ModuleCompiler::new();
    for scdefn in scdefns {
        mc.compile_scdefn(scdefn, u);
    }
    mc.into_compiled_module()
}
