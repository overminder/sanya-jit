/// c0: The baseline compiler.

pub mod codegen;
pub mod linking;
mod compiled_rt;

use self::codegen::CompiledModule;

pub fn compile(scdefns: &mut [ScDefn], universe: &Universe) -> CompiledModule {
    let mut mc = ModuleCompiler::new();
    for scdefn in scdefns {
        mc.compile_scdefn(scdefn);
    }
    mc.into_compiled_module()
}
