/// c0: The baseline compiler.

pub struct ModuleContext {
    functions: Vec<FunctionContext>,
}

pub struct FunctionContext {
    name: String,
}
