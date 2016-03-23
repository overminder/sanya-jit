/// The interpreter is generated from an assembly template.
/// This way we can have much better control on the execution environment
/// (GC, native interop etc).

pub struct InterpGen {
}

/// Stack-based VM instructions. Layout:
/// op_u8 oparg_i16
#[repr(u8)]
pub enum Op {
    Nop = 0,

    // stack[A] = localConsts[B]
    LoadK,

    // stack[A] = args[B]
    LoadA,

    // stack[A] = globals[B]
    LoadG,

    // stack[A] = thisClosure.upvals[B]
    LoadU,

    // stack[A] = infotables[B].alloc(stack[-C:])
    Alloc,

    // Used with InplaceNew to implement letrec. This
    // allocates a closure without filling in its payloads.
    // stack[A] = infotables[B].allocRaw()
    AllocRaw,
    // infotables[B].placementNew(stack[A], stack[-C:])
    PlacementNew,

    // stack[A] = stack[B].call(stack[-C:])
    Call,

    // return stack[A].call(stack[-B:])
    TailCall,

    // if stack[A]: pc += Imm
    If,

    // Binary primops:
    // stack[A] = stack[B] `op` stack[C]

    PrimIntAdd,
    PrimIntEq,
    PrimIntLt,
    PrimIntSub,

    // Size-effective primops:
    // op(stack[A])
    PrimDisplay,
    PrimPanic,
    PrimFixnump,
}

pub enum Instr {
    OpOnly(Op),
    OpArg(Op, i16),
}
