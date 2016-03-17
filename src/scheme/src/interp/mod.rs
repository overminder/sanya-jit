/// The interpreter is generated from an assembly template.
/// This way we can have much better control on the execution environment
/// (GC, native interop etc).

pub struct InterpGen {
}

/// Register-based VM instructions. Layout:
/// [63 ... 48] [47 ... 32] [31 ... 16] [15 ... 0]
/// [C or ImmH] [B or ImmL] [A        ] [Op      ]
#[repr(C)]
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
