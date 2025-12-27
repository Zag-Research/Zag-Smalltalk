# Copy-and-Patch JIT for Zag

> Reference: https://www.nish7.io/blog/copy-and-patch

## Overview

Copy-and-Patch (CnP) is a JIT compilation technique that generates machine code by copying pre-compiled binary templates and patching in runtime values.

### High-Level Components

- **Stencils**: Pre-compiled binary code templates for each operation
- **MetaVar System**: Generates stencil variants for different configurations
- **Runtime**: Patches constants into stencils to produce executable machine code

### Runtime Algorithm

1. Select the appropriate stencil variant based on the AST node or bytecode being compiled
2. Copy the stencil's binary code into executable memory
3. Patch "holes" with concrete runtime values (e.g., literal values, jump offsets) to produce final machine code

## Zag Integration

Zag's threaded interpreter uses two key features that align well with CnP:

**Calling Convention:**
```zig
pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result
```

**Tail-Recursive / Continuation-Passing Style:**
```zig
@call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
```

These features provide a natural foundation for the CnP system.

---

## Background: Copy-and-Patch Technique

> Excerpts from "Building a Baseline JIT for Lua Automatically" by Haoran Xu
> - Paper: https://sillycross.github.io/assets/copy-and-patch.pdf
> - Blog: https://sillycross.github.io/2023/05/12/2023-05-12/

### The Core Insight

Copy-and-Patch allows generating code without writing a traditional code generator. The key insight is that compilers already solve the problem of generating code for unknown values through **linker relocations**.

Consider this C++ function:

```c
int evaluate_lhs();
int evaluate_rhs();
int evaluate_add() {
  return evaluate_lhs() + evaluate_rhs();
}
```

The compiler produces an object file without knowing the definitions of `evaluate_lhs` and `evaluate_rhs`. The linker later resolves these references. We can leverage this same mechanism at runtime.

### Relocation = Code Generation

The object file contains **relocation records** that describe how to patch the code for any target addresses. By parsing these records, we can act as the linker at runtime, "linking" against any addresses we choose.

For example, `evaluate_add` produces:

```
evaluate_add:
  53 e8 00 00 00 00 89 c3 e8 00 00 00 00 01 d8 5b c3 

Relocation records:
  offset = 2, type = R_X86_64_PLT32, sym = evaluate_lhs, addend = -4
  offset = 9, type = R_X86_64_PLT32, sym = evaluate_rhs, addend = -4
```

The copy-and-patch code generator becomes trivial:

```c
void codegen(uint8_t* dst, uint8_t* lhsFn, uint8_t* rhsFn) {
  constexpr uint8_t code[] = { 
    0x53, 0xe8, 0x00, 0x00, 0x00, 0x00, 0x89, 0xc3, 
    0xe8, 0x00, 0x00, 0x00, 0x00, 0x01, 0xd8, 0x5b, 0xc3 };
  
  // Copy
  memcpy(dst, code, sizeof(code));
  
  // Patch
  *(uint32_t*)(dst + 2) = (uint32_t)(lhsFn - (dst + 2) - 4);
  *(uint32_t*)(dst + 9) = (uint32_t)(rhsFn - (dst + 9) - 4);
}
```

### Continuation-Passing Style = Efficient Branching

The code above works but has poor quality due to function call overhead. Rewriting to continuation-passing style eliminates this:

```c
void continuation(int result);
void evaluate_add(int lhs, int rhs) {
  int result = lhs + rhs;
  [[clang::musttail]] return continuation(result);
}
```

With tail calls, the function ends with a `jmp` instead of `call`/`ret`. By placing the continuation immediately after the current stencil, even the `jmp` can become a fall-through.

This eliminates:
- Indirect dispatch (a major interpreter bottleneck)
- Unnecessary branch instructions

### External Symbols = Runtime Constants

JITs are faster than interpreters partly because they can embed runtime constants directly in the instruction stream. CnP supports this via external symbols:

```c
extern char x;  // External symbol as placeholder
void continuation(uint64_t value);
void pointer_dereference_at_fixed_offset(void* ptr) {
  uint64_t result = *(uint64_t*)((uint64_t)ptr + (uint64_t)&x);
  [[clang::musttail]] return continuation(result);
}
```

The compiler cannot assume anything about `&x`, so it generates a relocation. At runtime, we patch this to any constant value we need.

---

## Core Principles

1. **Relocation = Code Generation**: Leverage linker relocations as markers for runtime patching
2. **Continuation-Passing Style = Efficient Control Flow**: Tail calls enable fall-through between stencils

---

## Challenges for Zag

### Internal Branches

Many Zag threaded functions contain internal branches that complicate stencil extraction:

```zig
pub const pushLiteral = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const value = pc.object();
        if (sp.push(value)) |newSp| {
            // Fast path
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        } else {
            // Slow path: stack spill
            const newSp, const newContext, const newExtra = sp.spillStackAndPush(value, context, extra);
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
        }
    }
};
```

**Problems:**
- Two distinct execution paths (fast path vs. stack spill)
- Internal branches prevent linear code flow
- No single exit point for simple fall-through
- This pattern is common across most threaded functions

### Architecture support

#### Aarch64 Support:
-  AArch64 does not support arbitrary 64-bit immediates in one instruction.
- The architecture cannot encode a full 64-bit constant directly inside a single instruction. It must synthesize it through multiple instructions (typically ADRP + LDR). Because of that, the compiler does not generate a relocatable immediate the way x86 does.

The relocation model is more complex.

- ARM64 relocations often target pages (high 21 bits) and page offsets, not a raw literal slot. This is ideal for position-independent code but inconvenient when you want a simple “write 8 bytes here” hole.

- The compiler may fold, optimize, or lower the constant differently.
- Even with an external symbol, the compiler might choose a code sequence that scatters the relocation across multiple instructions rather than creating a single memory location to patch.
 - Literal pools are separate structures.
- AArch64 may place the referenced constant into a literal pool instead of inline, so the relocation refers to a pool entry rather than an immediate embedded in the instruction stream. That’s useless for CnP unless you specifically design around literal pools.

[Reference](https://www.nish7.io/blog/copy-and-patch)

### Possible Approaches

#### 1. Assembly Surgery

Extract and linearize assembly manually, sharing slow paths across stencils.

**Pros:**
- Reuses existing threaded functions (single source of truth)

**Cons:**
- Extremely cumbersome to maintain
- Brittle: any assembly change breaks the extractor
- Requires separate extractors per architecture/platform

#### 2. Separate Stencil Sources

Create dedicated stencil functions that:
- Skip tail-call dispatch (runtime patches the continuation)
- Avoid callee-saved register usage (global register allocation)
- Fall through to the next stencil
- Use fast-path only (or handle slow paths separately)

**Pros:**
- Clean stencils
- Full control over generated code

**Cons:**
- Duplicate logic between threaded functions and stencils
- Unclear how to handle slow paths / stack spills

#### 3. Continuation-Only Patching

Keep existing threaded functions unchanged; only patch the continuation targets.

**Pros:**
- Simplest approach
- Reuses existing, tested code
- Internal branches are acceptable
- Literals and offsets can reference memory (no immediate patching needed)

**Cons:**
- May not achieve maximum performance (retains dispatch overhead)
- Stencils are larger than minimal implementations
