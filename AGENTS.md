---
name: smalltalk-runtime-context
description: Architectural constraints for our Zig-based Smalltalk VM
---

# Project Context: Smalltalk Runtime in Zig

## Core Architecture
- **Object Layout:** Objects are represented by an `Object` struct.
There are many versions of this -- all in the zig/zag/encoding directory. The default coding is in zig/zag/encoding/zag. No other part of the system should know about the structure of an `Object` but should call methods on `Object`
- **Memory Management:** Memory is structured into 3 levels:
    1. a stack. All operations push/pop values onto/off the stack. The stack is allocated at the beginning of a process. 
    2. a nursery heaps made up of 2 arenas that use a copying collector and copy back and forth beteen the arenas. This is also part of the `Process`. `Object`s on the stack can point to `Object`s in the nursery, but not vice versa.
    3. the shared Global Arena which uses an un-moving Mark-and-Sweep GC. `Object`s on the stack or in a nursery can point to `Object`s in the global arena, but not vice versa. The Global allocator has an interface so it can act as a zig allocator.
- **Process:** A `Process` is the memory structure for a running user process. A `Process` can have 0 or 1 CPU/OS thread executing in it at any time. This means there is never any conflict so no locks are required. It contains the stack, the nursery arenas, as well as the static position of the process when no thread is executing in it. This static position includes the stack pointer, the program counter, a pointer to the current context.
- **Interpreter:** The core loop is a direct-threaded interpreter.

## Zig Style Preferences
- **Allocators:** Never use a global allocator. Every function that allocates must explicitly accept a `std.mem.Allocator`.
- **Errors:** Prefer explicit error bubbling using `!` and payload capturing (`if (expr) |val|`).
- **Comptime:** We leverage `comptime` heavily. Do not generate macro-like inline blocks if `comptime` type-reflection can solve it cleanly.

## Common Definitions
- When I say "OOP Header", refer to the `struct HeapHeader` defined in `zig/zag/heap.zig`.
