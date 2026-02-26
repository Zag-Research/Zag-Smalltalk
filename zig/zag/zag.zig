//! Zag is a Smalltalk VM/runtime in Zig.
//!
//! This module is the public entry point for the core runtime and wires
//! together execution, heap, object encodings, primitives, and optional LLVM.
//!
//! ## Core Components
//!
//! - `object` — Object encoding and the compile-time-selected `Object` type
//! - `execute` — Execution engine (threaded/CPS), PC, and method dispatch
//! - `primitives` — Built-in primitive operations
//! - `Process` — Process structure, stack management, and nursery GC
//! - `heap` — Heap object layout, formats, and allocation
//! - `dispatch` — Polymorphic inline cache and signature-based method lookup
//! - `controlWords` — Threaded control flow operations
//! - `Context` — Execution context (stack frame) management
//! - `symbol` — Interned symbol table
//! - `config` — Compile-time build options and debug flags

/// Build configuration options and feature toggles.
pub const config = @import("config.zig");
pub const InMemory = @import("object/inMemory.zig");
pub const object = @import("object.zig");
pub const Object = object.Object;
pub const execute = @import("execute.zig");
pub const Context = @import("context.zig");
pub const Process = @import("process.zig");
pub const heap = @import("heap.zig");
pub const globalArena = @import("globalArena.zig");
pub const symbol = @import("symbol.zig");
pub const controlWords = @import("controlWords.zig");
pub const primitives = @import("primitives.zig");
pub const utilities = @import("utilities.zig");
pub const llvm = if (config.includeLLVM) @import("llvm-build-module") else null;
pub const threadedFn = @import("threadedFn.zig");
pub const Stats = utilities.Stats;
pub const dispatch = @import("dispatch.zig");
pub fn untested() void {
    @import("std").debug.panic("Untested", .{});
}
