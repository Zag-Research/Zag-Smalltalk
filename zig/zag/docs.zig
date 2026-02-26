//! # Zag Smalltalk VM
//!
//! Zag is an experimental Smalltalk VM/runtime written in Zig. Methods are
//! stored as type-annotated ASTs (no bytecode), and execution uses threaded
//! and CPS models interchangeably.
//!
//! ## Core Principle
//!
//! Only 3 operations exist: **message send**, **assignment**, and **return**.
//! No special-casing of control flow methods.
//!
//! ## Modules
//!
//! - `zag.object` — Object encoding and the compile-time-selected `Object` type
//! - `zag.execute` — Execution engine (threaded/CPS), PC, and method dispatch
//! - `zag.primitives` — Built-in primitive operations (integers, floats, arrays, etc.)
//! - `zag.process` — Process structure, stack management, and nursery GC
//! - `zag.heap` — Heap object layout, formats, and allocation
//! - `zag.dispatch` — Polymorphic inline cache and signature-based method lookup
//! - `zag.controlWords` — Threaded control flow operations (branch, send, return)
//! - `zag.context` — Execution context (stack frame) management
//! - `zag.symbol` — Interned symbol table
//! - `zag.config` — Compile-time build options and debug flags
//! - `zag.utilities` — General data structures and bit utilities

pub const zag = @import("zag.zig");
