pub const config = @import("config.zig");
const ptr = @import("object/ptr.zig");
pub const object = @import("object.zig");
pub const execute = @import("execute.zig");
pub const Context = @import("context.zig");
pub const Process = @import("process.zig");
pub const heap = @import("heap.zig");
pub const globalArena = @import("globalArena.zig");
pub const symbol = @import("symbol.zig");
pub const utilities = @import("utilities.zig");
pub const threadedFn = @import("threadedFn.zig");
pub const llvm = @import("libs/zig-llvm/src/llvm.zig");
test "ping" {
    _ = ptr;
}
