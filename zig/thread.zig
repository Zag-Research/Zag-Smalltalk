const std = @import("std");
const builtin = @import("builtin");
var next_thread_number : u64 = 0;
const Object = @import("object.zig").Object;
const heap = @import("heap.zig");
pub const Thread = struct {
    id : u64,
    stackDepth: i32,
    heap : heap.Arena,
    const Self = @This();
    pub fn init() !Self {
        defer next_thread_number += 1;
        return Self {
            .id = next_thread_number,
            .stackDepth = 0,
            .heap = try heap.NurseryArena.init(),
        };
    }
    pub fn initForTest() !Self {
        if (builtin.is_test) {
            return Self {
                .id = 0,
                .stackDepth = 0,
                .heap = try heap.TestArena.init(),
            };
        }
        else unreachable;
    }
    pub fn deinit(self : *Self) void {
        self.heap.deinit();
        self.* = undefined;
    }
    pub inline fn getArena(self: *Self) *heap.Arena {
        return &self.heap;
    }
};
