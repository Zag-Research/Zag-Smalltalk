const std = @import("std");
var next_thread_number : u64 = 0;
const Object = @import("object.zig").Object;
const heap = @import("heap.zig");
pub const Thread = struct {
    id : u64,
    heap : heap.Arena,
    const Self = @This();
    pub fn init() !Self {
        defer next_thread_number += 1;
        return Self {
            .id = next_thread_number,
            .heap = try heap.NurseryArena.init(null),
        };
    }
    pub fn deinit(self : *Self) void {
        self.heap.deinit();
        self.* = undefined;
    }
    pub inline fn stack(self: Self) [*]Object {
        return self.heap.tos;
    }
};
