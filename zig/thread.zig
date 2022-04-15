const std = @import("std");
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
        return Self {
            .id = 0,
            .stackDepth = 0,
            .heap = try heap.TestArena.init(),
        };
    }
    pub fn deinit(self : *Self) void {
        self.heap.deinit();
        self.* = undefined;
    }
    pub inline fn stack(self: Self) [*]Object {
        return self.heap.tos;
    }
    pub inline fn push(self: *Self, obj: Object) void {
        if (self.heap.space()<1) unreachable;
        self.heap.tos -= 1;
        self.heap.tos[0]=obj;
        self.stackDepth += 1;
    }
    pub inline fn pop(self: *Self, n: u8) void {
        self.stackDepth -= n;
        if (self.stackDepth<0) unreachable;
        self.heap.tos += n;
    }
    pub inline fn getArena(self: *Self) *heap.Arena {
        return &self.heap;
    }
};
