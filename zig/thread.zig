var next_thread_number : u64 = 0;
const default_heap_size = 512;
const Allocator = @import("std").mem.Allocator;
const Object = @import("object.zig").Object;
const memory = @import("memory.zig");
pub const Thread = struct {
    id : u64,
    heap : Memory.Heap,
    const Self = @This();
    pub fn init(allocator: Allocator,size:usize) !Self {
        defer next_thread_number += 1;
        return Self {
            .id = next_thread_number,
            .heap = memory.Heap.init(allocator,size),
        };
    }
    pub fn deinit(self : *Self) void {
        self.heap.deinit();
        self.* = undefined;
    }
};
fn thread0test(allocator:Allocator) !void {
    var thread = Thread.init(allocator,default_heap_size) catch |err| return err;
    defer thread.deinit();
}
test "thread 0 initialization" {
    try withAllocator(thread0test);
}
pub fn withAllocator(f : anytype) !void {
    var gpa = @import("std").heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) @panic("Allocation leakage");
    }
    //    const allocator = @import("std").heap.page_allocator;
    try f(allocator);
}
