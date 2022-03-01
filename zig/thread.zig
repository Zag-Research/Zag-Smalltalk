var next_thread_number : u64 = 0;
const default_heap_size = 512;
const Allocator = @import("std").mem.Allocator;
const Object = @import("object.zig").Object;
pub const Thread = struct {
    id : u64,
    heap : [*]Object,
    stack: [*]Object,
    allocated: []Object,
    allocator: Allocator,
    const Self = @This();
    pub fn init(allocator: Allocator,size:usize) !Thread {
        defer next_thread_number += 1;
        const allocated = allocator.alloc(Object,size) catch |err| return err;
        return Thread {
            .id = next_thread_number,
            .stack = allocated.ptr+allocated.len,
            .heap = allocated.ptr,
            .allocated = allocated,
            .allocator = allocator,
        };
    }
    pub fn deinit(self : *Self) void {
        self.allocator.free(self.allocated);
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
