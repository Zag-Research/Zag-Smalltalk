const Object = @import("object.zig").Object;
const Allocator = @import("std").mem.Allocator;
const Class = @import("class.zig");
const Heap = struct {
    heap: [*]Object,
    tos: [*]Object,
    allocated: []Object,
    allocator: Allocator,
    const Self = @This();
    pub fn init(allocator: Allocator,size:usize) !Self {
        const allocated = allocator.alloc(Object,size) catch |err| return err;
        return Self {
            .tos = allocated.ptr+allocated.len,
            .heap = allocated.ptr,
            .allocated = allocated,
            .allocator = allocator,
        };
    }
    pub fn deinit(self : *Self) void {
        self.allocator.free(self.allocated);
        self.* = undefined;
    }
    pub fn alloc(self : *Self, class : Class.ClassIndex, format: Format, iv_size : usize, array_size : usize, fill: anytype) !Object {
    }
};
test "fixed buffer allocator" {
    const expect = @import("std").testing.expect;
    var buffer: [1024]u8 = undefined;
    const allocator = @import("std").heap.FixedBufferAllocator.init(&buffer).allocator();

}
