const object = @import("object.zig");
const Object = object.Object;
const mem = @import("std").mem;
const Allocator = mem.Allocator;
const class = @import("class.zig");
const heap = @import("heap.zig");
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
    pub fn alloc(self : *Self, classIndex : class.ClassIndex, format: heap.Format, iv_size : usize, array_size : usize, fill: anytype) !heap.HeapPtr {
        var form = format.base();
        var size = iv_size + array_size;
        var fillPattern = @bitCast(Object,@as(u64,0));
        const objectWidth = @sizeOf(Object);
        var width : usize = objectWidth;
        switch (@TypeOf(fill)) {
            Object => {
                fillPattern = fill;
                if (iv_size>0) form=form.object();
                if (array_size>0) form=form.array();
            },
            u8,i8,u16,i16,u32,i32,f32,u64,i64,f64 => |T| {
                width = @sizeOf(T);
                size = (size+objectWidth-@sizeOf(T))/objectWidth;
                form = form.raw(T,size);
            },
            else => {},
        }
        const result = self.heap;
        _ = self;
        _ = classIndex;
        _ = format;
        _ = iv_size;
        _ = array_size;
        _ = fill;
        return @ptrCast(heap.HeapPtr,result);
    }
    pub fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize) !heap.HeapPtr {
        return self.alloc(classIndex, heap.Format.object, iv_size, 0, object.Nil);
    }
    pub fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, fill: anytype) !heap.HeapPtr {
        return self.alloc(classIndex, heap.Format.raw64, 0, array_size, fill);
    }
};
const TestHeap = struct {
    expected: []const Object,
    output: []Object,
    heap : Heap,
    const Self=@This();
    const testing = @import("std").testing;
    fn init(expected:[]const Object) !Self {
        const output = try testing.allocator.alloc(Object,expected.len);
        const allocator = @import("std").heap.FixedBufferAllocator.init(mem.sliceAsBytes(output)).allocator();
        return Self {
            .expected = expected,
            .output = output,
            .heap = try Heap.init(allocator,expected.len),
        };
    }
    fn deinit(self: *Self) void {
        self.heap.deinit();
        testing.allocator.free(self.output);
        self.* = undefined;
        //testing.allocator.deinit();
    }
    fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize) !heap.HeapPtr {
        return self.heap.allocObject(classIndex, iv_size);
    }
    fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, fill: anytype) !heap.HeapPtr {
        return self.heap.allocRaw(classIndex, array_size, fill);
    }
    fn alloc(self : *Self, classIndex : class.ClassIndex, format: heap.Format, iv_size : usize, array_size : usize, fill: anytype) !heap.HeapPtr {
        return self.heap.alloc(classIndex,format,iv_size,array_size,fill);
    }
    fn isEqual(self: Self) bool {
        for (self.expected) |item, index| {
            if (@bitCast(u64,self.output[index]) != @bitCast(u64,item)) return false;
        }
        return true;
    }
    fn expectEqual(self: Self) !void {
        try testing.expect(self.isEqual());
    }
};
test "fixed buffer allocator" {
    const h = heap.header(3,heap.Format.object,42);
    const expected = [_]Object{@bitCast(Object,h),object.True,object.Nil,object.False};
    var testHeap = try TestHeap.init(expected[0..]);
    defer testHeap.deinit();
    const obj = try testHeap.allocObject(42,3);
    const ivs = obj.instVars();
    ivs[0]=object.True;
    ivs[2]=object.False;
    try testHeap.expectEqual();
}
