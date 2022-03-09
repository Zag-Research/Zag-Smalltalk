const object = @import("object.zig");
const Object = object.Object;
const mem = @import("std").mem;
const Allocator = mem.Allocator;
const class = @import("class.zig");
const heap = @import("heap.zig");
const ArenaType = enum {
    Nursery,
    Teen,
    Global,
    Test,
};
const Arena = struct {
    heap: heap.HeapPtr,
    tos: [*]Object,
    allocated: []Object,
    allocator: Allocator,
    arenaType: ArenaType,
    const Self = @This();
    pub fn init(allocator: Allocator,size:usize, arenaType: ArenaType) !Self {
        const allocated = allocator.alloc(Object,size) catch |err| return err;
        return Self {
            .tos = allocated.ptr+allocated.len,
            .heap = @ptrCast(heap.HeapPtr,allocated.ptr),
            .allocated = allocated,
            .allocator = allocator,
            .arenaType = arenaType,
        };
    }
    pub fn deinit(self : *Self) void {
        self.allocator.free(self.allocated);
        self.* = undefined;
    }
    pub fn alloc(self : *Self, classIndex : class.ClassIndex, format: heap.Format, iv_size : usize, array_size : usize, fill: anytype) !heap.HeapPtr {
        var form = format.base();
        var size = iv_size + array_size;
        var totalSize = size+1;
        var asize = array_size;
        var fillPattern = @bitCast(Object,@as(u64,0));
        const objectWidth = @sizeOf(Object);
        var width : usize = objectWidth;
        switch (@TypeOf(fill)) {
            Object => {
                fillPattern = fill;
                if (iv_size>0) form=form.object();
                if (array_size>0 or format.isIndexable()) {
                    form=form.array();
                    if (array_size>32766) form=form.object(); // big arrays use Format.both
                }
                if (format.isWeak()) form=form.weak();
                if (form.hasBoth()) {
                    totalSize += 1;
                    size = iv_size;
                }
            },
            u8,i8,u16,i16,u32,i32,f32,u64,i64,f64 => |T| {
                width = @sizeOf(T);
                size = (size+objectWidth-width)/objectWidth;
                form = form.raw(T,size);
                asize = size;
                totalSize=size+1;
                if (size>32766) {
                    size=32767;
                    totalSize+=1;
                }
            },
            else => {},
        }
        const result = self.heap;
        const hash = if (self.arenaType==.Test) 0 else @intCast(u24,@ptrToInt(result)%16777213);
        const header = heap.header(@intCast(u16,size),form,classIndex,hash);
        result.* = @bitCast(Object,header);
        const instVars = result.instVars();
        for (instVars) |_,index| {
            instVars[index]=fill;
        }
        if (totalSize>size+1) @ptrCast([*]Object,result)[iv_size+1] = @bitCast(Object,asize);
        if (form.isIndexable()) {
            var indexables = try result.indexables(@TypeOf(fill));
            for (indexables) |_,index| {
                indexables[index]=fill;
            }
        }
        return @ptrCast(heap.HeapPtr,result);
    }
    pub inline fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize) !heap.HeapPtr {
        return self.alloc(classIndex, heap.Format.none, iv_size, 0, object.Nil);
    }
    pub inline fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, fill: anytype) !heap.HeapPtr {
        return self.alloc(classIndex, heap.Format.none, 0, array_size, fill);
    }
};
const TestArena = struct {
    expected: []const Object,
    output: []Object,
    heap : Arena,
    const Self=@This();
    const testing = @import("std").testing;
    fn init(expected:[]const Object) !Self {
        const output = try testing.allocator.alloc(Object,expected.len);
        const allocator = @import("std").heap.FixedBufferAllocator.init(mem.sliceAsBytes(output)).allocator();
        return Self {
            .expected = expected,
            .output = output,
            .heap = try Arena.init(allocator,expected.len,ArenaType.Test),
        };
    }
    fn deinit(self: *Self) void {
        self.heap.deinit();
        testing.allocator.free(self.output);
        self.* = undefined;
        //testing.allocator.deinit();
    }
    inline fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize) !heap.HeapPtr {
        return self.heap.allocObject(classIndex, iv_size);
    }
    inline fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, fill: anytype) !heap.HeapPtr {
        return self.heap.allocRaw(classIndex, array_size, fill);
    }
    inline fn alloc(self : *Self, classIndex : class.ClassIndex, format: heap.Format, iv_size : usize, array_size : usize, fill: anytype) !heap.HeapPtr {
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
test "simple object allocator" {
    const h = heap.header(3,heap.Format.object,42,0);
    const expected = [_]Object{h.asObject(),object.True,object.Nil,object.False};
    var testArena = try TestArena.init(expected[0..]);
    defer testArena.deinit();
    const obj = try testArena.allocObject(42,3);
    const ivs = obj.instVars();
    ivs[0]=object.True;
    ivs[2]=object.False;
    try testArena.expectEqual();
}
