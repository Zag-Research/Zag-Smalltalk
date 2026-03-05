const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const trace = config.trace;
const objectEncoding = config.objectEncoding;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const object = zag.object;
const Object = object.Object;
const heap = zag.heap;
const Age = heap.Age;
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const c = zag.object.ClassIndex;
const si = c.SmallInteger;
pub const ZERO = PointedObject{
    .header = .{ .classIndex = si },
    .data = .{ .int = 0 },
};
pub const False = PointedObject{
    .header = .{ .classIndex = .False },
    .data = .{ .int = 0 },
};
pub const True = PointedObject{
    .header = .{ .classIndex = .True },
    .data = .{ .int = 1 },
};
pub const Nil = PointedObject{
    .header = .{ .classIndex = .UndefinedObject },
    .data = .{ .int = 0 },
};
const SmallIntegerCache = struct {
    const min: i64 = -5;
    const max: i64 = 100;
    const len: usize = @intCast(max - min + 1);
    var initialized = false;
    var objects: [len]PointedObject = undefined;

    inline fn enabled() bool {
        return !config.immediateIntegers and objectEncoding == .cachedPtr;
    }
    inline fn inRange(i: i64) bool {
        return min <= i and i <= max;
    }
    fn ensure() void {
        if (!enabled() or initialized) return;
        for (0..len) |n| {
            const v = min + @as(i64, @intCast(n));
            _ = objects[n].set(.SmallInteger, v);
        }
        initialized = true;
    }
    inline fn at(i: i64) *PointedObject {
        ensure();
        return &objects[@as(usize, @intCast(i - min))];
    }
};
pub const PointedObject = packed struct {
    header: HeapHeader,
    data: PointedObjectUnion,
    const PointedObjectUnion = packed union {
        int: i64,
        unsigned: u64,
        float: f64,
        boolean: bool,
        nil: void,
        character: u24,
        object: Object,
        objects: ?[*]Object,
    };
    const staticCacheSize = 0;
    var staticCacheUsed: usize = 0;
    var staticCache = [_]PointedObject{.{ .header = .{}, .data = undefined }} ** staticCacheSize;
    pub fn format(
        self: PointedObject,
        writer: anytype,
    ) !void {
        try writer.print("PointedObject{{ {f}, 0x{x}}}", .{ self.header, @as(u64, @bitCast(self.data)) });
    }
    pub fn getClass(self: *PointedObject) object.ClassIndex {
        return self.header.classIndex;
    }
    fn cached(self: PointedObject) ?*PointedObject {
        for (staticCache[0..]) |*p| {
            if (p.header.classIndex == self.header.classIndex and
                p.data.unsigned == self.data.unsigned)
                return p;
        }
        if (staticCacheUsed < staticCacheSize) {
            const p = &staticCache[staticCacheUsed];
            staticCacheUsed += 1;
            p.setHeader(self.header.classIndex, self.header.hash);
            p.data = self.data;
            return p;
        }
        return null;
    }
    pub fn setHeader(self: *PointedObject, classIndex: object.ClassIndex, hash: u24) void {
        self.header = .{
            .classIndex = classIndex,
            .hash = hash,
            .objectFormat = .notIndexable,
            .age = .static,
            .length = 1,
        };
    }
    pub fn set(self: *PointedObject, classIndex: object.ClassIndex, value: anytype) *PointedObject {
        const hash = @as(u64, @bitCast(switch (@typeInfo(@TypeOf(value))) {
            .comptime_int => @as(i64, value),
            .comptime_float => @as(f64, value),
            else => value,
        }));
        self.header = .{
            .classIndex = classIndex,
            .hash = @truncate(hash | hash >> 24 | hash >> 48),
            .objectFormat = .notIndexable,
            .age = .static,
            .length = 1,
        };
        switch (@typeInfo(@TypeOf(value))) {
            .int => self.data.int = @bitCast(value),
            .comptime_int => self.data.int = value,
            .float, .comptime_float => self.data.float = value,
            .bool => self.data.bool = value,
            .null => self.data.null = value,
            else => @panic("Unsupported type for static set"),
        }
        return self;
    }
};
pub const PointedObjectRef = packed struct {
    ref: *PointedObject,
};
pub inline fn int(i: i64, sp: SP, context: *Context) Object {
    if (SmallIntegerCache.enabled() and SmallIntegerCache.inRange(i)) {
        return Object.fromAddress(SmallIntegerCache.at(i));
    }
    const allocResult = sp.alloc(context, .SmallInteger, 1, null, Object, false);
    allocResult.allocated.array(i64)[1] = i;
    allocResult.allocated.header.hash = @truncate(@as(u64, @bitCast(i)));
    return allocResult.allocated.asObject();
}
test "inMemory int()" {
    if (config.immediateIntegers) return error.SkipZigTest;
    const ee = std.testing.expectEqual;
    trace("inMemory int()", .{});
    var process: Process align(Process.alignment) = undefined;
    process.init();
    const sp = process.getSp();
    const context = process.getContext();
    const one_ = int(1, sp, context);
    const one: PointedObjectRef = @bitCast(one_);
    trace("one: {}", .{one});
    SmallIntegerCache.ensure();
    for (&SmallIntegerCache.objects, 0..) |*o, i| trace("[{}](0x{x:0>4}): 0x{x:0>16}", .{ i, @intFromPtr(o), @as(u64, @bitCast(o.*)) });
    try ee(.SmallInteger, one.ref.header.classIndex);
    try ee(1, one.ref.data.int);
    try ee(one_, int(1, sp, context));
    try ee(int(42, sp, context), int(42, sp, context));
}

pub const MemoryFloat = struct {
    header: HeapHeader,
    value: f64,
    fn static(v: f64) MemoryFloat {
        var mf: MemoryFloat = undefined;
        mf.init(v, .static);
        return mf;
    }
    pub inline fn init(self: *MemoryFloat, v: f64, age: Age) void {
        const u: u64 = @bitCast(v);
        const hash: u24 = @truncate(u ^ (u >> 40));
        self.header = .{ .classIndex = .Float, .hash = hash, .objectFormat = .notIndexable, .age = age, .length = 1 };
        self.value = v;
    }
};
const static = MemoryFloat.static;
pub const nanMemObject = static(std.math.nan(f64));
pub const pInfMemObject = static(std.math.inf(f64));
pub const nInfMemObject = static(-std.math.inf(f64));
pub const fZero = static(0.0);
pub const fOne = static(1.0);

const FCache = switch (objectEncoding) {
    .cachedPtr, .taggedPtr => true,
    else => false,
};
pub inline fn float(v: f64, sp: SP, context: *Context) Object {
    if (std.math.isNan(v))
        return Object.fromAddress(&nanMemObject);
    if (std.math.isInf(v)) {
        if (v > 0)
            return Object.fromAddress(&pInfMemObject);
        return Object.fromAddress(&nInfMemObject);
    }
    if (FCache) {
        if (v == 0.0)
            return Object.fromAddress(&fZero);
        if (v == 1.0)
            return Object.fromAddress(&fOne);
    }
    const allocResult = sp.alloc(context, .Float, 1, null, Object, false);
    allocResult.allocated.array(f64)[1] = v;
    return allocResult.allocated.asObject();
}
