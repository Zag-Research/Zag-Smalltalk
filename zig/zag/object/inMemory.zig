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
const compileRaw = zag.execute.compileRaw;
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
const SmallIntegerCache = if (config.immediateIntegers)
{} else compileRaw(.{
    si, -5,
    si, -4,
    si, -3,
    si, -2,
    si, -1,
    si, 0,
    si, 1,
    si, 2,
    si, 3,
    si, 4,
    si, 5,
    si, 6,
    si, 7,
    si, 8,
    si, 9,
    si, 10,
    si, 11,
    si, 12,
    si, 13,
    si, 14,
    si, 15,
    si, 16,
    si, 17,
    si, 18,
    si, 19,
    si, 20,
    si, 21,
    si, 22,
    si, 23,
    si, 24,
    si, 25,
    si, 26,
    si, 27,
    si, 28,
    si, 29,
    si, 30,
    si, 31,
    si, 32,
    si, 33,
    si, 34,
    si, 35,
    si, 36,
    si, 37,
    si, 38,
    si, 39,
    si, 40,
    si, 41,
    si, 42,
    si, 43,
    si, 44,
    si, 45,
    si, 46,
    si, 47,
    si, 48,
    si, 49,
    si, 50,
    si, 51,
    si, 52,
    si, 53,
    si, 54,
    si, 55,
    si, 56,
    si, 57,
    si, 58,
    si, 59,
    si, 60,
    si, 61,
    si, 62,
    si, 63,
    si, 64,
    si, 65,
    si, 66,
    si, 67,
    si, 68,
    si, 69,
    si, 70,
    si, 71,
    si, 72,
    si, 73,
    si, 74,
    si, 75,
    si, 76,
    si, 77,
    si, 78,
    si, 79,
    si, 80,
    si, 81,
    si, 82,
    si, 83,
    si, 84,
    si, 85,
    si, 86,
    si, 87,
    si, 88,
    si, 89,
    si, 90,
    si, 91,
    si, 92,
    si, 93,
    si, 94,
    si, 95,
    si, 96,
    si, 97,
    si, 98,
    si, 99,
    si, 100,
});
const SICacheMin = -5;
const SICacheMax = 100;
const SICache = switch (objectEncoding) {
    .cachedPtr, .taggedPtr => true,
    else => false,
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
            .int, .comptime_int => self.data.int = value,
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
    if (SICache and SICacheMin <= i and i <= SICacheMax)
        return Object.from(&SmallIntegerCache.objects[(@as(usize, @bitCast(i - SICacheMin))) << 1], null);
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
