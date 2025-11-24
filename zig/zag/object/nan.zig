//! This module implements Object encoding with NaNs.
const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const NaN: u64 = @bitCast(std.math.nan(f64));
const zag = @import("../zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const debugError = false;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const HeapHeader = heap.HeapHeader;
const Process = zag.Process;

const TagBaseType = u13;
pub const Tag = enum(TagBaseType) {
    ThunkReturnLocal = 0x7ff0 >> 3,
    ThunkReturnInstance,
    ThunkReturnSmallInteger,
    ThunkReturnImmediate,
    ThunkLocal,
    BlockAssignLocal,
    ThunkInstance,
    BlockAssignInstance,
    ThunkHeap,
    ThunkImmediate,
    Symbol,
    False,
    True,
    Character,
    Signature,
    heap,
    SmallInteger = 0xfff8 >> 3,
    _,
    // inline fn base(cg: Tag) u64 {
    //     return @as(u64, @intFromEnum(cg)) << 48;
    // }
    // inline fn tag(cg: Tag, low: u48) u64 {
    //     return cg.base() | low;
    // }
    inline fn u(cg: Tag) TagBaseType {
        return @intFromEnum(cg);
    }
    inline fn from(c: ClassIndex) Tag {
        const cls = @intFromEnum(c);
        assert(cls <= 15);
        return @enumFromInt(cls + @intFromEnum(Tag.ThunkReturnLocal) - 1);
    }
    inline fn class(t: Tag) ClassIndex {
        const tag = @intFromEnum(t);
        return @enumFromInt(tag - @intFromEnum(Tag.ThunkReturnLocal) + 1);
    }
    comptime {
        assert(class(from(.Signature)) == .Signature);
        assert(from(.ThunkReturnLocal) == .ThunkReturnLocal);
    }
};
pub const Object = packed struct(u64) {
    value: Largest,
    tag: Tag,
    const Largest = i51;
    pub const maxInt = 0x3_ffff_ffff_ffff;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 0);
    }
    pub inline fn Nil() Object {
        return oImm(.UndefinedObject, 0);
    }
    pub const tagged0: i64 = 0;
    pub const LowTagType = void;
    pub const lowTagSmallInteger = {};
    pub const HighTagType = TagBaseType;
    pub const highTagSmallInteger: HighTagType = Tag.u(.SmallInteger);
    pub const PackedTagType = u3;
    pub const packedTagSmallInteger = 1;
    pub const intTag = @import("zag.zig").Object.intTag;
    pub const immediatesTag = 1;
    const TagAndClassType = u13;
    const tagAndClassBits = @bitSizeOf(Tag);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagBits = @bitSizeOf(Tag);
    const integerTag: Tag = Tag.SmallInteger;
    inline fn untagged(obj: Object) i64 {
        return @bitCast(@as(u64, @bitCast(obj)) << tagBits);
    }
    inline fn asI64(self: Object) i64 {
        return self.value;
    }
    pub inline fn isInt(self: Object) bool {
        return self.tag == integerTag;
    }
    inline fn toObject(int: i64) Object {
        return @bitCast(std.math.rotr(u64, @as(u64, @bitCast(int)) + @intFromEnum(integerTag), tagBits));
    }
    inline fn toObjectFromNative(int: i64) Object {
        return toObject(int << tagBits);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    pub fn rawU(self: Object) u64 {
        return @bitCast(self);
    }
    fn rawI(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn invalidObject(_: Object) ?u64 {
        // there are no invalid objects in this implementation
        return null;
    }
    pub inline fn asObject(self: Object) Object {
        return self;
    }
    pub inline fn extraI(self: Object) i3 {
        return @bitCast(@as(u3, @truncate(self.rawU())));
    }
    pub inline fn isImmediateClass(self: Object, comptime class: ClassIndex) bool {
        return self.tag == Tag.from(class);
    }
    inline fn oImm(c: ClassIndex, h: u32) Object {
        return .{ .tag = Tag.from(c), .value = h };
    }
    pub inline fn isSymbol(self: Object) bool {
        return self.tag == .Symbol;
    }
    const nonIndexSymbol = 0xffffffff800000ff;
    inline fn indexNumber(self: Object) u24 {
        return @truncate(self.rawU() & nonIndexSymbol >> 8);
    }
    pub inline fn withPrimitive(self: Object, prim: u64) Object {
        return @bitCast(self.rawU() | prim << 40);
    }
    pub inline fn makeImmediate(cls: ClassIndex, low32: u32) Object {
        return oImm(cls, low32);
    }
    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.rawU());
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.rawU());
    }
    pub inline fn symbolDirectHash(self: Object) u32 {
        return @truncate(self.rawU());
    }
    pub inline fn untaggedI(self: Object) ?i64 {
        if (self.isInt()) return self.untaggedI_noCheck();
        return null;
    }
    pub inline fn untaggedI_noCheck(self: Object) i64 {
        return self.untagged();
    }
    pub inline fn taggedI(self: Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }
    pub const taggedI_noCheck = untaggedI_noCheck;
    pub const fromTaggedI = fromUntaggedI;
    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) Object {
        return toObject(i);
    }
    pub inline fn symbol40(self: Object) u40 {
        return @as(u40, self.hash32()) << 8 | 1;
    }
    pub inline fn untaggedInt(self: Object) u64 {
        return self.toNatNoCheck();
    }
    pub inline fn nativeI(self: Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: Object) i64 {
        return self.asI64();
    }
    pub inline fn fromNativeI(t: Largest, _: anytype, _: anytype) Object {
        return toObjectFromNative(t);
    }
    pub inline fn nativeF(self: Object) ?f64 {
        if (self.isImmediateDouble()) return self.toDoubleNoCheck();
        return null;
    }
    pub inline fn isFloat(self: Object) bool {
        return self.isImmediateDouble();
    }
    pub inline fn nativeF_noCheck(self: Object) f64 {
        return self.toDoubleNoCheck();
    }
    pub inline fn fromNativeF(t: f64, _: anytype, _: anytype) Object {
        return @bitCast(t);
    }
    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isImmediateClass(.Symbol)) return @truncate(self.hash32());
        return null;
    }
    pub inline fn heapObject(self: Object) ?*zag.InMemory.PointedObject {
        return pointer(self,*zag.InMemory.PointedObject);
    }
    pub inline fn extraValue(self: Object) Object {
        _ = self;
        @panic("not implemented");
    }
    pub inline fn selectorEquals(self: Object, other: Object) bool {
        //        return (self.rawU()^other.rawU())&0xffffffffffff == 0; // may be false positive
        return self.rawU() == other.rawU();
    }
    inline fn isNaN(self: Object) bool {
        return std.math.isNan(@as(f64, @bitCast(self)));
    }
    pub inline fn isNat(self: Object) bool {
        if (self.untaggedI()) |value| return value >= 0;
        return false;
    }
    inline fn isImmediateDouble(self: Object) bool {
        return !std.math.isNan(@as(f64, @bitCast(self))) or self.rawU() == NaN;
    }
    pub inline fn highPointer(self: Object, T: type) ?T {
        return @ptrFromInt(self.rawU() & 0xFFFF_FFFF_FFF8);
    }
    pub inline fn pointer(self: Object, T: type) ?T {
        switch (self.tag) {
            .heap => {@branchHint(.likely);
                return self.highPointer(T);
            },
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkLocal, .ThunkInstance, .ThunkHeap => return self.highPointer(T),
            else => {},
        }
        return null;
    }

    pub inline fn isDouble(self: Object) bool {
        return self.isMemoryDouble();
    }
    pub inline fn isMemoryAllocated(self: Object) bool {
        return switch (self.tag) {
            .heap, .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkLocal, .ThunkInstance, .ThunkHeap => true,
            else => false,
        };
    }
    pub inline fn isBlock(self: Object) bool {
        return switch (self.tag) {
            .heapThunk, .nonLocalThunk => true,
            .immediates => switch (self.classIndex) {
                .ThunkSmallInteger, .ThunkFloat, .ThunkImmediate, .ThunkHeap, .ThunkReturnSelf, .ThunkReturnTrue, .ThunkReturnFalse, .ThunkReturnNil, .ThunkReturn_1, .ThunkReturn0, .ThunkReturn1, .ThunkReturn2 => true,
                else => false,
            },
            else => false,
        };
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self == True();
    }
    pub inline fn toDoubleNoCheck(self: Object) f64 {
        return @bitCast(self);
    }
    pub inline fn fromAddress(value: anytype) Object {
        return .{ .tag = .heap, .value = @as(u48, @truncate(@intFromPtr(value)))};
    }
    pub const StaticObject = struct {
        pub fn init(_: *StaticObject, comptime value: anytype) Object {
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromNativeI(value, null, null),
                .comptime_float => return fromNativeF(value, null, null),
                .bool => return if (value) Object.True() else Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            }
        }
    };
    pub inline fn from(value: anytype, _: anytype, _: anytype) Object {
    //     return fromWithError(value) catch @panic("unreachable");
    // }
    // pub inline fn fromWithError(value: anytype) !Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return fromNativeI(@truncate(value), null, null),
            .float, .comptime_float => return fromNativeF(value, null, null),
            .bool => return if (value) Object.True() else Object.False(),
            .null => return Object.Nil(),
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .one, .many => return fromAddress(value),
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
    }
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (!check or self.isImmediateDouble()) return self.toDoubleNoCheck();
            },
            i64 => {
                if (!check or self.isInt()) return self.nativeI_noCheck();
            },
            bool => {
                if (!check or self.isBool()) return self.toBoolNoCheck();
            },
            object.PackedObject => {
                if (!check or self.isInt()) return @as(T, @bitCast(self));
            },

            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                switch (@typeInfo(T)) {
                    .pointer => |ptrInfo| {
                        switch (@typeInfo(ptrInfo.child)) {
                            .@"fn" => {},
                            .@"struct" => {
                                if (!check or (self.isMemoryAllocated() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.to(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
                                    if (@hasField(ptrInfo.child, "header") or (@hasDecl(ptrInfo.child, "includesHeader") and ptrInfo.child.includesHeader)) {
                                        return @as(T, @ptrFromInt(self.nanPointerAsInt()));
                                    } else {
                                        return @as(T, @ptrFromInt(@sizeOf(HeapHeader) + self.nanPointerAsInt()));
                                    }
                                }
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
            },
        }
        @panic("Trying to convert Object to " ++ @typeName(T));
    }
    inline fn nanPointerAsInt(self: Object) usize {
        return @as(u48, @truncate(@as(usize, @bitCast(self))));
    }
    pub inline fn which_class(self: Object) ClassIndex {
        const Choose = enum {
            tagCompare,
            nanCompare,
            nanSigned,
            fullCompare,
            bigSwitch,
            nanSwitch,
        };
        switch (Choose.nanSigned) {
            .fullCompare => {
                const u = Tag.u;
                const base = Tag.base;
                const full = self.rawU();
                if (full >= base(.smallInteger)) {@branchHint(.likely);
                    return .SmallInteger;
                } else if (full <= base(.heap)) {@branchHint(.likely);
                    return .Float;
                } else {
                    const tagEnum = self.tag;
                    const tag = tagEnum.u();
                    if (tag == u(.heap)) {@branchHint(.likely);
                        return self.toUnchecked(HeapObjectPtr).*.getClass();
                    } else if (tag == u(.immediates)) {@branchHint(.likely);
                        return self.classIndex;
                    } else switch (tagEnum) {
                        .thunkReturnLocal => {@branchHint(.unlikely); return .ThunkReturnLocal;},
                        .thunkReturnInstance => {@branchHint(.unlikely); return .ThunkReturnInstance;},
                        .thunkReturnSmallInteger => {@branchHint(.unlikely); return .ThunkReturnSmallInteger;},
                        .thunkReturnImmediate => {@branchHint(.unlikely); return .ThunkReturnImmediate;},
                        .thunkLocal => {@branchHint(.unlikely); return .ThunkLocal;},
                        .thunkInstance => {@branchHint(.unlikely); return .ThunkInstance;},
                        .thunkHeap => {@branchHint(.unlikely); return .ThunkHeap;},
                        else => @panic("Unknown tag"),
                    }
                }
            },
            .tagCompare => {
                const tagEnum = self.tag;
                const tag = tagEnum.u();
                const u = Tag.u;
                if (tag >= u(.smallInteger)) {@branchHint(.likely);
                    return .SmallInteger;
                } else if (tag < u(.heap)) {@branchHint(.likely);
                    return .Float;
                } else if (tag == u(.heap)) {@branchHint(.likely);
                    if (self.rawU() == Tag.base(.heap)) {@branchHint(.unlikely);
                        return .Float;
                    }
                    return self.toUnchecked(HeapObjectPtr).*.getClass();
                } else if (tag == u(.immediates)) {@branchHint(.likely);
                    return self.classIndex;
                } else  switch (tagEnum) {
                    .thunkReturnLocal => {@branchHint(.unlikely); return .ThunkReturnLocal;},
                    .thunkReturnInstance => {@branchHint(.unlikely); return .ThunkReturnInstance;},
                    .thunkReturnSmallInteger => {@branchHint(.unlikely); return .ThunkReturnSmallInteger;},
                    .thunkReturnImmediate => {@branchHint(.unlikely); return .ThunkReturnImmediate;},
                    .thunkLocal => {@branchHint(.unlikely); return .ThunkLocal;},
                    .thunkInstance => {@branchHint(.unlikely); return .ThunkInstance;},
                    .thunkHeap => {@branchHint(.unlikely); return .ThunkHeap;},
                    else => {
                        @panic("Unknown tag");
                    }
                }
            },
            .nanCompare => {
                const tagEnum = self.tag;
                const tag = tagEnum.u();
                const u = Tag.u;
                if (tag >= u(.smallInteger)) {@branchHint(.likely);
                    return .SmallInteger;
                } else if (!std.math.isNan(@as(f64, @bitCast(self)))) {@branchHint(.likely);
                    return .Float;
                } else if (tag == u(.heap)) {@branchHint(.likely);
                    return self.toUnchecked(HeapObjectPtr).*.getClass();
                } else if (tag == u(.immediates)) {@branchHint(.likely);
                    return self.classIndex;
                } else  switch (tagEnum) {
                    .nan => {@branchHint(.unlikely); return .Float;},
                    .thunkReturnLocal => {@branchHint(.unlikely); return .ThunkReturnLocal;},
                    .thunkReturnInstance => {@branchHint(.unlikely); return .ThunkReturnInstance;},
                    .thunkReturnSmallInteger => {@branchHint(.unlikely); return .ThunkReturnSmallInteger;},
                    .thunkReturnImmediate => {@branchHint(.unlikely); return .ThunkReturnImmediate;},
                    .thunkLocal => {@branchHint(.unlikely); return .ThunkLocal;},
                    .thunkInstance => {@branchHint(.unlikely); return .ThunkInstance;},
                    .thunkHeap => {@branchHint(.unlikely); return .ThunkHeap;},
                    else => {
                        @panic("Unknown tag");
                    }
                }
            },
            .nanSigned => {
                if (@as(i64, @bitCast(self)) < 0) {@branchHint(.likely);
                    if (std.math.isNan(@as(f64, @bitCast(self)))) {@branchHint(.likely);
                        return .SmallInteger;
                    }
                    return .Float;
                }
                if (!std.math.isNan(@as(f64, @bitCast(self)))) {@branchHint(.likely);
                    return .Float;
                }
                const tag = self.tag;
                if (tag == .heap) {@branchHint(.likely);
                    return self.toUnchecked(HeapObjectPtr).*.getClass();
                }
                if (self.rawU() == NaN) {@branchHint(.unlikely); return .Float;}
                return tag.class();
            },
            .nanSwitch => {
                if (@as(u64, @bitCast(self)) >= Tag.base(.smallInteger)) {@branchHint(.likely);
                    return .SmallInteger;
                } else if (!std.math.isNan(@as(f64, @bitCast(self)))) {@branchHint(.likely);
                    return .Float;
                } else switch (self.tag) {
                    .heap => {@branchHint(.likely); return self.toUnchecked(HeapObjectPtr).*.getClass();},
                    .immediates => {@branchHint(.likely); return self.classIndex;},
                    .nan => {@branchHint(.unlikely); return .Float;},
                    .thunkReturnLocal => {@branchHint(.unlikely); return .ThunkReturnLocal;},
                    .thunkReturnInstance => {@branchHint(.unlikely); return .ThunkReturnInstance;},
                    .thunkReturnSmallInteger => {@branchHint(.unlikely); return .ThunkReturnSmallInteger;},
                    .thunkReturnImmediate => {@branchHint(.unlikely); return .ThunkReturnImmediate;},
                    .thunkLocal => {@branchHint(.unlikely); return .ThunkLocal;},
                    .thunkInstance => {@branchHint(.unlikely); return .ThunkInstance;},
                    .thunkHeap => {@branchHint(.unlikely); return .ThunkHeap;},
                    else => {
                        @panic("Unknown tag");
                    }
                }
            },
            .bigSwitch => {
                switch (self.tag) {
                    .smallInteger => {@branchHint(.likely); return .SmallInteger;},
                    .immediates => {@branchHint(.likely); return self.classIndex;},
                    .smallInteger_1,
                    .smallInteger_2,
                    .smallInteger_3 => {@branchHint(.unlikely); return .SmallInteger;},
                    .thunkReturnLocal => {@branchHint(.unlikely); return .ThunkReturnLocal;},
                    .thunkReturnInstance => {@branchHint(.unlikely); return .ThunkReturnInstance;},
                    .thunkReturnSmallInteger => {@branchHint(.unlikely); return .ThunkReturnSmallInteger;},
                    .thunkReturnImmediate => {@branchHint(.unlikely); return .ThunkReturnImmediate;},
                    .thunkLocal => {@branchHint(.unlikely); return .ThunkLocal;},
                    .thunkInstance => {@branchHint(.unlikely); return .ThunkInstance;},
                    .thunkHeap => {@branchHint(.unlikely); return .ThunkHeap;},
                    .picPointer => {@branchHint(.unlikely); @panic("nonLocalThunk");},
                    .heap => return self.toUnchecked(HeapObjectPtr).*.getClass(),
                    else => {@branchHint(.likely); return .Float;},
                }
            },
        }
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.tag == .heap;
    }
    const OF = object.ObjectFunctions;
    pub const arrayAsSlice = OF.arrayAsSlice;
    pub const asMemoryObject = OF.asMemoryObject;
    pub const asObjectArray = OF.asObjectArray;
    pub const asZeroTerminatedString = OF.asZeroTerminatedString;
    pub const compare = OF.compare;
    pub const empty = OF.empty;
    pub const equals = OF.equals;
    pub const format = OF.format;
    pub const getField = OF.getField;
    pub const get_class = OF.get_class;
    pub const isBool = OF.isBool;
    pub const isIndexable = OF.isIndexable;
    pub const isNil = OF.isNil;
    pub const isUnmoving = OF.isUnmoving;
    pub const numArgs = OF.numArgs;
    pub const promoteToUnmovable = OF.promoteToUnmovable;
    pub const rawFromU = OF.rawFromU;
    pub const setField = OF.setField;
    pub const to = OF.to;
    pub const toUnchecked = OF.toUnchecked;
    pub const asVariable = zag.Context.asVariable;
    pub const PackedObject = object.PackedObject;
    pub const primitive = @import("zag.zig").Object.primitive;
    pub const symbol = @import("zag.zig").Object.symbol;
    pub const signature = zag.execute.Signature.signature;
    pub const tests = OF.tests;
};
test "all generated NaNs are positive" {
    // test that all things that generate NaN generate positive ones
    // otherwise we'd need to check in any primitive that could create a NaN
    // because a negative one could look like one of our tags (in particular a large positive SmallInteger)
    const inf = @as(f64, 1.0) / 0.0;
    const zero = @as(f64, 0);
    const one = @as(f64, 1);
    const fns = struct {
        const nanU: u64 = @bitCast(math.nan(f64));
        fn d(x: anytype) !void {
            try std.testing.expect(Object.from(x, null, null).isDouble());
        }
        fn v(x: anytype) !void {
            try d(x);
            try std.testing.expectEqual(nanU, @as(u64, @bitCast(x)));
        }
    };
    const d = fns.d;
    const v = fns.v;
    try v(@sqrt(-one));
    try v(@log(-one));
    try v(zero / zero);
    try v((-inf) * 0.0);
    try d((-inf) * inf);
    try v((-inf) + inf);
    try v(inf - inf);
    try v(inf * 0.0);
    try v(std.math.nan(f64));
}
test "test of test-all-encodings" {}
