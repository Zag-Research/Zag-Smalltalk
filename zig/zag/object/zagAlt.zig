const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const trace = config.trace;
const assert = std.debug.assert;
const debugError = false;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const InMemory = zag.InMemory;
const encode = @import("floatSpur.zig").encode;
const decode = @import("floatSpur.zig").decode;
pub const Object = packed struct(u64) {
    tag: Group,
    class: ClassIndex.Compact,
    hash: u56,
    pub const Group = enum(u3) {
        heap = 0,
        int = 1,
        immediates = 2,
        float = 4,
        _,
        inline fn u(cg: Group) u3 {
            return @intFromEnum(cg);
        }
        inline fn isSet(bits: u64, mask: Group) bool {
            return (bits & @intFromEnum(mask)) != 0;
        }
    };
    const Self = @This();
    pub const maxInt = 0x7f_ffff_ffff_ffff;
    pub const ZERO = of(0);
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 0);
    }
    pub inline fn Nil() Object {
        return Self{ .tag = .heap, .class = .none, .hash = 0 };
    }
    pub const tagged0: i64 = Group.u(.int);
    pub const LowTagType = TagAndClassType;
    pub const lowTagSmallInteger = Group.u(.int);
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
    pub const PackedTagType = u8;
    pub const packedTagSmallInteger = intTag;
    pub const intTag = Group.u(.int);
    pub const immediatesTag = Group.u(.immediates);
    const TagAndClassType = u8;
    const tagBits = enumBits(Group);
    const tagAndClassBits = enumBits(Group) + enumBits(ClassIndex.Compact);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagAndClass = (@as(u64, 1) << tagAndClassBits) - 1;
    const extraMask = 0xff;
    const ExtraType = u8;
    pub inline fn tagbits(self: Self) TagAndClassType {
        return @truncate(self.rawU());
    }
    fn enumBits(T: type) usize {
        return @typeInfo(@typeInfo(T).@"enum".tag_type).int.bits;
    }
    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return untaggedI_noCheck(self);
        return null;
    }
    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self.rawU() - Group.u(.int));
    }
    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }
    pub inline fn taggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self);
    }
    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i);
    }
    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i + Group.u(.int));
    }
    // pub inline fn cast(v: anytype) object.Object {
    //     // stored using little-endian order
    //     return @bitCast(v);
    // }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.rawU());
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: object.Object) i64 {
        return self.rawI() >> tagBits;
    }
    pub inline fn nativeU(self: object.Object) ?u64 {
        if (self.isInt()) return self.nativeU_noCheck();
        return null;
    }
    inline fn nativeU_noCheck(self: object.Object) u64 {
        return self.rawU() >> tagBits;
    }
    pub inline fn nativeF(self: object.Object) ?f64 {
        if (self.isImmediateDouble()) return self.toDoubleNoCheck();
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn isFloat(self: object.Object) bool {
        return self.isImmediateDouble() or self.isMemoryDouble();
    }
    pub inline fn nativeF_noCheck(self: object.Object) f64 {
        if (self.isHeap()) return self.toDoubleFromMemory();
        return self.toDoubleNoCheck();
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return from(t, sp, context);
    }
    pub inline fn symbolHash(self: object.Object) ?u24 {
        if (self.isImmediateClass(.Symbol)) return @truncate(self.hash);
        return null;
    }
    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(self.nativeI_noCheck() >> 8);
    }
    pub inline fn withPrimitive(self: Self, prim: u64) object.Object {
        return @bitCast(self.rawU() | prim << 40);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    pub inline fn rawU(self: Self) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: object.Object) i64 {
        return @bitCast(self);
    }
    inline fn of(comptime v: u64) object.Object {
        return @bitCast(v);
    }
    pub inline fn makeThunk(class: ClassIndex.Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @truncate((@intFromPtr(obj) << 8) | tag));
    }
    pub inline fn makeThunkNoArg(class: ClassIndex.Compact, value: u56) Object {
        return .oImm(class, value);
    }
    inline fn thunkImmediate(o: object.Object) ?object.Object {
        const value: i64 = @bitCast(o);
        const shifted = value >> 55;
        if (shifted == 0 or shifted == -1)
            return oImm(.ThunkImmediate, @bitCast(@as(i56, @truncate(value))));
        return null;
    }
    inline fn thunkImmediateValue(self: Self) object.Object {
        return @bitCast(self.rawI() >> 8);
    }
    inline fn isThunkImmediate(self: object.Object) bool {
        return self.isImmediateClass(.ThunkImmediate);
    }
    pub inline fn tagMethod(o: object.Object) ?object.Object {
        return @bitCast(@as(u64, @bitCast(o)) | 1);
    }
    pub inline fn tagMethodValue(self: Self) object.Object {
        return @bitCast(@as(u64, @bitCast(self)) >> 1 << 1);
    }
    pub inline fn isTaggedMethod(self: object.Object) bool {
        return (@as(u64, @bitCast(self)) & 1) != 0;
    }
    pub inline fn extraI(self: object.Object) i8 {
        return @bitCast(@as(u8, @truncate(self.hash & extraMask)));
    }
    test "ThunkImmediate" {
        trace("Test: ThunkImmediate\n", .{});
        const ee = std.testing.expectEqual;
        if (thunkImmediate(object.Object.tests[0])) |value|
            try ee(object.Object.tests[0], value.thunkImmediateValue());
        if (thunkImmediate(object.Object.from(-42, undefined, undefined))) |value|
            try ee(object.Object.from(-42, undefined, undefined), value.thunkImmediateValue());
        try ee(null, thunkImmediate(object.Object.from(@as(u64, 1) << 47, undefined, undefined)));
    }
    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex.Compact) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
    }
    pub inline fn isHeap(self: object.Object) bool {
        return self.tag == .heap;
    }
    pub inline fn isImmediateDouble(self: object.Object) bool {
        return Group.isSet(self.rawU(),.float);
    }
    pub inline fn isMemoryDouble(self: object.Object) bool {
        return self.isMemoryAllocated() and self.to(HeapObjectPtr).*.getClass() == .Float;
    }
    inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    inline fn g(grp: Group) u64 {
        return grp.base();
    }
    pub inline fn isInt(self: object.Object) bool {
        return Group.isSet(self.rawU(),.int);
    }
    pub inline fn isNat(self: object.Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn hasPointer(self: object.Object) bool {
        const bits = math.rotr(TagAndClassType, self.tagbits(), 3);
        return bits <= math.rotr(TagAndClassType, oImm(.ThunkHeap, 0).tagbits(), 3) and bits != 0;
    }
    pub inline fn highPointer(self: object.Object, T: type) ?T {
        return @ptrFromInt(self.rawU() >> 16);
    }
    pub inline fn pointer(self: object.Object, T: type) ?T {
        switch (self.tag) {
            .heap => return @ptrFromInt(self.rawU()),
            .immediates => switch (self.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance => return self.highPointer(T),
                else => {},
            },
            else => {},
        }
        return null;
    }
    pub inline fn toBoolNoCheck(self: object.Object) bool {
        return self.rawU() == object.Object.True().rawU();
    }
    pub inline fn withClass(self: object.Object, class: ClassIndex) object.Object {
        if (!self.isSymbol()) std.debug.panic("not a Symbol: {f}", self);
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }
    pub inline fn rawWordAddress(self: object.Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    pub inline fn toDoubleNoCheck(self: object.Object) f64 {
        return decode(@bitCast(self));
    }
    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u56) object.Object {
        return oImm(cls, hash);
    }
    pub inline fn hash24(self: object.Object) u24 {
        return @truncate(self.hash);
    }
    pub inline fn hash32(self: object.Object) u32 {
        return @truncate(self.hash);
    }

    fn memoryFloat(value: f64, sp: SP, context: *Context) object.Object {
        if (math.isNan(value)) return object.Object.fromAddress(&InMemory.nanMemObject);
        if (math.inf(f64) == value) return object.Object.fromAddress(&InMemory.pInfMemObject);
        if (math.inf(f64) == -value) return object.Object.fromAddress(&InMemory.nInfMemObject);
        return InMemory.float(value, sp, context);
    }

    pub fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub inline fn from(value: anytype, sp: SP, context: *Context) object.Object {
        const T = @TypeOf(value);
        if (T == object.Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return fromUntaggedI(value << tagBits, sp, context),
            .float => return @bitCast(encode(value) catch {
                return memoryFloat(value, sp, context);
            }),
            .comptime_float => return from(@as(f64, value), sp, context),
            .bool => return if (value) object.Object.True() else object.Object.False(),
            .null => return object.Object.Nil(),
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .one, .many => {
                        return @bitCast(@intFromPtr(value));
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
    }
    pub fn toWithCheck(self: object.Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (!check or self.isImmediateDouble()) return self.toDoubleNoCheck();
                if (!check or self.isMemoryDouble()) return self.toDoubleFromMemory();
            },
            i64 => {
                if (!check or self.isInt()) return self.nativeI_noCheck();
            },
            u64 => {
                if (!check or self.isNat()) return self.nativeU_noCheck();
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
                                        return @as(T, @ptrFromInt(@as(usize, @bitCast(self))));
                                    } else {
                                        return @as(T, @ptrFromInt(@sizeOf(HeapHeader) + (@as(usize, @bitCast(self)))));
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
    pub inline fn which_class(self: object.Object) ClassIndex {
        const bits: u64 = @bitCast(self);
        if (Group.isSet(bits,.int)) {@branchHint(.likely);
            return .SmallInteger;
        } else if (Group.isSet(bits,.float)) {@branchHint(.likely);
            return .Float;
        } else if (Group.isSet(bits,.immediates)) {@branchHint(.unlikely);
            return self.class.classIndex();
        } else if (bits == 0) {@branchHint(.unlikely);
            return .UndefinedObject;
        } else {@branchHint(.likely);
            return self.to(HeapObjectPtr).*.getClass();
        }
    }
    pub inline fn isMemoryAllocated(self: object.Object) bool {
        return if (self.isHeap()) self != object.Object.Nil() else @intFromEnum(self.class) <= @intFromEnum(ClassIndex.Compact.ThunkHeap);
    }
    pub const Special = packed struct {
        imm: TagAndClassType,
        tag: u8,
        rest: u48,
        pub fn ptr(self: Special) *object.Object {
            return @ptrFromInt(self.rest);
        }
        pub fn objectFrom(tact: TagAndClassType, tag: u8, p: *opaque {}) object.Object {
            return @bitCast(Special{ .imm = tact, .tag = tag, .rest = @truncate(@intFromPtr(p)) });
        }
    };
    pub inline fn rawSpecial(self: object.Object) Special {
        return @bitCast(self);
    }
    pub const Scanner = struct {
        ptr: *anyopaque,
        vtable: *const VTable,
        pub const VTable = struct {
            simple: *const fn (ctx: *anyopaque, obj: object.Object) void = noSimple,
        };
        pub inline fn simple(self: Scanner, obj: object.Object) void {
            return self.vtable.simple(self.ptr, obj);
        }
        fn noSimple(ctx: *anyopaque, obj: object.Object) void {
            _ = .{ ctx, obj };
        }
    };
    pub inline fn isSymbol(self: object.Object) bool {
        return self.tagbits() == comptime makeImmediate(.Symbol, 0).tagbits();
    }
    pub inline fn isImmediate(self: object.Object) bool {
        return self.tag == .immediates;
    }
    pub inline fn isHeapObject(self: object.Object) bool {
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
    pub const header = OF.header;
    pub const asVariable = zag.Context.asVariable;
    pub const PackedObject = object.PackedObject;
    pub const signature = zag.execute.Signature.signature;
    pub const tests = OF.tests;
};
