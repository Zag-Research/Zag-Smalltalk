//! This module implements Object encoding for ZagOrig encoding
const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const assert = std.debug.assert;
const math = std.math;
const zag = @import("../zag.zig");
const trace = zag.config.trace;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const Compact = ClassIndex.Compact;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const HeapHeader = zag.heap.HeapHeader;
const HeapObject = zag.heap.HeapObject;
const HeapObjectConstPtr = zag.heap.HeapObjectConstPtr;
const InMemory = zag.InMemory;
const execute = zag.execute;
const Signature = execute.Signature;
const floatEncoding = @import("floatEncoding.zig").Fst2;
const encode = floatEncoding.encode;
const decode = floatEncoding.decode;

pub const Tag = enum(u3) {
    heap = 0,
    immediates,
    float2,
    float3,
    float4,
    float5,
    float6,
    float7,
    inline fn u(cg: Tag) u3 {
        return @intFromEnum(cg);
    }
};
pub const Object = packed struct(u64) {
    tag: Tag,
    class: Compact,
    hash: u56,
    const Self = @This();
    pub const maxInt = 0x7f_ffff_ffff_ffff;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 0);
    }
    pub inline fn Nil() Object {
        return ZERO;
    }
    pub const LowTagType = TagAndClassType;
    pub const lowTagSmallInteger = makeImmediate(.SmallInteger, 0).tagbits();
    pub const HighTagType = u0;
    pub const highTagSmallInteger = 0;
    pub const PackedTagType = u8;
    pub const packedTagSmallInteger = oImm(.SmallInteger, 0).tagbits();
    pub const signatureTag = Tag.u(.immediates);
    const TagAndClassType = u8;
    const tagAndClassBits = @bitSizeOf(Tag) + @bitSizeOf(Compact);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagAndClassMask: u64 = (@as(u64, 1) << tagAndClassBits) - 1;
    const extraMask = 0xff;
    const TaggedClass = packed struct(u64) {
        tag: TagAndClassType,
        data: i56,
        inline fn from(obj: Object) TaggedClass {
            return @bitCast(obj);
        }
        inline fn dataI(self: TaggedClass) i64 {
            return self.data;
        }
    };
    inline fn tagbits(self: Self) TagAndClassType {
        return @truncate(self.rawU());
    }

    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            return @bitCast(self.rawU() >> tagAndClassBits << tagAndClassBits);
        }
        return null;
    }
    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i + oImm(.SmallInteger, 0).tagbits());
    }

    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            return @bitCast(self);
        }
        return null;
    }
    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i);
    }

    inline fn isInt(self: object.Object) bool {
        return self.isImmediateClass(.SmallInteger);
    }
    pub inline fn isNat(self: object.Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            return @as(i64, @bitCast(self)) >> @bitSizeOf(TagAndClassType);
        }
        return null;
    }
    pub inline fn fromNativeI(t: i56, _: anytype, _: anytype) Object {
        return oImm(.SmallInteger, @as(u56, @bitCast(t)));
    }
    pub inline fn nativeF(self: object.Object) ?f64 {
        if (decode(@bitCast(self))) |flt| return flt;
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn isFloat(self: object.Object) bool {
        return self.isImmediateDouble() or self.isMemoryDouble();
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return @bitCast(encode(t) catch {
            return InMemory.float(t, sp, context);
        });
    }
    pub inline fn symbolHash(self: object.Object) ?u24 {
        if (self.isSymbol()) return self.hash24();
        return null;
    }
    pub inline fn numArgs(self: Object) u4 {
        return @truncate(self.rawU() >> 32);
    }
    pub fn makeSymbol(class: Compact, hash: u24, arity: u4) Object {
        return makeImmediate(class, @as(u32, hash) | (@as(u32, arity) << 24));
    }
    pub inline fn isSymbol(self: object.Object) bool {
        return self.tagbits() == comptime makeImmediate(.Symbol, 0).tagbits();
    }

    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(@as(i64, @bitCast(self)) >> @bitSizeOf(TagAndClassType) >> 8);
    }
    pub inline fn highPointer(self: object.Object, T: type) ?T {
        return @ptrFromInt(self.rawU() >> 16);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    inline fn rawU(self: Self) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: object.Object) i64 {
        return @bitCast(self);
    }
    pub inline fn invalidObject(_: object.Object) ?u64 {
        // there are no invalid objects in this encoding
        return null;
    }
    pub inline fn isImmediateClass(self: object.Object, comptime class: Compact) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
    }
    inline fn isImmediateDouble(self: object.Object) bool {
        return (self.rawU() & 6) != 0;
    }
    inline fn isMemoryDouble(self: object.Object) bool {
        return if (self.ifHeapObject()) |ptr|
            ptr.getClass() == .Float
        else
            false;
    }
    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.toUnchecked(*InMemory.MemoryFloat).*.value;
    }
    inline fn oImm(c: Compact, h: u56) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    inline fn oImmContext(c: Compact, context: *Context, e: u8) Self {
        return Self{ .tag = .immediates, .class = c, .hash = @as(u56, @intCast(@intFromPtr(context))) << 8 | e };
    }
    inline fn hasPointer(self: object.Object) bool {
        const bits = math.rotr(TagAndClassType, self.tagbits(), 3);
        return bits <= math.rotr(TagAndClassType, oImm(.ThunkHeap, 0).tagbits(), 3) and bits != 0;
    }
    pub inline fn pointer(self: object.Object, T: type) ?T {
        switch (self.tag) {
            .heap => return @ptrFromInt(self.rawU()),
            .immediates => switch (self.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnObject, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance => return self.highPointer(T),
                else => {},
            },
            else => {},
        }
        return null;
    }
    pub inline fn makeImmediate(cls: Compact, hash: u56) object.Object {
        return oImm(cls, hash);
    }
    pub inline fn hash24(self: object.Object) u24 {
        return @truncate(self.hash);
    }
    pub inline fn hash32(self: object.Object) u32 {
        return @truncate(self.hash);
    }

    pub fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, comptime value: anytype) object.Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return oImm(.SmallInteger, @as(u56, @bitCast(@as(i56, value)))),
                .comptime_float => {
                    if (encode(value)) |encoded| {
                        return @bitCast(encoded);
                    } else |_| return fromAddress(ptr.set(.Float, value));
                },
                .bool => return if (value) object.Object.True() else object.Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            }
        }
    };
    pub inline fn from(value: anytype, sp: SP, context: *Context) object.Object {
        const T = @TypeOf(value);
        if (T == object.Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return fromNativeI(value, null, null),
            .float, .comptime_float => return fromNativeF(value, sp, context),
            .bool => return if (value) object.Object.True() else object.Object.False(),
            .null => return object.Object.Nil(),
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

    pub fn toWithCheck(self: object.Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (self.nativeF()) |flt| return flt;
            },
            i64 => {
                if (self.nativeI()) |int| return int;
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
                                if (!check or (self.hasMemoryReference() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.toUnchecked(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
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
    const class_table = blk: {
        var table = [_]ClassIndex{.Float} ** 256;
        for (0..31) |i| {
            table[i << 3] = .none;
            table[i << 3 | 1] = @enumFromInt(i);
        }
        break :blk table;
    };
    pub inline fn which_class(self: object.Object) ClassIndex {
        const Choose = enum {
            tag,
            table,
            firstFloat,
            andTagbits,
            bigSwitch,
            rotateTagbits,
        };
        switch (Choose.tag) {
            .tag => {
                switch (self.tag) {
                    .heap => if (self.rawU() == 0) {
                        @branchHint(.unlikely);
                        return .UndefinedObject;
                    } else return self.toUnchecked(*HeapObject).*.getClass(),
                    .immediates => {
                        @branchHint(.likely);
                        return self.class.classIndex();
                    },
                    else => {
                        @branchHint(.likely);
                        return .Float;
                    },
                }
            },
            .table => {
                const class = class_table[self.tagbits()];
                if (class == .none) {
                    @branchHint(.unlikely);
                    if (self.rawU() == 0) {
                        @branchHint(.unlikely);
                        return .UndefinedObject;
                    } else return self.toUnchecked(*HeapObject).*.getClass();
                }
                return class;
            },
            .bigSwitch => {
                const t = Compact.tag;
                switch (@as(u8, @truncate(self.rawU()))) {
                    0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38, 0x40, 0x48, 0x50, 0x58, 0x60, 0x68, 0x70, 0x78, 0x80, 0x88, 0x90, 0x98, 0xA0, 0xA8, 0xB0, 0xB8, 0xC0, 0xC8, 0xD0, 0xD8, 0xE0, 0xE8, 0xF0, 0xF8 => {
                        if (self.rawU() == 0) {
                            @branchHint(.unlikely);
                            return .UndefinedObject;
                        } else return self.toUnchecked(*HeapObject).*.getClass();
                    },
                    t(.ThunkReturnLocal) => {
                        @branchHint(.unlikely);
                        return .ThunkReturnLocal;
                    },
                    t(.ThunkReturnInstance) => {
                        @branchHint(.unlikely);
                        return .ThunkReturnInstance;
                    },
                    t(.ThunkReturnObject) => {
                        @branchHint(.unlikely);
                        return .ThunkReturnObject;
                    },
                    t(.ThunkReturnImmediate) => {
                        @branchHint(.unlikely);
                        return .ThunkReturnImmediate;
                    },
                    t(.ThunkReturnCharacter) => {
                        @branchHint(.unlikely);
                        return .ThunkReturnCharacter;
                    },
                    t(.ThunkReturnFloat) => {
                        @branchHint(.unlikely);
                        return .ThunkReturnFloat;
                    },
                    t(.ThunkLocal) => {
                        @branchHint(.unlikely);
                        return .ThunkLocal;
                    },
                    t(.BlockAssignLocal) => {
                        @branchHint(.unlikely);
                        return .BlockAssignLocal;
                    },
                    t(.ThunkInstance) => {
                        @branchHint(.unlikely);
                        return .ThunkInstance;
                    },
                    t(.BlockAssignInstance) => {
                        @branchHint(.unlikely);
                        return .BlockAssignInstance;
                    },
                    t(.ThunkHeap) => {
                        @branchHint(.unlikely);
                        return .ThunkHeap;
                    },
                    t(.LLVM) => {
                        @branchHint(.unlikely);
                        return .LLVM;
                    },
                    t(.ThunkImmediate) => {
                        @branchHint(.unlikely);
                        return .ThunkImmediate;
                    },
                    t(.ThunkFloat) => {
                        @branchHint(.unlikely);
                        return .ThunkFloat;
                    },
                    t(.SmallInteger) => {
                        @branchHint(.likely);
                        return .SmallInteger;
                    },
                    t(.Symbol) => {
                        @branchHint(.unlikely);
                        return .Symbol;
                    },
                    t(.Signature) => {
                        @branchHint(.unlikely);
                        return .Signature;
                    },
                    t(.False) => {
                        @branchHint(.likely);
                        return .False;
                    },
                    t(.True) => {
                        @branchHint(.likely);
                        return .True;
                    },
                    t(.Character) => {
                        @branchHint(.unlikely);
                        return .Character;
                    },
                    0xa9, 0xb1, 0xb9, 0xc1, 0xc9, 0xd1, 0xd9, 0xe1, 0xe9, 0xf1, 0xf9 => {
                        @branchHint(.unlikely);
                        @panic("Unexpected tag");
                    },
                    else => {
                        @branchHint(.likely);
                        return .Float;
                    },
                }
            },
            .firstFloat => {
                if (self.isImmediateDouble()) {
                    @branchHint(.likely);
                    return .Float;
                } else if (self.rawU() & 1 != 0) {
                    @branchHint(.likely);
                    return ClassIndex.classIndexFromInt(@truncate(self.rawU() >> 3));
                } else if (self.rawU() == 0) {
                    @branchHint(.unlikely);
                    return .UndefinedObject;
                } else return self.toUnchecked(*HeapObject).*.getClass();
            },
            .andTagbits => {
                const tag_bits = self.tagbits();
                const tag = tag_bits & 7;
                if (tag == 1) return @enumFromInt(tag_bits >> 3) else if (tag > 1) return .Float else if (self.rawU() == 0) return .UndefinedObject else return self.toUnchecked(*HeapObject).*.getClass();
            },
            .rotateTagbits => {
                const tag_bits = std.math.rotl(u8, self.tagbits(), 5);
                if (tag_bits >= 2 << 5) {
                    @branchHint(.likely);
                    return .Float;
                } else if (tag_bits > 1 << 5) {
                    @branchHint(.likely);
                    return ClassIndex.classIndexFromInt(@truncate(tag_bits));
                } else if (self.rawU() == 0) {
                    @branchHint(.unlikely);
                    return .UndefinedObject;
                } else return self.toUnchecked(*HeapObject).*.getClass();
            },
        }
    }
    pub inline fn hasMemoryReference(self: Object) bool {
        return if (self.ifHeapObject()) |_|
            self != Nil()
        else switch (self.class) {
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnObject, .ThunkReturnImmediate, .ThunkLocal, .BlockAssignLocal, .ThunkInstance, .BlockAssignInstance, .ThunkHeap, .ThunkReturnCharacter, .ThunkReturnFloat => true,
            else => false, // catches the nil case
        };
    }
    pub inline fn ifHeapObject(self: object.Object) ?*HeapObject {
        if (self.tag == .heap and self != Nil()) return @ptrFromInt(@as(u64, @bitCast(self)));
        return null;
    }

    pub inline fn asUntaggedI(i: i56) i64 {
        return @as(i64, i) << tagAndClassBits;
    }

    pub fn returnObjectClosure(self: Object, context: *Context) ?Object {
        if (self.nativeI()) |i| {
            switch (i) {
                -128...127 => return oImmContext(.ThunkReturnObject, context, @bitCast(@as(i8, @intCast(i)))),
                else => {},
            }
        } else {
            switch (self.which_class()) {
                .False, .True => |c| return oImmContext(.ThunkReturnImmediate, context, @truncate(oImm(c.compact(), 0).rawU())),
                .UndefinedObject => return oImmContext(.ThunkReturnImmediate, context, 0),
                else => {},
            }
        }
        return null;
    }
    pub fn returnLocalClosure(self: Object, context: *Context) ?Object {
        if (self.nativeI()) |i| {
            switch (i) {
                0...255 => return oImmContext(.ThunkReturnLocal, context, @bitCast(@as(i8, @intCast(i)))),
                else => {},
            }
        }
        return null;
    }
    pub fn immediateClosure(sig: Signature, sp: SP, context: *Context) ?Object {
        const class = sig.getClass();
        _ = sp;
        return switch (class) {
            .ThunkReturnObject, .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat => oImm(class.compact(), @intCast(@intFromPtr(context) << 8 | sig.primitive())),
            else => null,
        };
    }

    pub fn extraImmediateU(obj: Object) ?u11 {
        switch (obj.class) {
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat => {
                return obj.extraU();
            },
            else => {},
        }
        return null;
    }

    pub fn extraImmediateI(obj: Object) ?i11 {
        switch (obj.class) {
            .ThunkReturnObject => {
                return obj.extraI();
            },
            else => {},
        }
        return null;
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
    pub inline fn makeThunk(class: Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @intCast((@intFromPtr(obj) << 8) | tag));
    }
    pub inline fn makeThunkNoArg(class: Compact, value: u56) Object {
        return .oImm(class, value);
    }
    pub inline fn extraU(self: object.Object) u8 {
        return @intCast(self.hash & extraMask);
    }
    pub inline fn extraI(self: object.Object) i8 {
        return @bitCast(self.extraU());
    }
    const OF = object.ObjectFunctions;
    pub const arrayAsSlice = OF.arrayAsSlice;
    pub const asObjectArray = OF.asObjectArray;
    pub const asZeroTerminatedString = OF.asZeroTerminatedString;
    pub const compare = OF.compare;
    pub const empty = OF.empty;
    pub const equals = OF.equals;
    pub const format = OF.format;
    pub const getField = OF.getField;
    pub const get_class = OF.get_class;
    pub const isBool = OF.isBool;
    pub const toBoolNoCheck = OF.toBoolNoCheck;
    pub const isIndexable = OF.isIndexable;
    pub const isNil = OF.isNil;
    pub const isUnmoving = OF.isUnmoving;
    pub const promoteToUnmovable = OF.promoteToUnmovable;
    pub const rawFromU = OF.rawFromU;
    pub const setField = OF.setField;
    pub const to = OF.to;
    pub const toUnchecked = OF.toUnchecked;
    pub const header = OF.header;
    pub const asVariable = zag.Context.asVariable;
    pub const PackedObject = object.PackedObject;
    pub const signature = zag.execute.Signature.signature;
};
