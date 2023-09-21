const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const ClassIndex = object.ClassIndex;
const utilities = @import("utilities.zig");
const largerPowerOf2 = utilities.largerPowerOf2;
const inversePhi = utilities.inversePhi;
const assert = std.debug.assert;
const SP = @import("execute.zig").SP;
pub const Format = enum(u8) {
    immutableSizeZero,
    _i1,_i2,_i3,_i4,_i5,_i6,_i7,_i8,_i9,_i10,_i11,_i12,_i13,_i14,_i15,_i16,_i17,_i18,_i19,_i20,_i21,_i22,_i23,_i24,_i25,_i26,_i27,_i28,_i29,_i30,_i31,
    _i32,_i33,_i34,_i35,_i36,_i37,_i38,_i39,_i40,_i41,_i42,_i43,_i44,_i45,_i46,_i47,_i48,_i49,_i50,_i51,_i52,_i53,_i54,_i55,_i56,_i57,_i58,_i59,_i60,_i61,_i62,
    immutableByteMax,
    notObject, // this is an allocated struct, not an Object
    mutableSizeOne,
    _m2,_m3,_m4,_m5,_m6,_m7,_m8,_m9,_m10,_m11,_m12,_m13,_m14,_m15,_m16,_m17,_m18,_m19,_m20,_m21,_m22,_m23,_m24,_m25,_m26,_m27,_m28,_m29,_m30,_m31,
    _m32,_m33,_m34,_m35,_m36,_m37,_m38,_m39,_m40,_m41,_m42,_m43,_m44,_m45,_m46,_m47,_m48,_m49,_m50,_m51,_m52,_m53,_m54,_m55,_m56,_m57,_m58,_m59,_m60,_m61,_m62,
    mutableByteMax,
    notIndexable,
    notIndexable_mut,
    directIndexed,
    directIndexed_mut,
    indexed,
    indexed_mut,
    external,
    external_mut,
    nonObjectIndexed,
    nonObjectIndexed_mut, // this and below have no pointers
    _x10, //    header, // this is a header word, which points to the proper HeapObject footer word
    context, // this is a context object - special format
    compiledMethod, // this is a compiledMethod - special format
    _x13,_x14,_x15,
    notIndexableWithPointers,
    notIndexableWithPointers_mut, // this and below have no pointers in array portion
    directIndexedWithPointers,
    directIndexedWithPointers_mut, // this and below have no size/pointer
    indexedWithPointers,
    indexedWithPointers_mut,
    externalWithPointers,
    externalWithPointers_mut,
    nonObjectIndexedWithPointers, // this has no pointers in array portion
    nonObjectIndexedWithPointers_mut, // this has no pointers in array portion
    _y10,_y11,
    weakWithPointers, // only this and following have weak queue link
    weakWithPointers_mut,
    externalWeakWithPointers,
    externalWeakWithPointers_mut,
    _,
    const Self = @This();
    const MutableFlag: u8 = 1;
    const ExternalFlag: u8 = 8;
    const PointerFlag: u8 = 16;
    const ImmutableSizeZero = @intFromEnum(Format.immutableSizeZero);
    const ImmutableByteMax = @intFromEnum(Format.immutableByteMax);
    const NumberOfBytes = ImmutableByteMax - ImmutableSizeZero;
    const NotObject = @intFromEnum(Format.notObject);
    const MutableOffset = NotObject;
    const MutableSizeOne = @intFromEnum(Format.mutableSizeOne);
    const MutableByteMax = @intFromEnum(Format.mutableByteMax);
    const NotIndexable = @intFromEnum(Format.notIndexable);
    const LastPointerFree = @intFromEnum(Format.nonObjectIndexed_mut);
    const DirectIndexed = @intFromEnum(Format.directIndexed);
    const Indexed = @intFromEnum(Format.indexed);
    const External = @intFromEnum(Format.external);
    const Context = @intFromEnum(Format.context);
    const CompiledMethod = @intFromEnum(Format.compiledMethod);
    const FirstWeak = @intFromEnum(Format.weakWithPointers);
    const LastWeak = @intFromEnum(Format.externalWeakWithPointers_mut);
    const WithoutPointers = ~PointerFlag;
    comptime {
        assert(@intFromEnum(Format.notIndexable) == 128);
    }
    const Size = union(enum) {
        size: u8,
        indexable,
        notIndexable,
        special,
        const Indexable = Size{ .indexable = {} };
        const NotIndexable = Size{ .notIndexable = {} };
        const Special = Size{ .special = {} };
    };
    pub inline fn size(self: Self) Size {
        const s = @intFromEnum(self);
        switch (s) {
            ImmutableSizeZero...ImmutableByteMax => return Size{ .size = s },
            MutableSizeOne...MutableByteMax => return Size{ .size = s - MutableOffset},
            NotObject => return Size.NotIndexable,
            Context,CompiledMethod => return Size.Special,
            else => {}
        }
        return switch (s & ~(MutableFlag+PointerFlag)) {
            NotIndexable => Size.NotIndexable,
            else => Size.Indexable,
        };
    }
    // pub inline fn objectSize(s: usize) Self {
    //     if (s == 0) return .immutableSizeZero;
    //     return .indexed;
    // }
    pub inline fn isByteSize(s: usize) bool {
        return s <= NumberOfBytes;
    }
    pub inline fn byteSize(s: usize) Self {
        if (isByteSize(s))
            return @as(Self, @enumFromInt(s));
        return .indexed;
    }
    // pub inline fn isHeader(self: Self) bool {
    //     return self == .header;
    // }
    pub inline fn isIndexableWithPointers(_: Self) bool {
        return false;
    }
    pub inline fn isIndexable(self: Self) bool {
        return self != .notIndexable and self != .notIndexableWithPointers;
    }
    pub inline fn isWeak(self: Self) bool {
        return switch (@intFromEnum(self)) {
            FirstWeak...LastWeak => true,
            else => false,
        };
    }
    pub inline fn hasInstVars(_: Self) bool {
        // TODO: 
        return false;
    }
    pub inline fn hasIndexPointers(_: Self) bool {
        return false; // TODO
    }
    pub inline fn hasPointers(_: Self) bool {
        return false; // TODO
    }
    //    pub inline fn hasIndexFields(self: Self) bool {
    //        return @enumToInt(self) >= Indexed;
    //    }
    pub inline fn isExternal(self: Self) bool {
        return @intFromEnum(self)&(0xff-PointerFlag-ExternalFlag-MutableFlag) == External;
    }
    pub inline fn mutable(self: Self) Self {
        const s = @intFromEnum(self);
        return switch (s) {
            else => @enumFromInt(@intFromEnum(self) | MutableFlag),
        };
    }
    pub inline fn isImmutable(self: Self) bool {
        return switch (@intFromEnum(self)) {
            0...NotObject => true,
            MutableOffset+1...MutableByteMax => false,
            else => |i| i & MutableFlag == 0,
        };
    }
    const Iterator = *const fn (obj: HeapObjectConstPtr) HeapObjectPtrIterator;
    pub fn iterator(self: Self) Iterator {
        const i = HeapObjectPtrIterator;
        return switch (self) {
            .immediateSizeZero....immediateByteMax, .notIndexableWithPointers => i.ivPointers,
            .notObject, .notIndexable, .directIndexed => i.noPointers,
            .immediateObjectOne....immediateObjectMax, .directIndexedWithPointers => i.directPointers,
            .header => i.header,
            .externalWeakWithPointers, .weakWithPointers => i.weakDefault,
            else => i.bothPointers,
        };
    }
    fn eq(f: Self, v: u8) !void {
        return std.testing.expectEqual(@as(Self, @enumFromInt(v)), f);
    }
    pub fn allocationInfo(iVars: u12, indexed: ?usize, elementSize: usize, makeWeak: bool) AllocationInfo {
        if (indexed) |nElements| {
            const maxSize = HeapObject.maxLength;
            const arraySize = (nElements * elementSize + @sizeOf(Object) - 1) / @sizeOf(Object);
            if (makeWeak) {
                if (iVars + arraySize > maxSize - 3)
                    return .{ .format = .externalWeakWithPointers, .size = iVars, .sizeField = 3 };
                return .{ .format = .weakWithPointers, .size = iVars + @as(u12, @intCast(arraySize)), .sizeField = 3 };
            }
            if (nElements == 0 or (elementSize == 1 and nElements <= NumberOfBytes))
                return .{ .format = @as(Self, @enumFromInt(nElements)), .size = iVars + @as(u12, @intCast(arraySize)) };
            if (elementSize == @sizeOf(Object)) {
                if (iVars == 0 and nElements <= maxSize)
                    return .{ .format = .directIndexed, .size = @as(u12, @intCast(arraySize)) };
            }
            if (iVars + arraySize > maxSize - 2)
                return .{ .format = .external, .size = iVars, .sizeField = 2 };
            return .{ .format = .indexed, .size = iVars + @as(u12, @intCast(arraySize)), .sizeField = 2 };
        }
        if (makeWeak)
            return .{ .format = .weakWithPointers, .size = iVars, .sizeField = 3 };
        return .{ .format = .notIndexable, .size = iVars };
    }
    fn expectTrue(self: Self, ok: bool) !void {
        if (!ok) {
            std.debug.print("unexpected false for {}\n",.{self});
            return error.TestUnexpectedFalse;
        }
    }
    fn expectFalse(self: Self, ok: bool) !void {
        if (ok) {
            std.debug.print("unexpected true for {}\n",.{self});
            return error.TestUnexpectedTrue;
        }
    }
};
pub const AllocationInfo = struct {
    format: Format,
    size: u12,
    sizeField: u8 = 0,
    const Self = @This();
    pub inline fn requiresIndex(self: Self) bool {
        return self.sizeField > 0;
    }
    pub inline fn wholeSize(self: Self, maxLength: u12) !u12 {
        if (self.format.isExternal()) return error.ObjectTooLarge;
        return self.objectSize(maxLength);
    }
    pub inline fn objectSize(self: Self, maxLength: u12) !u12 {
        const size = self.size + self.sizeField;
        if (size+1 >= maxLength) return error.ObjectTooLarge;
        return size;
    }
    pub inline fn needsExternalAllocation(self: Self) bool {
        return self.format.isExternal();
    }
    pub inline fn nilAll(self: Self, theHeapObject: HeapObjectPtr) void {
        const start = theHeapObject.asObjectArray()-self.size-1;
        for (start[0..self.size]) |*obj|
            obj.* = Nil;
    }
    pub inline fn fillFooters(self: Self, theHeapObject: HeapObjectPtr, classIndex: ClassIndex, age: Age, nElements: usize, elementSize: usize) bool {
        const hash = if (builtin.is_test) 0 else @as(u24, @truncate(@as(u32, @truncate(@intFromPtr(theHeapObject) >> 4)) *% object.u32_phi_inverse >> 8));
        theHeapObject.* = HeapObject{
            .classIndex = classIndex,
            .hash = hash,
            .format = self.format,
            .age = age,
            .length = self.size + self.sizeField,
        };
        const size = self.sizeField;
        const external = self.format.isExternal();
        if (size > 0) {
            const footers = @as([*]u64, @ptrCast(theHeapObject)) - size;
            if (size>=3) footers[0] = 0;
            footers[size - 1] = nElements;
            footers[size - 2] = if (external) 0 else @intFromPtr(footers) - elementSize * nElements;
        }
        return external;
    }
};
test "isWeak formats" {
    for (0..255) |n| {
        const e: Format = @enumFromInt(n);
        switch (e) {
            .externalWeakWithPointers,
            .externalWeakWithPointers_mut,
            .weakWithPointers,
            .weakWithPointers_mut
            => try e.expectTrue(e.isWeak()),
            else => try e.expectFalse(e.isWeak()),
        }
    }
}
test "isExternal formats" {
    for (0..255) |n| {
        const e: Format = @enumFromInt(n);
        switch (e) {
            .external,
            .external_mut,
            .externalWithPointers,
            .externalWithPointers_mut,
            .externalWeakWithPointers,
            .externalWeakWithPointers_mut,
            ._x14,._x15, // never created
            => try e.expectTrue(e.isExternal()),
            else => try e.expectFalse(e.isExternal()),
        }
    }
}
test "raw size" {
    const ee = std.testing.expectEqual;
    try ee(@as(Format, @enumFromInt(7)).size(), Format.Size{ .size = 7 });
    try ee(@as(Format, @enumFromInt(64)).size(), Format.Size.NotIndexable);
    try ee(@as(Format, @enumFromInt(128)).size(), Format.Size.NotIndexable);
    try ee(@as(Format, @enumFromInt(130)).size(), Format.Size.Indexable);
    try ee(@as(Format, @enumFromInt(70)).size(), Format.Size{ .size = 6 });
    try ee(@as(Format, @enumFromInt(71)).size(), Format.Size{ .size = 7 });
    try ee(Format.immutableSizeZero.size(), Format.Size{ .size = 0 });
    try ee(Format.context.size(), Format.Size.Special);
    try ee(Format.compiledMethod.size(), Format.Size.Special);
}
test "HeapObject formats" {
    const expect = std.testing.expect;
    try expect(Format.immutableSizeZero.mutable().isImmutable());
    try expect(!Format.notIndexable.isIndexable());
    try expect(Format.indexed.isIndexable());
    try expect(Format.indexedWithPointers.isIndexable());
    try expect(Format.weakWithPointers.isIndexable());
    try expect(!Format.indexed.isIndexableWithPointers());
    try expect(Format.indexedWithPointers.isIndexableWithPointers());
    try expect(!Format.indexed.hasIndexPointers());
    try expect(Format.indexedWithPointers.hasIndexPointers());
    try expect(Format.weakWithPointers.hasIndexPointers());
    try expect(Format.weakWithPointers.isWeak());
    try expect(!Format.indexedWithPointers.isWeak());
    try expect(!Format.immutableByteMax.isWeak());
}
test "allocationInfo" {
    const ee = std.testing.expectEqual;
    // allocationInfo(iVars: u12, indexed: ?usize, eSize: ?usize, mSize: ?usize, makeWeak: bool)
    try ee(Format.allocationInfo(10, null, 0, false), AllocationInfo{ .format = .notIndexable, .size = 10 });
    try ee(Format.allocationInfo(10, null, 0, true), AllocationInfo{ .format = .weakWithPointers, .size = 10, .sizeField = 3 });
    try ee(Format.allocationInfo(10, 0, 0, false), AllocationInfo{ .format = .immutableSizeZero, .size = 10 });
//    try ee(Format.allocationInfo(10, 9, 8, false), AllocationInfo{ .format = @enumFromInt(Format.ImmediateObjectOne + 16), .size = 19 });
    try ee(Format.allocationInfo(10, 9, 1, false), AllocationInfo{ .format = @enumFromInt(Format.ImmutableSizeZero + 9), .size = 12 });
    try ee(Format.allocationInfo(10, 9, 2, false), AllocationInfo{ .format = .indexed, .size = 13, .sizeField = 2 });
    try ee(Format.allocationInfo(10, 90, 8, false), AllocationInfo{ .format = .indexed, .size = 100, .sizeField = 2 });
    try ee(Format.allocationInfo(0, 90, 8, false), AllocationInfo{ .format = .directIndexed, .size = 90 });
    try ee(Format.allocationInfo(0, 90, 1, false), AllocationInfo{ .format = .indexed, .size = 12, .sizeField = 2 });
    try ee(Format.allocationInfo(0, 90, 2, false), AllocationInfo{ .format = .indexed, .size = 23, .sizeField = 2 });
    try ee(Format.allocationInfo(10, 90, 8, true), AllocationInfo{ .format = .weakWithPointers, .size = 100, .sizeField = 3 });
    try ee(Format.allocationInfo(10, 9000, 8, false), AllocationInfo{ .format = .external, .size = 10, .sizeField = 2 });
    try ee(Format.allocationInfo(10, 9000, 8, true), AllocationInfo{ .format = .externalWeakWithPointers, .size = 10, .sizeField = 3 });
}
pub const Age = enum(u4) {
    onStack,
    nursery,
    nursery2,
    nursery3,
    nursery4,
    nursery5,
    nurseryLast,
    static,
    global,
    globalMarked,
    aStruct,
    globalScanned,
    aoo,
    aooMarked,
    free,
    aooScanned,
    const Static: Age = .static;
    // const ScanMask: u4 = GlobalScanned; // anded with this give 0 or Struct for non-global; Global, GlobalMarked or GlobalScanned for global (AoO or not)
    const Self = @This();
    pub const lastNurseryAge = @intFromEnum(Age.nurseryLast);
    pub inline fn isAoO(self: Self) bool {
        return switch (self) {
            .aoo, .aooMarked, .aooScanned => true,
            else => false,
        };
    }
    pub inline fn isUnmoving(self: Self) bool {
        return switch (self) {
            .static, .global, .globalMarked, .aStruct, .globalScanned, .aoo, .aooMarked, .free, .aooScanned => true,
            else => false,
        };
    }
    pub inline fn isGlobal(self: Self) bool {
        return switch (self) {
            .global, .globalMarked, .aStruct, .globalScanned, .aoo, .aooMarked, .free, .aooScanned => true,
            else => false,
        };
    }
    pub inline fn isNonHeap(self: Self) bool {
        return switch (self) {
            .static, .onStack => true,
            else => false,
        };
    }
    pub inline fn isStatic(self: Self) bool {
        return self == .static;
    }
    pub inline fn isOnStack(self: Self) bool {
        return self == .onStack;
    }
    pub inline fn isMarked(self: Self) bool {
        return switch (self) {
            .globalMarked, .globalScanned, .aooMarked, .aooScanned => true,
            else => false,
        };
    }
    pub inline fn marked(self: Self) !Self {
        return switch (self) {
            .global => .globalMarked,
            .aoo => .aooMarked,
            .globalMarked, .globalScanned, .aooMarked, .aooScanned => error.alreadyMarked,
            else => self,
        };
    }
    pub inline fn scanned(self: Self) !Self {
        return switch (self) {
            .globalMarked => .globalScanned,
            .aooMarked => .aooScanned,
            .globalScanned, .aooScanned => error.alreadyScanned,
            else => error.notMarked,
        };
    }
    // Note: assigning a ptr to a scanned object must block for collection
};
pub const HeapObjectPtrIterator = struct {
    const Self = @This();
    nextPointer: *const fn (*Self) ?HeapObjectPtr,
    scanObject: HeapObjectConstPtr,
    current: [*]const Object,
    beyond: [*]const Object,
    pub fn weakDefault(_: HeapObjectConstPtr) HeapObjectPtrIterator {
        @panic("weakDefault called");
    }
    pub fn noPointers(_: HeapObjectConstPtr) HeapObjectPtrIterator {
        return .{
            .nextPointer = allDone,
            .scanObject = undefined,
            .current = undefined,
            .beyond = undefined,
        };
    }
    pub fn ivPointers(obj: HeapObjectConstPtr) HeapObjectPtrIterator { // only ivs, or both with only iv pointers
        if (!obj.hasInstVarsWithPtrs()) return noPointers(obj);
        const ivs = @as([*]const Object, @ptrCast(obj));
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = ivs + 1,
            .beyond = ivs + 1 + obj.length,
        };
    }
    pub fn arrayPointers(obj: HeapObjectConstPtr) HeapObjectPtrIterator { // only array
        if (!obj.isIndexableWithPtrs()) return noPointers(obj);
        const ivs = @as([*]const Object, @ptrCast(obj));
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = ivs + 1,
            .beyond = ivs + 1 + obj.length,
        };
    }
    pub fn bothPointers(obj: HeapObjectConstPtr) HeapObjectPtrIterator { // both with iv and array pointers
        if (!obj.hasInstVarsWithPtrs()) return bothOnlyArrayPointers(obj);
        if (!obj.isIndexableWithPtrs()) return ivPointers(obj);
        const ivs = @as([*]const Object, @ptrCast(obj));
        return .{
            .nextPointer = firstPointerGroup,
            .scanObject = obj,
            .current = ivs + 1,
            .beyond = ivs + 1 + obj.length,
        };
    }
    pub fn bothOnlyArrayPointers(obj: HeapObjectConstPtr) HeapObjectPtrIterator { // both with no iv pointers
        if (!obj.isIndexableWithPtrs()) return noPointers(obj);
        const ivs = @as([*]const Object, @ptrCast(obj));
        const size = ivs[1 + obj.length].u();
        const array = @as([*]Object, @ptrFromInt(ivs[2 + obj.length].u()));
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = array,
            .beyond = array + size - 1,
        };
    }
    fn lastPointerGroup(self: *Self) ?HeapObjectPtr {
        while (@intFromPtr(self.current) < @intFromPtr(self.beyond)) {
            const obj = self.current[0];
            self.current += 1;
            if (obj.isHeapAllocated())
                return obj.toUnchecked(HeapObjectPtr);
        }
        self.nextPointer = allDone;
        return null;
    }
    fn firstPointerGroup(self: *Self) ?HeapObjectPtr {
        while (@intFromPtr(self.current) < @intFromPtr(self.beyond)) {
            const obj = self.current[0];
            self.current += 1;
            if (obj.isHeapAllocated())
                return obj.toUnchecked(HeapObjectPtr);
        }
        const obj = self.scanObject;
        const ivs = @as([*]const Object, @ptrCast(obj));
        const size = ivs[1 + obj.length].u();
        const array = @as([*]Object, @ptrFromInt(ivs[2 + obj.length].u()));
        self.current = array;
        self.beyond = array + size - 1;
        self.nextPointer = lastPointerGroup;
        return self.lastPointerGroup();
    }
    fn allDone(_: *Self) ?HeapObjectPtr {
        return null;
    }
    pub inline fn next(self: *const Self) ?HeapObjectPtr {
        return self.nextPointer(@constCast(self));
    }
};
// test "heapPtrIterator" {
//     const testing = std.testing;
//     var h1 = header(0x17, Format.objectNP, 0x27, 0x129,Age.nursery);
//     var h2 = header(0x0, Format.objectP, 0x27, 0x129,Age.nursery);
//     var o1 = [_]Object{Nil,Nil,h1.asObject(),True,h1.asObject(),h2.asObject(),True};
//     const ho1 = @ptrCast(HeapObjectPtr,&o1);
//     var i = h1.makeIterator();
//     try testing.expectEqual(i.next(),null);
//     i = h2.makeIterator();
//     try testing.expectEqual(i.next(),null);
//     o1[0] = header(@sizeOf(@TypeOf(o1))/8-1,Format.objectP, 0x27, 0x129,Age.nursery).o();
//     i = HeapObjectPtrIterator.noPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapObjectPtrIterator.ivPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h2);
//     try testing.expectEqual(i.next(),null);
//     o1[0] = header(@sizeOf(@TypeOf(o1))/8-1,Format.arrayP, 0x27, 0x129,Age.nursery).o();
//     i = HeapObjectPtrIterator.noPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapObjectPtrIterator.ivPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapObjectPtrIterator.arrayPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h2);
//     try testing.expectEqual(i.next(),null);
//     o1[3] = @bitCast(Object,@as(u64,2));
//     o1[4] = @bitCast(Object,@intFromPtr(&o1[5]));
//     o1[0] = header(2,Format.bothOP, 0x27, 0x129,Age.nursery).o();
//     i = HeapObjectPtrIterator.ivPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapObjectPtrIterator.bothPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapObjectPtrIterator.arrayPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     o1[0] = header(2,Format.bothPP, 0x27, 0x129,Age.nursery).o();
//     i = ho1.makeIterator();
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h2);
//     try testing.expectEqual(i.next(),null);
// }

pub const AllocErrors = error{ Fail, HeapFull, NotIndexable, ObjectTooLarge, NeedNurseryCollection };
pub const AllocResult = struct {
    age: Age,
    allocated: HeapObjectPtr,
    info: AllocationInfo,
    pub fn nilAll(self: *AllocResult) void {
        self.info.nilAll(self.allocated);
    }
};
pub const AllocReturn = AllocErrors!AllocResult;

pub const HeapObjectArray = [*]align(@alignOf(u64)) HeapObject;
pub const HeapObjectSlice = []align(@alignOf(u64)) HeapObject;
pub const HeapObjectPtr = *align(@alignOf(u64)) HeapObject;
pub const HeapObjectConstPtr = *align(@alignOf(u64)) const HeapObject;
pub const HeapObject = packed struct(u64) {
    classIndex: ClassIndex,
    hash: u24,
    format: Format,
    age: Age,
    length: u12,

    const immediateLength: u16 = 4095; // all immediate objects (except doubles) have this as top 12 bits
    const forwardLength: u16 = 4094;
    pub const maxLength = 4093;
    pub const includesHeader = true;
    pub fn iterator(self: HeapObjectConstPtr) Format.Iterator {
        return self.format.iterator();
    }
    pub fn makeIterator(self: HeapObjectConstPtr) HeapObjectPtrIterator {
        return self.format.iterator()(self);
    }
    const partialHeader = @as(u64, @bitCast(HeapObject{ .classIndex = @enumFromInt(0), .hash = 0, .format = .header, .age = .onStack, .length = 0 }));
    pub inline fn partialHeaderOnStack(selfOffset: u16) HeapObject {
        return @as(HeapObject, @bitCast(partialHeader | @as(u64, selfOffset) << 16));
    }
    pub inline fn staticHeaderWithLength(size: u12) HeapObject {
        return HeapObject{ .classIndex = @enumFromInt(0), .hash = 0, .format = .header, .age = .static, .length = size };
    }
    pub inline fn staticHeaderWithClassLengthHash(classIndex: ClassIndex, size: u12, hash: u24) HeapObject {
        return HeapObject{ .classIndex = classIndex, .hash = hash, .format = .header, .age = .static, .length = size };
    }
    pub inline fn simpleStackObject(classIndex: ClassIndex, size: u12, hash: u24) HeapObject {
        return HeapObject{ .classIndex = classIndex, .hash = hash, .format = .directIndexed, .age = .onStack, .length = size };
    }
    pub fn addFooter(headerPtr: HeapObjectPtr) void {
        const footerPtr = headerPtr.realHeapObject();
        footerPtr.* = headerPtr.*;
        footerPtr.format = .directIndexed;
    }
    pub fn asHeader(self: HeapObject) HeapObject {
        var result = self;
        result.format = .header;
        result.length -= 1;
        return result;
    }
    pub inline fn realHeapObject(self: HeapObjectConstPtr) HeapObjectPtr {
        const result = if (self.format.isHeader())
            @as(HeapObjectPtr, @ptrCast(@as(HeapObjectArray, @ptrCast(@constCast(self))) + self.length + 1))
        else
            @constCast(self);
        return result;
    }
    inline fn init(length: u12, format: Format, classIndex: ClassIndex, hash: u24, age: Age) HeapObject {
        return HeapObject{
            .classIndex = classIndex,
            .hash = hash,
            .format = format,
            .age = age,
            .length = length,
        };
    }
    pub inline fn calcHeapObject(classIndex: ClassIndex, comptime iVars: u12, hash: u24, age: Age, indexed: ?usize, elementSize: usize, makeWeak: bool) !HeapObject {
        const aI = comptime Format.allocationInfo(iVars, indexed, elementSize, makeWeak);
        if (aI.requiresIndex()) return error.DoesntFit;
        return HeapObject{
            .classIndex = classIndex,
            .hash = hash,
            .format = aI.format,
            .age = age,
            .length = aI.size,
        };
    }
    pub inline fn setFooters(self: HeapObjectPtr, iVars: u12, classIndex: u16, hash: u24, age: Age, indexed: ?usize, elementSize: ?usize, mSize: ?usize, makeWeak: bool) void {
        return Format.allocationInfo(iVars, indexed, elementSize, mSize, makeWeak).fillFooters(self, classIndex, hash, age, indexed, elementSize);
    }
    pub inline fn prev(self: HeapObjectPtr) Object {
        const ptr = @as([*]Object, @ptrCast(self)) - 1;
        return ptr[0];
    }
    pub inline fn prevPrev(self: HeapObjectPtr) Object {
        const ptr = @as([*]Object, @ptrCast(self)) - 2;
        return ptr[0];
    }
    pub inline fn setFields(self: HeapObjectPtr, fill: Object, age: ?Age) void {
        if (fill == Nil and !self.format.isExternal()) {
            unreachable;
            //mem.set(Object,result.wholeObjectSlice(),fill);
            //return;
        } else {
            unreachable;
        }
        _ = age; // if (age) |newAge| self.age = newAge;
    }
    pub inline fn isOnStack(self: HeapObjectConstPtr) bool {
        return self.age.isOnStack();
    }
    pub inline fn isStatic(self: HeapObjectConstPtr) bool {
        return self.age.isStatic();
    }
    pub inline fn isNonHeap(self: HeapObjectConstPtr) bool {
        return self.age.isNonHeap();
    }
    pub inline fn isUnmoving(self: HeapObjectConstPtr) bool {
        return self.age.isUnmoving();
    }
    pub inline fn forwardedTo(self: HeapObjectConstPtr) HeapObjectConstPtr {
        return @as(HeapObjectConstPtr, @ptrFromInt(@as(u64, @intCast(@as(i64, @intCast(@as(u64, @bitCast(self.*)) << 16)) >> 16))));
    }
    pub inline fn isForwarded(self: HeapObjectConstPtr) bool {
        return self.length == forwardLength;
    }
    pub inline fn forwarded(self: HeapObjectConstPtr) HeapObjectConstPtr {
        if (self.isForwarded()) {
            return self.forwardedTo();
        }
        return self;
    }
    pub inline fn skipBack(self: HeapObjectConstPtr) HeapObjectArray {
        const head = self.*;
        const size = head.length;
        return @ptrFromInt(@intFromPtr(self) - @sizeOf(Object) * size);
    }
    pub inline fn asSlice(self: HeapObjectConstPtr) ![]Object {
        const head = self.*;
        const size = head.length;
        if (size == forwardLength) {
            const realSelf = self.forwardedTo();
            const start = @as([*]Object, @ptrFromInt(@intFromPtr(realSelf) - @sizeOf(Object) * realSelf.length));
            return start[0..size];
        } else {
            const start = @as([*]Object, @ptrFromInt(@intFromPtr(self) - @sizeOf(Object) * size));
            return start[0..size];
        }
    }
    pub inline fn asSliceWithoutHeader(self: HeapObjectConstPtr) ![]Object {
        const head = self.*;
        const size = head.length;
        //        std.io.getStdErr().writer().print("\naASWH {}",.{head}) catch unreachable;
        if (size == forwardLength) {
            const realSelf = self.forwardedTo();
            const start = @as([*]Object, @ptrFromInt(@intFromPtr(realSelf) - @sizeOf(Object) * realSelf.length));
            return start[1..size];
        } else {
            const start = @as([*]Object, @ptrFromInt(@intFromPtr(self) - @sizeOf(Object) * size));
            return start[1..size];
        }
    }
    pub inline fn arrayAsSlice(self: HeapObjectConstPtr, comptime T: type) ![]T {
        const head = self.*;
        //        std.io.getStdErr().writer().print("\naAS {}",.{head}) catch unreachable;
        if (head.length == forwardLength) {
            const realSelf = self.forwardedTo();
            return realSelf.arrayAsSlice_(realSelf.*, T);
        } else return self.arrayAsSlice_(head, T);
    }
    inline fn arrayAsSlice_(self: HeapObjectConstPtr, head: HeapObject, comptime T: type) ![]T {
        //        if (head.age.isOnStack()) unreachable;
        //        std.io.getStdErr().writer().print("\nsize={} 0x{x} {}",.{head.format.size(),@intFromPtr(self),head}) catch unreachable;
        switch (head.format.size()) {
            .notIndexable => return error.NotIndexable,
            .size => |s| {
                const oa = @as([*]T, @ptrFromInt(@intFromPtr(self) - @sizeOf(T) * s));
                return oa[0..s];
            },
            .indexable => {
                const oa = @as([*]usize, @ptrFromInt(@intFromPtr(self) - @sizeOf(usize) * 2));
                return @as([*]T, @ptrFromInt(oa[0]))[0..oa[1]];
            },
            .special => unreachable,
        }
    }
    pub inline fn arraySize(maybeForwarded: HeapObjectConstPtr) !usize {
        const self = maybeForwarded.forwarded();
        switch (self.format.size()) {
            .notIndexable => return error.NotIndexable,
            .size => |s| {
                return s;
            },
            .indexable => {
                const oa = @as([*]u64, @ptrFromInt(@intFromPtr(self) - @sizeOf(u64)));
                return oa[0];
            },
        }
    }
    pub fn growSizeX(maybeForwarded: HeapObjectConstPtr, stepSize: usize) !usize {
        const self = maybeForwarded.forwarded();
        const form = self.format;
        if (!form.isIndexable()) return error.NotIndexable;
        var size: usize = self.length;
        if (form.hasInstVars()) {
            const oa = @as([*]u64, @ptrFromInt(@intFromPtr(self)));
            size = form.wordSize(oa[size + 1]);
        }
        size = largerPowerOf2(size * 2);
        if (size > HeapObject.maxLength and size < HeapObject.maxLength * 2) size = HeapObject.maxLength;
        return (form.getSize() * size + stepSize - 1) / stepSize * stepSize;
    }
    pub inline fn inHeapSize(maybeForwarded: HeapObjectConstPtr) usize {
        const self = maybeForwarded.forwarded();
        const form = self.format;
        const size = self.length;
        if (form.isIndexable() and form.hasInstVars()) {
            var oa = @as([*]u64, @ptrFromInt(@intFromPtr(self))) + size;
            return size + 3 + if (oa[2] != @intFromPtr(oa + 3)) 0 else form.wordSize(oa[1]);
        }
        return size + 1;
    }
    pub inline fn isIndirect(maybeForwarded: HeapObjectConstPtr) bool {
        const self = maybeForwarded.forwarded();
        const form = self.format;
        var size: usize = self.length;
        if (!form.isIndexable()) return false;
        if (form.hasInstVars()) {
            var oa = @as([*]u64, @ptrFromInt(@intFromPtr(self))) + size;
            return oa[2] != @intFromPtr(oa + 3);
        }
        return false;
    }
    pub inline fn isRaw(self: HeapObjectConstPtr) bool {
        return self.format.isRaw();
    }
    pub inline fn asObject(self: HeapObjectConstPtr) Object {
        return Object.from(self);
    }
    pub inline fn asObjectConstPtr(self: HeapObjectConstPtr) [*]const Object {
        return @as([*]const Object, @ptrCast(self));
    }
    pub inline fn asObjectPtr(self: HeapObjectPtr) [*]Object {
        return @as([*]Object, @ptrCast(self));
    }
    pub inline fn fromObjectPtr(op: [*]const Object) HeapObjectArray {
        return @as(HeapObjectArray, @ptrFromInt(@intFromPtr(op)));
    }
    pub inline fn o(self: HeapObject) Object {
        return @as(Object, @bitCast(self));
    }
    pub inline fn asObjectArray(self: HeapObjectConstPtr) [*]align(@alignOf(u64)) Object {
        return @as([*]align(@alignOf(u64)) Object, @ptrFromInt(@intFromPtr(self))) + 1;
    }
    pub inline fn setHash(self: HeapObjectPtr, hash: u24) HeapObject {
        self.hash = hash;
        return self.*;
    }
    pub inline fn setNArgs(self: HeapObjectPtr, args: u8) HeapObject {
        self.hash = (self.hash & 0xffff) + (args << 16);
        return self.*;
    }
    pub inline fn getClass(self: HeapObjectConstPtr) ClassIndex {
        return self.classIndex;
    }
    pub inline fn hash16(self: HeapObject) u16 {
        return @as(u16, @truncate(@as(u64, @bitCast(self)) >> 16));
    }
    pub inline fn hasInstVars(self: HeapObjectConstPtr) bool {
        return self.format.hasInstVars();
    }
    pub inline fn hasInstVarsWithPtrs(self: HeapObjectConstPtr) bool {
        return self.format.hasInstVarsWithPtrs();
    }
    pub inline fn isIndexable(self: HeapObjectConstPtr) bool {
        return self.format.isIndexable();
    }
    pub inline fn isIndexableWithPtrs(self: HeapObjectConstPtr) bool {
        return self.format.isIndexableWithPtrs();
    }
    pub inline fn instVars(self: HeapObjectConstPtr) []Object {
        if (self.format.hasInstVars()) {
            const size = self.length;
            return self.asObjectArray()[0..size];
        } else return &[0]Object{};
    }
    pub inline fn instVarPut(self: HeapObjectConstPtr, index: usize, obj: Object) void {
        self.instVars()[index] = obj;
    }
    fn @"format FUBAR"(
        self: HeapObjectConstPtr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        _ = fmt;
        _ = options;
        if (self.format.isRaw()) {
            if (self.format.is8() and self.classIndex == object.String_I) {
                try writer.print("{s}", .{self.indexables(u8)});
            } else if (self.format.is8()) {
                try writer.print("raw[{}]", .{self.indexables(u8).len});
            } else if (self.format.is16()) {
                try writer.print("raw[{}]", .{self.indexables(u16).len});
            } else if (self.format.is32()) {
                try writer.print("raw[{}]", .{self.indexables(u32).len});
            } else try writer.print("raw[{}]", .{self.indexables(u64).len});
        } else {
            var blank = false;
            const ivs = self.instVars();
            if (ivs.len > 0) {
                try writer.print("#(", .{});
                for (ivs) |item| {
                    if (blank) try writer.print(" ", .{});
                    blank = true;
                    try writer.print("{}", .{item});
                }
                try writer.print(")", .{});
            }
            if (self.format.isIndexable()) {
                const idx = self.indexables(Object);
                if (blank) try writer.print(" ", .{});
                blank = false;
                try writer.print("{c}", .{'{'});
                for (idx) |item| {
                    if (blank) try writer.print(" ", .{});
                    blank = true;
                    try writer.print("{}", .{item});
                }
                try writer.print("{c}", .{'}'});
            }
        }
    }
};
pub fn growSize(obj: anytype, comptime Target: type) !usize {
    const T = @TypeOf(obj);
    if (T == Object) return growSize(obj.arrayAsSlice(Target), Target);
    switch (@typeInfo(T)) {
        .Pointer => |ptr| if (ptr.child != Target) @compileError("types must match " ++ @typeName(ptr.child) ++ " and " ++ @typeName(Target)),
        else => @compileError("only pointer types: " ++ @typeName(T)),
    }
    var size = (obj.len * @sizeOf(Target) + @sizeOf(Object) - 1) / @sizeOf(Object);
    size = largerPowerOf2(size * 2);
    // ToDo: use Format to round down to fit - i.e. consider number of footer words
    if (size > HeapObject.maxLength and size < HeapObject.maxLength * 2) size = HeapObject.maxLength;
    return (size * @sizeOf(Object) + @sizeOf(Target) - 1) / @sizeOf(Target);
}
test "growSize" {
    //try std.testing.expectEqual(growSize(@as([]const u8,"foo"[0..]),u8),8);
}
pub const footer = HeapObject.init;
// test "HeapObject structure" {
//     const testing = std.testing;
//     try testing.expectEqual(@sizeOf(HeapObject),8);
//     const hdr = header(0x17, Format.objectNP, 0x27, 0x129,Age.nursery);
//     try testing.expectEqual(hdr.o().u(),0x0173200001290027);
// }
fn hash24(str: []const u8) u24 {
    const phi: u32 = inversePhi(u24);
    var hash = phi *% @as(u32, @truncate(str.len +% 1));
    for (str, 0..) |c, idx| {
        if (idx > 9) break;
        hash +%= phi *% c;
    }
    return @as(u24, @truncate(hash));
}
pub fn CompileTimeString(comptime str: []const u8) type {
    const size = str.len;
    const words = (size + @sizeOf(Object) - 1) / @sizeOf(Object);
    const fill = words * @sizeOf(Object) - size;
    const hash = hash24(str);
    return extern struct {
        chars: [size + fill]u8,
        footer: HeapObject,
        const Self = @This();
        pub fn init() Self {
            var result = Self{
                .footer = footer(words, @as(Format, @enumFromInt(size)), ClassIndex.String, hash, Age.static),
                .chars = [_]u8{0} ** (size + fill),
            };
            for (str, result.chars[fill..]) |c, *r| {
                r.* = c;
            }
            return result;
        }
        fn h(self: *const Self) []const u8 {
            return @as([*]const u8, @ptrCast(self))[0 .. (size + 15) / 8 * 8];
        }
        fn obj(self: *const Self) HeapObjectConstPtr {
            return @as(*const HeapObject, @ptrCast(&self.footer));
        }
        pub fn asObject(self: *const Self) Object {
            return Object.from(self.obj());
        }
    };
}
pub fn compileStrings(comptime tup: anytype) [tup.len]HeapObjectConstPtr {
    @setEvalBranchQuota(3000);
    comptime var result: [tup.len]HeapObjectConstPtr = undefined;
    inline for (tup, 0..) |name, idx| {
        result[idx] = comptime (&CompileTimeString(name).init()).obj();
    }
    return result;
}

const abcde = CompileTimeString("abcdefghijklm").init();
const strings = compileStrings(.{ // must be in same order as above
    "Object", "SmallInteger", "Float", "False", "True",
});
test "compile time" {
    try std.testing.expect(mem.eql(u8, abcde.asObject().arrayAsSlice(u8), "abcdefghijklm"));
    try std.testing.expect(mem.eql(u8, try strings[3].arrayAsSlice(u8), "False"));
}
