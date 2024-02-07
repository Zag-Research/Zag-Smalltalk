const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const config = @import("config.zig");
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
const compileObject = @import("execute.zig").compileObject;
const Sym = @import("symbol.zig").symbols;
pub const Format = enum(u8) {
    immutableSizeZero,
    notObject, // this is an allocated struct, not an Object
    notIndexable = 128,
    notIndexable_mut,
    directIndexed,
    directIndexed_mut,
    indexed,
    indexed_mut,
    external,
    external_mut,
    indexedNonObject,
    indexedNonObject_mut, // this and below have no pointers
    special, // this is a special format
    specialHeader,
    _x12,
    _x13,
    _x14,
    _x15,
    notIndexableWithPointers,
    notIndexableWithPointers_mut, // this and below have no pointers in array portion
    directIndexedWithPointers,
    directIndexedWithPointers_mut, // this and below have no size/pointer
    indexedWithPointers,
    indexedWithPointers_mut,
    externalWithPointers,
    externalWithPointers_mut,
    indexedNonObjectWithPointers, // this has no pointers in array portion
    indexedNonObjectWithPointers_mut, // this has no pointers in array portion
    _y10,
    _y11,
    weakWithPointers, // only this and following have weak queue link
    weakWithPointers_mut,
    externalWeakWithPointers,
    externalWeakWithPointers_mut,
    _,
    const Self = @This();
    const MutableFlag: u8 = 1;
    const ExternalFlag: u8 = 8;
    const PointerFlag: u8 = 16;
    const BaseFormatMask: u8 = 0xff - PointerFlag - MutableFlag;
    const ImmutableSizeZero = @intFromEnum(Format.immutableSizeZero);
    const MutableOffset = NotObject;
    const NumberOfBytes = 63;
    const NotObject = @intFromEnum(Format.notObject);
    const NotIndexable = @intFromEnum(Format.notIndexable);
    const LastPointerFree = @intFromEnum(Format.indexedNonObject_mut);
    const DirectIndexed = @intFromEnum(Format.directIndexed);
    const Indexed = @intFromEnum(Format.indexed);
    const External = @intFromEnum(Format.external);
    const SpecialHeader = @intFromEnum(Format.specialHeader);
    const Special = @intFromEnum(Format.special);
    const FirstWeak = @intFromEnum(Format.weakWithPointers);
    const Last = @intFromEnum(Format.externalWeakWithPointers_mut);
    comptime {
        assert(@intFromEnum(Format.notIndexable) == 128);
    }
    inline fn base(self: Self, comptime mask: comptime_int) Self {
        return @enumFromInt(@intFromEnum(self) & (BaseFormatMask - mask));
    }
    const Size = enum(u8) {
        indexable = 251,
        external,
        directIndexable,
        notIndexable,
        special,
        _,
        fn set(ptr: *Size, value: Size) void {
            const s: [*]Size = @ptrCast(ptr);
            s[0] = value;
            s[MutableFlag] = value;
            s[PointerFlag] = value;
            s[PointerFlag + MutableFlag] = value;
        }
    };
    const sizes = init: {
        var s = [_]Size{.indexable} ** 256;
        for (ImmutableSizeZero..NotIndexable) |n| {
            s[n] = @enumFromInt(n / 2);
        }
        s[NotObject] = Size.notIndexable;
        (&s[NotIndexable]).set(Size.notIndexable);
        (&s[DirectIndexed]).set(Size.directIndexable);
        (&s[External]).set(Size.external);
        s[SpecialHeader] = Size.special;
        s[Special] = Size.special;
        break :init s;
    };
    pub inline fn size(self: Self) Size {
        return sizes[@intFromEnum(self)];
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
    pub fn isHeader(self: Self) bool {
        return self == .specialHeader;
    }
    pub inline fn isSpecial(self: Self) bool {
        return self == .context or self == .compiledMethod;
    }
    pub inline fn isIndexable(self: Self) bool {
        return self.base(0) != .notIndexable;
    }
    pub inline fn isWeak(self: Self) bool {
        return @intFromEnum(self) >= FirstWeak;
    }
    pub inline fn potentiallyHasInstVars(_: Self) bool {
        unreachable;
    }
    pub inline fn hasIndexPointers(self: Self) bool {
        return self.hasPointers() and self.isIndexable();
    }
    pub inline fn hasPointers(self: Self) bool {
        return @intFromEnum(self) >= LastPointerFree;
    }
    inline fn addPointers(self: Self) Self {
        return @enumFromInt(@intFromEnum(self) | PointerFlag);
    }
    pub inline fn isExternal(self: Self) bool {
        return self.base(ExternalFlag) == .external;
    }
    pub inline fn mutable(self: Self) Self {
        if (self == .immutableSizeZero) return self;
        return @enumFromInt(@intFromEnum(self) | MutableFlag);
    }
    pub inline fn isImmutable(self: Self) bool {
        return @intFromEnum(self) & MutableFlag == 0;
    }
    fn eq(f: Self, v: u8) !void {
        return std.testing.expectEqual(@as(Self, @enumFromInt(v)), f);
    }
    fn expectTrue(self: Self, ok: bool) !void {
        if (!ok) {
            std.debug.print("unexpected false for {}\n", .{self});
            return error.TestUnexpectedFalse;
        }
    }
    fn expectFalse(self: Self, ok: bool) !void {
        if (ok) {
            std.debug.print("unexpected true for {}\n", .{self});
            return error.TestUnexpectedTrue;
        }
    }
};
test "isWeak formats" {
    for (0..Format.Last) |n| {
        const e: Format = @enumFromInt(n);
        switch (e) {
            .externalWeakWithPointers, .externalWeakWithPointers_mut, .weakWithPointers, .weakWithPointers_mut => try e.expectTrue(e.isWeak()),
            else => try e.expectFalse(e.isWeak()),
        }
    }
}
test "isExternal formats" {
    for (0..Format.Last) |n| {
        const e: Format = @enumFromInt(n);
        switch (e) {
            .external,
            .external_mut,
            .externalWithPointers,
            .externalWithPointers_mut,
            .externalWeakWithPointers,
            .externalWeakWithPointers_mut,
            ._x14,
            ._x15, // never created
            => try e.expectTrue(e.isExternal()),
            else => try e.expectFalse(e.isExternal()),
        }
    }
}
test "raw size" {
    const ee = std.testing.expectEqual;
    try ee(@as(Format, @enumFromInt(7)).size(), @as(Format.Size, @enumFromInt(3)));
    try ee(@as(Format, @enumFromInt(1)).size(), Format.Size.notIndexable);
    try ee(@as(Format, @enumFromInt(128)).size(), Format.Size.notIndexable);
    try ee(@as(Format, @enumFromInt(130)).size(), Format.Size.directIndexable);
    try ee(@as(Format, @enumFromInt(71)).size(), @as(Format.Size, @enumFromInt(35)));
    try ee(Format.immutableSizeZero.size(), @as(Format.Size, @enumFromInt(0)));
    try ee(Format.specialHeader.size(), Format.Size.special);
    try ee(Format.special.size(), Format.Size.special);
}
test "HeapObject formats" {
    const expect = std.testing.expect;
    try expect(Format.immutableSizeZero.mutable().isImmutable());
    try expect(!Format.notIndexable.isIndexable());
    try expect(Format.indexed.isIndexable());
    try expect(Format.indexedWithPointers.isIndexable());
    try expect(Format.weakWithPointers.isIndexable());
    try expect(!Format.indexed.hasIndexPointers());
    try expect(Format.indexedWithPointers.hasIndexPointers());
    try expect(!Format.indexed.hasIndexPointers());
    try expect(Format.indexedWithPointers.hasIndexPointers());
    try expect(Format.weakWithPointers.hasIndexPointers());
    try expect(Format.weakWithPointers.isWeak());
    try expect(!Format.indexedNonObject.isWeak());
    try expect(!Format.notObject.isWeak());
}
pub const AllocationInfo = struct {
    format: Format,
    size: u12,
    sizeField: u8 = 0,
    const Self = @This();
    pub fn calc(iVars: u12, indexed: ?usize, comptime element: type, makeWeak: bool) Self {
        if (indexed) |nElements| {
            const maxSize = HeapObject.maxLength;
            const arraySize = (nElements * @sizeOf(element) + @sizeOf(Object) - 1) / @sizeOf(Object);
            if (makeWeak) {
                if (iVars + arraySize > maxSize - 3)
                    return .{ .format = .externalWeakWithPointers, .size = iVars, .sizeField = 3 };
                return .{ .format = .weakWithPointers, .size = iVars + @as(u12, @intCast(arraySize)), .sizeField = 3 };
            }
            if (iVars == 0) {
                if (nElements == 0 or (element == u8 and nElements <= Format.NumberOfBytes))
                    return .{ .format = @as(Format, @enumFromInt(nElements * 2)), .size = @intCast(arraySize) };
                if (element == Object and nElements <= maxSize)
                    return .{ .format = .directIndexed, .size = @intCast(arraySize) };
            }
            if (iVars + arraySize > maxSize - 2)
                return .{ .format = .external, .size = iVars, .sizeField = 2 };
            return .{ .format = .indexed, .size = iVars + @as(u12, @intCast(arraySize)), .sizeField = 2 };
        }
        if (makeWeak)
            return .{ .format = .weakWithPointers, .size = iVars, .sizeField = 3 };
        return .{ .format = .notIndexable, .size = iVars };
    }

    pub inline fn requiresIndex(self: Self) bool {
        return self.sizeField > 0;
    }
    pub inline fn wholeSize(self: Self, maxLength: u12) !u12 {
        if (self.format.isExternal()) return error.ObjectTooLarge;
        return self.objectSize(maxLength);
    }
    pub inline fn objectSize(self: Self, maxLength: u12) !u12 {
        const size = self.size + self.sizeField;
        if (size + 1 >= maxLength) return error.ObjectTooLarge;
        return size;
    }
    pub inline fn needsExternalAllocation(self: Self) bool {
        return self.format.isExternal();
    }
    pub inline fn nilAll(self: Self, theHeapObject: HeapObjectPtr) void {
        const start = theHeapObject.asObjectArray() - self.size - 1;
        for (start[0..self.size]) |*obj|
            obj.* = Nil;
    }
    inline fn heapObject(self: Self, classIndex: ClassIndex, age: Age, hash: u24) HeapObject {
        return .{
            .classIndex = classIndex,
            .hash = hash,
            .format = self.format,
            .age = age,
            .length = self.size + self.sizeField,
        };
    }
    pub inline fn fillFooters(self: Self, theHeapObject: HeapObjectPtr, classIndex: ClassIndex, age: Age, nElements: usize, comptime element: type) bool {
        const hash = if (builtin.is_test) 0 else @as(u24, @truncate(@as(u32, @truncate(@intFromPtr(theHeapObject) >> 4)) *% object.u32_phi_inverse >> 8));
        theHeapObject.* = self.heapObject(classIndex, age, hash);
        const size = self.sizeField;
        const external = self.format.isExternal();
        if (size > 0) {
            const footers = @as([*]u64, @ptrCast(theHeapObject)) - size;
            if (size >= 3) footers[0] = 0;
            footers[size - 1] = nElements;
            footers[size - 2] = if (external) 0 else @intFromPtr(footers) - @sizeOf(element) * nElements;
        }
        return external;
    }
};
test "allocationInfo" {
    const ee = std.testing.expectEqual;
    // allocationInfo(iVars: u12, indexed: ?usize, eSize: ?usize, mSize: ?usize, makeWeak: bool)
    try ee(AllocationInfo.calc(10, null, void, false), AllocationInfo{ .format = .notIndexable, .size = 10 });
    try ee(AllocationInfo.calc(10, null, void, true), AllocationInfo{ .format = .weakWithPointers, .size = 10, .sizeField = 3 });
    try ee(AllocationInfo.calc(0, 0, void, false), AllocationInfo{ .format = .immutableSizeZero, .size = 0 });
    //    try ee(AllocationInfo.calc(10, 9, 8, false), AllocationInfo{ .format = @enumFromInt(Format.ImmediateObjectOne + 16), .size = 19 });
    try ee(AllocationInfo.calc(10, 9, u8, false), AllocationInfo{ .format = .indexed, .size = 12, .sizeField = 2 });
    try ee(AllocationInfo.calc(10, 9, u16, false), AllocationInfo{ .format = .indexed, .size = 13, .sizeField = 2 });
    try ee(AllocationInfo.calc(10, 90, u64, false), AllocationInfo{ .format = .indexed, .size = 100, .sizeField = 2 });
    try ee(AllocationInfo.calc(0, 90, Object, false), AllocationInfo{ .format = .directIndexed, .size = 90 });
    try ee(AllocationInfo.calc(0, 90, u8, false), AllocationInfo{ .format = .indexed, .size = 12, .sizeField = 2 });
    try ee(AllocationInfo.calc(0, 90, u16, false), AllocationInfo{ .format = .indexed, .size = 23, .sizeField = 2 });
    try ee(AllocationInfo.calc(10, 90, u64, true), AllocationInfo{ .format = .weakWithPointers, .size = 100, .sizeField = 3 });
    try ee(AllocationInfo.calc(10, 9000, Object, false), AllocationInfo{ .format = .external, .size = 10, .sizeField = 2 });
    try ee(AllocationInfo.calc(10, 9000, Object, true), AllocationInfo{ .format = .externalWeakWithPointers, .size = 10, .sizeField = 3 });
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
    pub inline fn isNursery(self: Self) bool {
        return switch (self) {
            .nursery, .nursery2, .nursery3, .nursery4, .nursery5, .nurseryLast => true,
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
    nextPointer: *const fn (*Self) ?*Object,
    scanObject: HeapObjectConstPtr,
    current: [*]Object,
    beyond: [*]Object,
    pub inline fn next(self: *const Self) ?*Object {
        return self.nextPointer(@constCast(self));
    }
    var specials = [_]*const fn (*Self) ?*Object{&notSpecial} ** (ClassIndex.LastSpecial + 1);
    var specialHeaders = [_]*const fn (*Self) ?*Object{&realFromHeader} ** (ClassIndex.LastSpecial + 1);
    fn notSpecial(_: *Self) ?*Object {
        @panic("special with no special function");
    }
    fn realFromHeader(self: *Self) ?*Object {
        const obj = self.scanObject;
        self.nextPointer = specials[@intFromEnum(obj.classIndex)];
        self.scanObject = obj.realHeapObject();
        return self.next();
    }
    pub inline fn iterator(maybeForwarded: HeapObjectConstPtr) ?HeapObjectPtrIterator {
        var obj = maybeForwarded.forwarded();
        const format = obj.format;
        if (!format.hasPointers()) return null;
        switch (format.base(0)) {
            .notObject => return null,
            .notIndexable, .directIndexed => {
                const oa = obj.start();
                return .{
                    .nextPointer = remainingPointers,
                    .scanObject = obj,
                    .current = oa,
                    .beyond = @constCast(@ptrCast(obj)),
                };
            },
            .specialHeader => {
                return .{
                    .nextPointer = specialHeaders[@intFromEnum(obj.classIndex)],
                    .scanObject = obj,
                    .current = @constCast(@ptrCast(obj)),
                    .beyond = @constCast(@ptrCast(obj)),
                };
            },
            .special => {
                return .{
                    .nextPointer = specials[@intFromEnum(obj.classIndex)],
                    .scanObject = obj,
                    .current = @constCast(@ptrCast(obj)),
                    .beyond = @constCast(@ptrCast(obj)),
                };
            },
            .externalWeakWithPointers, .weakWithPointers => unreachable,
            else => unreachable,
        }
    }
    fn remainingPointers(self: *Self) ?*Object {
        while (@intFromPtr(self.current) < @intFromPtr(self.beyond)) {
            const addr = &self.current[0];
            self.current += 1;
            if (addr.isMemoryAllocated())
                return addr;
        }
        return null;
    }
};
test "heapPtrIterator" {
    const testing = std.testing;
    const ho1 = AllocationInfo.calc(0, 0, Object, false).heapObject(ClassIndex.Object, .static, 0);
    try testing.expectEqual(ho1.makeIterator(), null);
    const c = ClassIndex;
    var o1b = compileObject(.{
        True,
        Sym.i_0, // alternate reference to replacement Object #1
        42,
        c.Class, // third HeapObject
    });
    o1b.setLiterals(&[_]Object{Nil}, &[_]ClassIndex{});
    const ho1b = o1b.asHeapObjectPtr();
    try testing.expectEqual(ho1b.format, .notIndexable);
    try testing.expectEqual(ho1b.makeIterator(), null);
    var o2 = compileObject(.{
        "def",
        True,
        ":first",
        c.Method, // first HeapObject

        ":second",
        c.replace0, // second HeapObject - runtime ClassIndex #0
        "first", // pointer to first object
        "1mref", // reference to replacement Object #1
        Sym.i_1, // alternate reference to replacement Object #1
        "second", // pointer to second object
        ":def",
        c.Class, // third HeapObject
    });
    o2.setLiterals(&[_]Object{ Nil, True }, &[_]ClassIndex{@enumFromInt(0xdead)});
    const ho2 = o2.asHeapObjectPtr();
    try testing.expectEqual(ho2.classIndex, .Class);
    try testing.expectEqual(ho2.format, .notIndexableWithPointers);
    var i = ho2.makeIterator() orelse return error.NoIterator;
    try testing.expectEqual(i.next(), &o2.asObjectArray()[4]);
    try testing.expectEqual(i.next(), &o2.asObjectArray()[7]);
    try testing.expectEqual(i.next(), null);
    // var o3 = [_]Object{Nil,Nil,h1.asObject(),True,h1.asObject(),h2.asObject(),True};
    // const ho3 = AllocationInfo.calc(o3.len, null, Object, false).fillFooters(@ptrCast(&o3[o3.len-1]), ClassIndex.Object, .static, 0, Object);
}

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
    pub inline fn makeIterator(self: HeapObjectConstPtr) ?HeapObjectPtrIterator {
        return HeapObjectPtrIterator.iterator(self);
    }
    const contextHeader = @as(u64, @bitCast(HeapObject{ .classIndex = @enumFromInt(0), .hash = 0, .format = .specialHeader, .age = .onStack, .length = 0 }));
    pub inline fn contextHeaderOnStack(selfOffset: u16) HeapObject {
        return @as(HeapObject, @bitCast(contextHeader | @as(u64, selfOffset) << 16));
    }
    pub inline fn staticHeaderWithLength(size: u12) HeapObject {
        return HeapObject{ .classIndex = @enumFromInt(0), .hash = 0, .format = .special, .age = .static, .length = size };
    }
    pub inline fn staticHeaderWithClassLengthHash(classIndex: ClassIndex, size: u12, hash: u24) HeapObject {
        return HeapObject{ .classIndex = classIndex, .hash = hash, .format = .specialHeader, .age = .static, .length = size };
    }
    pub inline fn contextHeaderWithClassLengthHash(classIndex: ClassIndex, size: u12, hash: u24) HeapObject {
        return HeapObject{ .classIndex = classIndex, .hash = hash, .format = .context, .age = .static, .length = size };
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
        result.format = .specialHeader;
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
    pub inline fn calcHeapObject(classIndex: ClassIndex, comptime iVars: u12, hash: u24, age: Age, indexed: ?usize, comptime element: type, makeWeak: bool) !HeapObject {
        const aI = comptime AllocationInfo.calc(iVars, indexed, element, makeWeak);
        if (aI.requiresIndex()) return error.DoesntFit;
        return HeapObject{
            .classIndex = classIndex,
            .hash = hash,
            .format = aI.format,
            .age = age,
            .length = aI.size,
        };
    }
    pub inline fn setFooters(self: HeapObjectPtr, iVars: u12, classIndex: u16, hash: u24, age: Age, indexed: ?usize, element: type, mSize: ?usize, makeWeak: bool) void {
        return AllocationInfo.calc(iVars, indexed, element, mSize, makeWeak).fillFooters(self, classIndex, hash, age, indexed, element);
    }
    pub inline fn copyTo(self: HeapObjectPtr, hp: [*]HeapObject, reference: *Object) [*]HeapObject {
        const size = self.length + 1;
        if (size == forwardLength + 1) { // already forwarded
            reference.* = switch (config.objectEncoding) {
                .nan => @bitCast((reference.rawU() & 0xffff000000000000) + @as(u48, @truncate(@as(u64, @bitCast(self.*))))),
                .tag => {}};
            return hp;
        }
        const target = hp - size;
        @memcpy(target[0..size], @as([*]HeapObject, @ptrCast(self.start())));
        self.* = @bitCast((@as(u64, forwardLength) << 48) + @intFromPtr(hp - 1));
        reference.* = switch (config.objectEncoding) {
            .nan => @bitCast((reference.rawU() & 0xffff000000000000) + @intFromPtr(hp - 1)),
            .tag => {}};
        return target;
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
        return @ptrFromInt(@as(u64, @intCast(@as(i64, @intCast(@as(u64, @bitCast(self.*)) << 16)) >> 16)));
    }
    pub inline fn isForwarded(self: HeapObject) bool {
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
    pub inline fn asSlice(maybeForwarded: HeapObjectConstPtr) ![]Object {
        const self = maybeForwarded.forwarded();
        return self.start[0..self.length];
    }
    pub inline fn asSliceWithoutHeader(maybeForwarded: HeapObjectConstPtr) ![]Object {
        const self = maybeForwarded.forwarded();
        return self.start[1..self.length];
    }
    pub inline fn arrayAsSlice(self: HeapObjectConstPtr, comptime T: type) ![]T {
        const head = self.*;
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
            .indexable => {
                const oa = @as([*]usize, @ptrFromInt(@intFromPtr(self) - @sizeOf(usize) * 2));
                return @as([*]T, @ptrFromInt(oa[0]))[0..oa[1]];
            },
            .directIndexable => {
                const oa = @as([*]T, @ptrCast(self.start()));
                return oa[0..head.length];
            },
            .special => unreachable,
            .external => unreachable,
            else => |s| {
                const size = @intFromEnum(s);
                const oa = @as([*]T, @constCast(@ptrCast(self))) - size;
                return oa[0..size];
            },
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
            const oa = @as([*]u64, @ptrFromInt(@intFromPtr(self))) + size;
            return size + 3 + if (oa[2] != @intFromPtr(oa + 3)) 0 else form.wordSize(oa[1]);
        }
        return size + 1;
    }
    pub inline fn isIndirect(maybeForwarded: HeapObjectConstPtr) bool {
        const self = maybeForwarded.forwarded();
        const form = self.format;
        const size: usize = self.length;
        if (!form.isIndexable()) return false;
        if (form.hasInstVars()) {
            const oa = @as([*]u64, @ptrFromInt(@intFromPtr(self))) + size;
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
    pub inline fn start(self: HeapObjectConstPtr) [*]Object {
        return @as([*]Object, @constCast(@ptrCast(self))) - self.length;
    }
    pub inline fn instVars(self: HeapObjectConstPtr) []Object {
        const length = self.length;
        const oa = self.start();
        switch (self.format.size()) {
            .notIndexable => return oa[0..length],
            .indexable => {
                const array: [*]usize = @ptrFromInt(@intFromPtr(self) - @sizeOf(usize) * 2);
                return oa[0 .. length - array[1]];
            },
            .special => unreachable,
            .external => unreachable,
            else => return &[0]Object{},
        }
    }
    pub inline fn instVarPut(self: HeapObjectPtr, index: usize, obj: Object) void {
        self.format = self.format.addPointers();
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
                .footer = footer(words, @as(Format, @enumFromInt(size * 2)), ClassIndex.String, hash, Age.static),
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
const strings = compileStrings(.{
    "Object", "SmallInteger", "Float", "False", "True",
});
test "compile time" {
    try std.testing.expect(mem.eql(u8, abcde.asObject().arrayAsSlice(u8), "abcdefghijklm"));
    //    try std.testing.expect(mem.eql(u8, try strings[3].arrayAsSlice(u8), "False"));
}
