const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const debugError = false;
const object = if (debugError) struct {
    const ClassIndex = enum(u16) { String };
    const Object = struct {
        field: u64,
        const Self = @This();
        fn asObject(self: *Self) Self {
            return .{ .field = @intFromPtr(self) };
        }
    };
} else zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const ClassIndex = object.ClassIndex;
const utilities = zag.utilities;
const largerPowerOf2 = utilities.largerPowerOf2;
const inversePhi24 = utilities.inversePhi(u24);
const assert = std.debug.assert;
pub const Format = enum(u7) {
    immutableSizeZero = 0,
    indexedStruct = NumberOfBytes + 1, // this is an allocated struct, not an Object
    externalStruct, // this is a big allocated struct, not an Object
    notIndexable, // this is an Object with no indexable values
    indexedNonObject,
    externalNonObject,
    directIndexed,
    indexed,
    external,
    free, // this and below have no pointers
    special, // this is a special format
    notIndexableWithPointers, // this and below have no pointers in array portion
    indexedNonObjectWithPointers, // this has no pointers in array portion
    externalNonObjectWithPointers, // this has no pointers in array portion
    directIndexedWithPointers,
    indexedWithPointers,
    externalWithPointers,
    externalWeakWithPointers, // only this and following have weak queue link
    indexedWeakWithPointers,
    _,
    comptime {
        assert(@intFromEnum(Format.notIndexable) == 128 - 16);
        assert(@intFromEnum(Format.indexedWeakWithPointers) == 127);
    }
    const Self = @This();
    const ImmutableSizeZero = @intFromEnum(Format.immutableSizeZero);
    const MutableOffset = NotObject;
    const NumberOfBytes = 109;
    const NotObject = @intFromEnum(Format.notObject);
    const NotIndexable = @intFromEnum(Format.notIndexable);
    const LastPointerFree = @intFromEnum(Format.free);
    const DirectIndexed = @intFromEnum(Format.directIndexed);
    const Indexed = @intFromEnum(Format.indexed);
    const External = @intFromEnum(Format.external);
    const Special = @intFromEnum(Format.special);
    const FirstWeak = @intFromEnum(Format.externalWeakWithPointers);
    const LastWeak = @intFromEnum(Format.indexedWeakWithPointers);
    const Last = 128;
    // comptime {
    //     assert(@intFromEnum(Format.notIndexable) == 0x6e);
    //     assert(@intFromEnum(Format.notIndexableWithPointers) == 0x76);
    //     assert(@intFromEnum(Format.indexedWeakWithPointers) == 0x7f);
    // }
    pub inline fn asU7(self: Self) u7 {
        return @truncate(@intFromEnum(self));
    }
    const operationsArray = HeapOperations.init();
    pub fn operations(self: Self) *const HeapOperations {
        return &operationsArray[self.asU7()];
    }
    pub inline fn instVars(self: Self, header: HeapHeader, obj: *const HeapObject) HeapOperationError![]Object {
        return self.operations().instVars(self, header, @constCast(obj));
    }
    pub //inline
    fn array(self: Self, header: HeapHeader, obj: *const HeapObject, elementSize: usize) HeapOperationError![]Object {
        return self.operations().array(self, header, @constCast(obj), elementSize);
    }
    pub inline fn mutableArray(self: Self, header: HeapHeader, obj: *const HeapObject, elementSize: usize) HeapOperationError![]Object {
        if (self.isMutable())
            return self.operations().array(self, header, obj, elementSize);
        return error.immutable;
    }
    pub inline fn size(self: Self, header: HeapHeader, obj: *const HeapObject) HeapOperationError!usize {
        return self.operations().size(self, header, obj);
    }
    pub inline fn pointerIterator(self: Self, header: HeapHeader, obj: *HeapObject) ?HeapObjectPtrIterator {
        trace("pointerIterator: {} {}\n", .{ self, self.operations() });
        if (self.operations().iterator) |iteratorFn|
            return iteratorFn(header, obj);
        return null;
    }
    pub inline fn isIndexable(self: Self) bool {
        return self.operations().isIndexable();
    }
    pub inline fn isExternal(self: Self) bool {
        return self.operations().isExternal();
    }
    pub inline fn isByteSize(s: usize) bool {
        return s <= NumberOfBytes;
    }
    pub inline fn byteSizeOf(s: usize) Self {
        if (isByteSize(s))
            return @as(Self, @enumFromInt(s));
        return .indexed;
    }
    pub inline fn isSpecial(self: Self) bool {
        return self == .special;
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
        return @intFromEnum(self) > LastPointerFree;
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
            .externalWeakWithPointers,
            .indexedWeakWithPointers,
            => try e.expectTrue(e.isWeak()),
            else => try e.expectFalse(e.isWeak()),
        }
    }
}
// test "isExternal formats" {
//     for (0..Format.Last) |n| {
//         const e: Format = @enumFromInt(n);
//         switch (e) {
//             .external,
//             .externalNonObject,
//             .free,
//             .special,
//             .externalWithPointers,
//             .externalNonObjectWithPointers,
//             .externalWeakWithPointers,
//             => try e.expectTrue(e.isExternal()),
//             else => try e.expectFalse(e.isExternal()),
//         }
//     }
// }
// test "HeapObject formats" {
//     const expect = std.testing.expect;
//     try expect(Format.immutableSizeZero.mutable().isImmutable());
//     try expect(!Format.notIndexable.isIndexable());
//     try expect(Format.indexed.isIndexable());
//     try expect(Format.indexedWithPointers.isIndexable());
//     try expect(Format.weakWithPointers.isIndexable());
//     try expect(!Format.indexed.hasIndexPointers());
//     try expect(Format.indexedWithPointers.hasIndexPointers());
//     try expect(!Format.indexed.hasIndexPointers());
//     try expect(Format.indexedWithPointers.hasIndexPointers());
//     try expect(Format.weakWithPointers.hasIndexPointers());
//     try expect(Format.weakWithPointers.isWeak());
//     try expect(!Format.indexedNonObject.isWeak());
//     try expect(!Format.notObject.isWeak());
// }
const HeapOperationError = error{ immutable, notIndexable, wrongElementSize };
const HeapOperations = struct {
    array: *const fn (Format, HeapHeader, *const HeapObject, usize) HeapOperationError![]Object = notArray,
    instVars: *const fn (Format, HeapHeader, *const HeapObject) HeapOperationError![]Object = noInstVars,
    size: *const fn (Format, HeapHeader, *const HeapObject) HeapOperationError!usize = noSize,
    iterator: ?*const fn (HeapHeader, *const HeapObject) ?HeapObjectPtrIterator = null,
    instVarWithPtr: Format = .free,
    inline fn set(ops: *[128]HeapOperations, format: Format, tuple: HeapOperations) void {
        const idx = @intFromEnum(format);
        const withPtr = if (idx >= @intFromEnum(Format.notIndexable) and idx < @intFromEnum(Format.free)) idx + 8 else idx;
        ops[idx].array = tuple.array;
        ops[idx].instVars = tuple.instVars;
        ops[idx].size = tuple.size;
        ops[idx].iterator = tuple.iterator;
        ops[idx].instVarWithPtr = @enumFromInt(withPtr);
        if (withPtr != idx)
            ops[withPtr] = ops[idx];
    }
    fn init() [128]HeapOperations {
        var ops = [_]HeapOperations{undefined} ** 128;
        for (ops[0..], 0..) |*op, n| {
            op.* = if (n < Format.NotIndexable) .{
                .array = byteArray,
                .size = byteSize,
                .instVarWithPtr = @enumFromInt(n),
            } else .{
                .array = unimplementedArray,
                .instVars = unimplementedInstVars,
                .size = unimplementedSize,
            };
        }
        set(&ops, .notIndexable, .{
            .instVars = justInstVars,
            .iterator = HeapObjectPtrIterator.iteratorAll,
        });
        //indexedNonObject,
        //externalNonObject,
        set(&ops, .directIndexed, .{
            .array = directArray,
            .size = directSize,
            .iterator = HeapObjectPtrIterator.iteratorAll,
        });
        //indexed,
        //external,
        return ops;
    }
    fn isIndexable(self: *const HeapOperations) bool {
        return self.array != notArray;
    }
    fn isExternal(_: *const HeapOperations) bool {
        return false; // ToDo
    }
    fn directArray(_: Format, header: HeapHeader, obj: *const HeapObject, _: usize) HeapOperationError![]Object {
        return obj.asObjectArray()[0..header.length];
    }
    fn directSize(_: Format, header: HeapHeader, _: *const HeapObject) HeapOperationError!usize {
        return header.length;
    }
    fn byteArray(format: Format, _: HeapHeader, obj: *const HeapObject, elementSize: usize) HeapOperationError![]Object {
        if (format.asU7() > 0 and elementSize != 1) return error.wrongElementSize;
        return obj.asObjectArray()[0..format.asU7()];
    }
    fn byteSize(format: Format, _: HeapHeader, _: *const HeapObject) HeapOperationError!usize {
        return format.asU7();
    }
    fn notArray(_: Format, _: HeapHeader, _: *const HeapObject, _: usize) HeapOperationError![]Object {
        return error.notIndexable;
    }
    fn noInstVars(_: Format, _: HeapHeader, _: *const HeapObject) HeapOperationError![]Object {
        return &[0]Object{};
    }
    fn justInstVars(_: Format, header: HeapHeader, obj: *const HeapObject) HeapOperationError![]Object {
        return obj.asObjectArray()[0..header.length];
    }
    fn noSize(_: Format, _: HeapHeader, _: *const HeapObject) HeapOperationError!usize {
        return error.notIndexable;
    }
    fn unimplementedArray(format: Format, _: HeapHeader, _: *const HeapObject, _: usize) HeapOperationError![]Object {
        std.debug.print("format: {}\n", .{format});
        @panic("unimplemented");
    }
    fn unimplementedInstVars(format: Format, _: HeapHeader, _: *const HeapObject) HeapOperationError![]Object {
        std.debug.print("format: {}\n", .{format});
        @panic("unimplemented");
    }
    fn unimplementedSize(format: Format, _: HeapHeader, _: *const HeapObject) HeapOperationError!usize {
        std.debug.print("format: {}\n", .{format});
        @panic("unimplemented");
    }
};
pub const HeapObjectPtrIterator = struct {
    const Self = @This();
    nextPointer: *const fn (*Self) ?*Object,
    scanObject: HeapObjectConstPtr,
    current: [*]Object,
    beyond: [*]Object,
    pub inline fn next(self: *Self) ?*Object {
        return self.nextPointer(self);
    }
    var specials = [_]*const fn (*Self) ?*Object{&notSpecial} ** (ClassIndex.LastSpecial + 1);
    fn notSpecial(_: *Self) ?*Object {
        @panic("special with no special function");
    }
    pub fn iteratorAll(head: HeapHeader, self: HeapObjectConstPtr) ?HeapObjectPtrIterator {
        const oa = self.start() + 1;
        return .{
            .nextPointer = remainingPointers,
            .scanObject = self,
            .current = oa,
            .beyond = oa + head.length,
        };
    }
    pub fn iteratorSpecial(head: HeapHeader, self: HeapObjectConstPtr) ?HeapObjectPtrIterator {
        const oa = self.start() + 1;
        return .{
            .nextPointer = specials[@intFromEnum(head.classIndex)],
            .scanObject = self,
            .current = oa,
            .beyond = oa + 1,
        };
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
// test "heapPtrIterator" {
//     const testing = std.testing;
//     const ho1 = AllocationInfo.calc(0, 0, Object, false).heapHeader(ClassIndex.Object, .static, 0);
//     try testing.expectEqual(ho1.makeIterator(), null);
//     const c = ClassIndex;
//     const compileObject = zag.execute.compileObject;
//     const Sym = zag.symbol.symbols;
//     var o1b = compileObject(.{
//         True,
//         Sym.i_0, // alternate reference to replacement Object #1
//         42,
//         c.Class, // third HeapObject
//     });
//     o1b.setLiterals(&[_]Object{Nil}, &[_]ClassIndex{});
//     const ho1b = o1b.asHeapObjectPtr();
//     try testing.expectEqual(ho1b.format, .notIndexable);
//     try testing.expectEqual(ho1b.makeIterator(), null);
//     var o2 = compileObject(.{
//         "def",
//         True,
//         ":first",
//         c.Method, // first HeapObject

//         ":second",
//         c.replace0, // second HeapObject - runtime ClassIndex #0
//         "first", // pointer to first object
//         "1mref", // reference to replacement Object #1
//         Sym.i_1, // alternate reference to replacement Object #1
//         "second", // pointer to second object
//         ":def",
//         c.Class, // third HeapObject
//     });
//     o2.setLiterals(&[_]Object{ Nil, True }, &[_]ClassIndex{@enumFromInt(0xdead)});
//     const ho2 = o2.asHeapObjectPtr();
//     try testing.expectEqual(ho2.classIndex, .Class);
//     try testing.expectEqual(ho2.format, .notIndexableWithPointers);
//     var i = ho2.makeIterator() orelse return error.NoIterator;
//     try testing.expectEqual(i.next(), &o2.asObjectArray()[4]);
//     try testing.expectEqual(i.next(), &o2.asObjectArray()[7]);
//     try testing.expectEqual(i.next(), null);
//     // var o3 = [_]Object{Nil,Nil,h1.asObject(),True,h1.asObject(),h2.asObject(),True};
//     // const ho3 = AllocationInfo.calc(o3.len, null, Object, false).fillFooters(@ptrCast(&o3[o3.len-1]), ClassIndex.Object, .static, 0, Object);
// }
inline fn hashFromPtr(ptr: anytype) u24 {
    return @truncate((@intFromPtr(ptr) >> 3) *% inversePhi24);
}
pub const AllocationInfo = struct {
    format: Format,
    nInstVars: u11 = 0,
    footerLength: u11 = 0,
    isObject: bool = true,
    nIndexed: usize = 0,
    size: usize = 0,
    footerSetup: ?*const fn (Self, HeapObjectPtr) void = null,
    const Self = @This();
    pub fn calc(iVars: u11, indexed: ?usize, comptime element: type, makeWeak: bool) Self {
        const isObject = element == Object;
        if (indexed) |nElements| {
            const maxSize = HeapHeader.maxLength;
            const arraySize = (nElements * @sizeOf(element) + @sizeOf(Object) - 1) / @sizeOf(Object);
            if (makeWeak) {
                if (iVars + arraySize > maxSize - 3)
                    return WeakIndexedFooter.init(.{ .format = .externalWeakWithPointers, .nInstVars = iVars, .size = nElements });
                return WeakIndexedFooter.init(.{ .format = .indexedWeakWithPointers, .nInstVars = iVars, .nIndexed = arraySize, .size = nElements });
            }
            if (iVars == 0) {
                if (nElements == 0 or (element == u8 and nElements <= Format.NumberOfBytes))
                    return .{ .format = @as(Format, @enumFromInt(nElements)), .nIndexed = arraySize, .isObject = isObject };
                if (isObject and nElements <= maxSize)
                    return .{ .format = .directIndexed, .nIndexed = arraySize, .size = nElements };
            }
            if (iVars + arraySize > maxSize - 2)
                return IndexedFooter.init(.{ .format = if (isObject) .external else .externalNonObject, .nInstVars = iVars, .size = nElements, .isObject = isObject });
            return IndexedFooter.init(.{ .format = if (isObject) .indexed else .indexedNonObject, .nInstVars = iVars, .nIndexed = arraySize, .size = nElements, .isObject = isObject });
        }
        if (makeWeak)
            return WeakIndexedFooter.init(.{ .format = .indexedWeakWithPointers, .nInstVars = iVars, .isObject = isObject });
        return .{ .format = .notIndexable, .nInstVars = iVars };
    }
    pub fn array(indexed: usize, comptime element: type) Self {
        return calc(0, indexed, element);
    }
    pub fn of(comptime x: anytype) Self {
        const T = @TypeOf(x);
        return switch (@typeInfo(T)) {
            .array => |a| calc(0, a.len, a.child, false),
            .pointer => |p| calc(0, x.len, p.child, false),
            .@"struct" => calc(@sizeOf(T) / @sizeOf(Object), null, Object, false),
            else => |unknown| @compileLog(unknown),
        };
    }
    pub inline fn requiresIndex(self: Self) bool {
        return self.footerLength > 0;
    }
    pub inline fn objectSize(self: Self, maxLength: u11) ?u11 {
        const size = self.nInstVars + self.nIndexed + self.footerLength;
        if (size + 1 >= maxLength) return null;
        return @intCast(size);
    }
    pub inline fn objectHeapSize(self: Self) u11 {
        if (self.objectSize(HeapHeader.maxLength)) |size| return size;
        return self.nInstVars +% self.footerLength;
    }
    pub inline fn externalSize(self: Self) usize {
        if (self.objectSize(HeapHeader.maxLength)) |_| return 0;
        return self.nIndexed;
    }
    pub inline fn initContents(self: Self, theHeapObject: HeapObjectPtr) void {
        const start = theHeapObject.asObjectArray();
        for (start[0..self.nInstVars]) |*obj|
            obj.* = Nil;
        if (self.nIndexed > 0) {
            const slice = start[self.nInstVars .. self.nInstVars + self.nIndexed];
            if (self.isObject) {
                for (slice) |*obj|
                    obj.* = Nil;
            } else {
                for (slice) |*obj|
                    obj.* = Object.ZERO;
            }
        }
    }
    inline fn heapHeader(self: Self, classIndex: ClassIndex, age: Age, hash: u24) HeapHeader {
        return .{
            .classIndex = classIndex,
            .hash = hash,
            .format = self.format,
            .age = age,
            .length = self.objectHeapSize(),
        };
    }
    pub inline fn initObjectStructure(self: Self, theHeapObject: HeapObjectPtr, classIndex: ClassIndex, age: Age) void {
        const hash = if (builtin.is_test) 0 else hashFromPtr(theHeapObject);
        theHeapObject.setHeader(self.heapHeader(classIndex, age, hash));
        if (self.footerSetup) |setup|
            setup(self, theHeapObject);
    }
};
pub const IndexedFooter = union {
    m: [@sizeOf(Internal)]u8,
    i: Internal,
    const Internal = extern struct {
        arrayPtr: [*]Object,
        arrayLen: usize,
    };
    const nObjects = @sizeOf(@This()) / @sizeOf(Object);
    inline fn init(self: AllocationInfo) AllocationInfo {
        var result = self;
        result.footerLength = @sizeOf(@This()) / @sizeOf(Object);
        result.footerSetup = &indexedFooter;
        return result;
    }
    fn indexedFooter(self: AllocationInfo, theHeapObject: HeapObjectPtr) void {
        const base = theHeapObject.asObjectArray() + self.nInstVars;
        const footers: *IndexedFooter = @ptrCast(base + self.nIndexed);
        footers.i.arrayPtr = base;
        footers.i.arrayLen = self.size;
    }
};
pub const WeakIndexedFooter = union {
    m: [@sizeOf(Internal)]u8,
    i: Internal,
    const Internal = extern struct {
        weakLink: ?HeapObjectConstPtr,
        arrayPtr: [*]Object,
        arrayLen: usize,
    };
    inline fn init(self: AllocationInfo) AllocationInfo {
        var result = self;
        result.footerLength = @sizeOf(@This()) / @sizeOf(Object);
        result.footerSetup = &weakIndexedFooter;
        return result;
    }
    fn weakIndexedFooter(self: AllocationInfo, theHeapObject: HeapObjectPtr) void {
        const base = theHeapObject.asObjectArray() + self.nInstVars;
        const footers: *WeakIndexedFooter = @ptrCast(base + self.nIndexed);
        footers.i.weakLink = null;
        footers.i.arrayPtr = base;
        footers.i.arrayLen = self.size;
    }
};

test "allocationInfo" {
    const ee = std.testing.expectEqual;
    // allocationInfo(iVars: u12, indexed: ?usize, eSize: ?usize, mSize: ?usize, makeWeak: bool)
    try ee(AllocationInfo.calc(0, null, Object, false), AllocationInfo{ .format = .notIndexable });
    try ee(AllocationInfo.calc(10, null, void, false), AllocationInfo{ .format = .notIndexable, .nInstVars = 10 });
    try ee(AllocationInfo.calc(10, null, void, true), WeakIndexedFooter.init(.{ .format = .indexedWeakWithPointers, .nInstVars = 10, .isObject = false }));
    try ee(AllocationInfo.calc(0, 0, Object, false), AllocationInfo{ .format = .immutableSizeZero });
    try ee(AllocationInfo.calc(0, 9, u8, false), AllocationInfo{ .format = @enumFromInt(9), .nIndexed = 2, .isObject = false });
    try ee(AllocationInfo.calc(10, 9, u8, false), IndexedFooter.init(.{ .format = .indexedNonObject, .nInstVars = 10, .nIndexed = 2, .size = 9, .isObject = false }));
    try ee(AllocationInfo.calc(10, 9, u16, false), IndexedFooter.init(.{ .format = .indexedNonObject, .nInstVars = 10, .nIndexed = 3, .size = 9, .isObject = false }));
    try ee(AllocationInfo.calc(10, 90, u64, false), IndexedFooter.init(.{ .format = .indexedNonObject, .nInstVars = 10, .nIndexed = 90, .size = 90, .isObject = false }));
    try ee(AllocationInfo.calc(0, 90, Object, false), AllocationInfo{ .format = .directIndexed, .nIndexed = 90, .size = 90 });
    try ee(AllocationInfo.calc(0, 90, u8, false), AllocationInfo{ .format = @enumFromInt(90), .nIndexed = 12, .isObject = false });
    try ee(AllocationInfo.calc(0, 90, u16, false), IndexedFooter.init(.{ .format = .indexedNonObject, .nIndexed = 23, .size = 90, .isObject = false }));
    try ee(AllocationInfo.calc(0, 127, u8, false), IndexedFooter.init(.{ .format = .indexedNonObject, .nIndexed = 16, .size = 127, .isObject = false }));
    try ee(AllocationInfo.calc(10, 90, Object, true), WeakIndexedFooter.init(.{ .format = .indexedWeakWithPointers, .nInstVars = 10, .nIndexed = 90, .size = 90 }));
    try ee(AllocationInfo.calc(10, 9000, Object, false), IndexedFooter.init(.{ .format = .external, .nInstVars = 10, .size = 9000 }));
    try ee(AllocationInfo.calc(10, 9000, Object, true), WeakIndexedFooter.init(.{ .format = .externalWeakWithPointers, .nInstVars = 10, .size = 9000 }));
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
    const markedBit = 1;
    const scannedBit = 2;
    const Static: Age = .static;
    // const ScanMask: u4 = GlobalScanned; // anded with this give 0 or Struct for non-global; Global, GlobalMarked or GlobalScanned for global (AoO or not)
    const Self = @This();
    pub const lastNurseryAge = @intFromEnum(Age.nurseryLast);
    inline fn needsPromotionTo(self: Self, referrer: Self) bool {
        switch (referrer) {
            .onStack => return false,
            .nursery, .nursery2, .nursery3, .nursery4, .nursery5, .nurseryLast => return self == .onStack,
            else => switch (self) {
                .onStack, .nursery, .nursery2, .nursery3, .nursery4, .nursery5, .nurseryLast => return true,
                else => return false,
            },
        }
    }
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

pub const AllocErrors = error{ Fail, HeapFull, NotIndexable, ObjectTooLarge, NeedNurseryCollection };
pub const AllocResult = struct {
    age: Age,
    allocated: HeapObjectPtr,
    info: AllocationInfo,
    pub fn initAll(self: *const AllocResult) HeapObjectPtr {
        const ptr = self.allocated;
        self.info.initContents(ptr);
        return ptr;
    }
};
pub const AllocReturn = AllocErrors!AllocResult;
const getHeader = HeapHeader.headerPtr;
pub const HeapHeader = packed struct(u64) {
    classIndex: ClassIndex = .none,
    hash: u24 = 0,
    format: Format = .free,
    immutable: bool = false,
    age: Age = .nursery,
    length: u11 = 0,
    forwarded: bool = false,
    pub const maxLength: u11 = 2047;
    pub const includesHeader = true;
    comptime {
        assert(@sizeOf(HeapHeader) == 8);
        std.debug.assert(std.meta.hasUniqueRepresentation(HeapHeader));
    }
    pub inline fn isForwarded(self: HeapHeader) bool {
        return self.forwarded;
    }
    pub inline fn array(self: HeapHeader, obj: HeapObjectConstPtr, elementSize: usize) ![]Object {
        return self.format.array(self, obj, elementSize);
    }
    pub inline fn instVars(self: HeapHeader, obj: HeapObjectConstPtr) ![]Object {
        return self.format.instVars(self, obj);
    }
    pub inline fn size(self: HeapHeader, obj: HeapObjectConstPtr) !usize {
        return self.format.size(self, obj);
    }
    pub inline fn objectInNursery(self: *HeapHeader, class: ClassIndex, objectSize: u11) void {
        self.* = .{ .classIndex = class, .hash = hashFromPtr(self), .format = .directIndexed, .age = .nursery, .length = objectSize };
    }
    pub inline fn headerOnStack(comptime class: ClassIndex, hash: u24, length: u11) HeapHeader {
        return .{ .classIndex = class, .hash = hash, .format = .special, .age = .onStack, .length = length };
    }
    pub inline fn freeHeader(length: u12) HeapHeader {
        return .{ .classIndex = .none, .hash = 0, .format = .free, .age = .free, .length = length };
    }
    pub inline fn storeFreeHeader(self: *HeapHeader) void {
        self.* = freeHeader(0);
    }
    pub inline fn staticHeaderWithLengthX(length: u12) HeapHeader {
        return .{ .classIndex = @enumFromInt(0), .hash = 0, .format = .special, .age = .static, .length = length };
    }
    pub inline fn staticHeaderWithClassAllocHash(classIndex: ClassIndex, ai: AllocationInfo, hash: u24) HeapHeader {
        return ai.heapHeader(classIndex, .static, hash);
    }
    pub inline fn staticHeaderWithClassLengthHash(classIndex: ClassIndex, length: u12, hash: u24) HeapHeader {
        return .{ .classIndex = classIndex, .hash = hash, .format = .special, .age = .static, .length = length };
    }
    pub inline fn simpleStackHeaderX(classIndex: ClassIndex, length: u12, hash: u24) HeapHeader {
        return .{ .classIndex = classIndex, .hash = hash, .format = .directIndexed, .age = .onStack, .length = length };
    }
    inline fn init(length: u12, format: Format, classIndex: ClassIndex, hash: u24, age: Age) HeapHeader {
        return .{
            .classIndex = classIndex,
            .hash = hash,
            .format = format,
            .age = age,
            .length = length,
        };
    }
    // pub inline fn headerPtr(self: anytype) *HeapHeader {
    //     return @as(*HeapHeader,@ptrCast(@as([*]HeapHeader,@ptrFromInt(@intFromPtr(self)))-1));
    // }
    pub inline fn calc(classIndex: ClassIndex, iVars: u11, hash: u24, age: Age, indexed: ?usize, comptime element: type, makeWeak: bool) !HeapHeader {
        const aI = AllocationInfo.calc(iVars, indexed, element, makeWeak);
        if (aI.requiresIndex()) return error.DoesntFit;
        return .{
            .classIndex = classIndex,
            .hash = hash,
            .format = aI.format,
            .age = age,
            .length = aI.nInstVars,
        };
    }
    pub inline fn withLength(self: HeapHeader, length: u12) HeapHeader {
        return .{
            .classIndex = self.classIndex,
            .hash = self.hash,
            .format = self.format,
            .age = self.age,
            .length = length,
        };
    }
    pub inline fn withHash(self: HeapHeader, hash: u24) HeapHeader {
        return .{
            .classIndex = self.classIndex,
            .hash = hash,
            .format = self.format,
            .age = self.age,
            .length = self.length,
        };
    }
    pub inline fn isOnStack(self: HeapHeader) bool {
        return self.age.isOnStack();
    }
    pub inline fn isStatic(self: HeapHeader) bool {
        return self.age.isStatic();
    }
    pub inline fn isNonHeap(self: HeapHeader) bool {
        return self.age.isNonHeap();
    }
    pub inline fn isUnmoving(self: HeapHeader) bool {
        return self.age.isUnmoving();
    }
    pub inline fn forwardedTo(self: HeapHeader) ?HeapObjectConstPtr {
        if (self.isForwarded())
            return @ptrFromInt(@as(u64, @bitCast(self)) & 0xffff_ffff_ffff);
        return null;
    }
    pub inline fn setHash(self: *HeapHeader, hash: u24) void {
        self.hash = hash;
    }
    pub inline fn setNArgs(self: *HeapHeader, args: u8) void {
        self.hash = (self.hash & 0xffff00) + args;
    }
    pub inline fn getClass(self: HeapHeader) ClassIndex {
        return self.classIndex;
    }
    pub inline fn hash16(self: HeapHeader) u16 {
        return @truncate(self.hash);
    }
    pub inline fn hasInstVars(self: HeapHeader) bool {
        return self.format.hasInstVars();
    }
    pub inline fn hasInstVarsWithPtrs(self: HeapHeader) bool {
        return self.format.hasInstVarsWithPtrs();
    }
    pub inline fn isIndexable(self: HeapHeader) bool {
        return self.format.isIndexable();
    }
    pub inline fn o(self: HeapHeader) Object {
        return @as(Object, @bitCast(self));
    }
};
pub const HeapObjectArray = [*]align(@alignOf(u64)) HeapObject;
pub const HeapObjectSlice = []align(@alignOf(u64)) HeapObject;
pub const HeapObjectPtr = *align(@alignOf(u64)) HeapObject;
pub const HeapObjectConstPtr = *align(@alignOf(u64)) const HeapObject;
pub const HeapObject = packed struct {
    header: HeapHeader,
    pub inline fn fillToBoundary(self: HeapObjectArray) HeapObjectArray {
        if (@intFromPtr(self) & 8 == 0)
            return self;
        self[0].header = HeapHeader.freeHeader(0);
        return self + 1;
    }
    pub inline fn headerPtr(self: *const HeapObject) *HeapHeader {
        return @constCast(&self.header);
    }
    pub inline fn setHeader(self: *const HeapObject, aHeader: HeapHeader) void {
        self.headerPtr().* = aHeader;
    }
    pub inline fn at(self: *const HeapObject, index: usize) HeapOperationError!u64 {
        const h = self.header;
        return h.format.at(h, self, index);
    }
    pub inline fn atPut(self: *HeapObject, index: usize, data: u64) HeapOperationError!void {
        const h = self.header;
        h.format.atPut(h, self, index, data);
    }
    pub inline fn size(self: *const HeapObject) HeapOperationError!usize {
        const h = self.header;
        return h.format.size(h, self);
    }
    pub inline fn iterator(self: *HeapObject) ?HeapObjectPtrIterator {
        const h = self.header;
        return h.format.pointerIterator(h, self);
    }
    pub inline fn isIndexable(self: HeapObjectConstPtr) bool {
        return self.header.isIndexable();
    }
    pub //inline
    fn isUnmoving(self: HeapObjectConstPtr) bool {
        return self.header.isUnmoving();
    }
    pub inline fn isForwarded(self: HeapObjectConstPtr) bool {
        return self.header.isForwarded();
    }
    pub inline fn isNursery(self: HeapObjectConstPtr) bool {
        return self.header.age.isNursery();
    }
    pub inline fn getClass(self: HeapObjectConstPtr) ClassIndex {
        return self.header.classIndex;
    }
    pub inline fn forwardedX(self: HeapObjectConstPtr) HeapObjectConstPtr {
        return self.header().forwardedTo() orelse self;
    }
    pub inline fn copyTo(self: HeapObjectPtr, hp: [*]HeapObject, reference: *Object) [*]HeapObject {
        const head = self.header;
        if (head.forwardedTo()) |_| { // already forwarded
            reference.* = switch (config.objectEncoding) {
                .nan => Nil, //@bitCast((reference.rawU() & 0xffff000000000000) + @as(u48, @truncate(@intFromPtr(target)))),
                .zag => Nil,
                else => unreachable,
            };
            return hp;
        }
        const len = head.length + 1;
        const newHp = hp + len;
        @memcpy(hp[0..len], @as([*]HeapObject, @ptrCast(self.start())));
        self.setHeader(@bitCast((@as(u64, @bitCast(HeapHeader{ .forwarded = true })) << 48) + @intFromPtr(hp + 1)));
        // ToDo: adjust header if necessary
        reference.* = switch (config.objectEncoding) {
            .nan => Nil, //@bitCast((reference.rawU() & 0xffff000000000000) + @intFromPtr(hp + 1)),
            .zag => Nil,
            .ptr => Nil,
            else => unreachable,
        };
        return newHp;
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
    pub inline fn skipForward(self: HeapObjectConstPtr) [*]HeapObject {
        const head = self.header;
        return @as([*]HeapObject, @constCast(@ptrCast(self))) + head.length + 1;
    }
    pub inline fn asSlice(maybeForwarded: HeapObjectConstPtr) ![]Object {
        const self = maybeForwarded.forwarded();
        return self.start[0..self.length];
    }
    pub inline fn asSliceWithoutHeader(maybeForwarded: HeapObjectConstPtr) ![]Object {
        const self = maybeForwarded.forwarded();
        return self.start[1..self.length];
    }
    pub //inline
    fn arrayAsSlice(self: HeapObjectConstPtr, comptime T: type) ![]T {
        const head = self.header;
        const arry = if (head.forwardedTo()) |realSelf|
            try realSelf.header.array(realSelf, @sizeOf(T))
        else
            try head.array(self, @sizeOf(T));
        const ba = @as([*]T, @constCast(@ptrCast(arry.ptr)));
        return ba[0..arry.len];
    }
    pub inline fn arraySize(self: HeapObjectConstPtr) !usize {
        const head = self.header;
        if (head.forwardedTo()) |realSelf| {
            return realSelf.header.size(realSelf);
        } else return head.size(self);
    }
    pub inline fn instVars(self: HeapObjectConstPtr) ![]Object {
        const head = self.header;
        if (head.forwardedTo()) |realSelf| {
            return realSelf.header.instVars(realSelf);
        }
        return head.instVars(self);
    }
    pub inline fn instVarPut(self: HeapObjectPtr, index: usize, obj: Object) !void {
        const head = self.header;
        const ivs = try self.instVars();
        if (index < 0 or index >= ivs.len) return error.indexOutOfRange;
        trace("\nbefore\n", .{});
        if (obj.asMemoryObject()) |otherHeapObject| {
            if (otherHeapObject.header.age.needsPromotionTo(head.age))
                return error.needsPromotion;
            const format = head.format;
            const newFormat = format.operations().instVarWithPtr;
            if (newFormat != format)
                self.header.format = newFormat;
        }
        ivs[index] = obj;
    }
    pub fn growSizeX(maybeForwarded: HeapObjectConstPtr, stepSize: usize) !usize {
        const self = maybeForwarded.forwarded();
        const form = self.format;
        if (!form.isIndexable()) return error.NotIndexable;
        var len: usize = self.length;
        if (form.hasInstVars()) {
            const oa = @as([*]u64, @ptrFromInt(@intFromPtr(self)));
            len = form.wordSize(oa[len + 1]);
        }
        len = largerPowerOf2(len * 2);
        if (len > HeapHeader.maxLength and len < HeapHeader.maxLength * 2) size = HeapHeader.maxLength;
        return (form.getSize() * len + stepSize - 1) / stepSize * stepSize;
    }
    pub inline fn inHeapSize(maybeForwarded: HeapObjectConstPtr) usize {
        const self = maybeForwarded.forwarded();
        const form = self.format;
        const len = self.length;
        if (form.isIndexable() and form.hasInstVars()) {
            const oa = @as([*]u64, @ptrFromInt(@intFromPtr(self))) + len;
            return len + 3 + if (oa[2] != @intFromPtr(oa + 3)) 0 else form.wordSize(oa[1]);
        }
        return len + 1;
    }
    pub inline fn isIndirect(maybeForwarded: HeapObjectConstPtr) bool {
        const self = maybeForwarded.forwarded();
        const form = self.format;
        const len: usize = self.length;
        if (!form.isIndexable()) return false;
        if (form.hasInstVars()) {
            const oa = @as([*]u64, @ptrFromInt(@intFromPtr(self))) + len;
            return oa[2] != @intFromPtr(oa + 3);
        }
        return false;
    }
    pub inline fn isRaw(self: HeapObjectConstPtr) bool {
        return self.format.isRaw();
    }
    pub //inline
    fn asObject(self: HeapObjectConstPtr) Object {
        return Object.from(self, null);
    }
    pub inline fn asObjectValue(self: HeapObjectConstPtr) Object {
        return @bitCast(self.*);
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
    pub inline fn asObjectArray(self: HeapObjectConstPtr) [*]align(@alignOf(u64)) Object {
        return @as([*]align(@alignOf(u64)) Object, @ptrFromInt(@intFromPtr(self))) + 1;
    }
    pub inline fn start(self: HeapObjectConstPtr) [*]Object {
        return @constCast(@ptrCast(self));
    }
    pub inline fn array(self: HeapObjectConstPtr, T: type) [*]T {
        return @constCast(@ptrCast(self));
    }
};
pub fn growSize(obj: anytype, comptime Target: type) !usize {
    const T = @TypeOf(obj);
    if (T == Object) return growSize(obj.arrayAsSlice(Target), Target);
    switch (@typeInfo(T)) {
        .pointer => |ptr| if (ptr.child != Target) @compileError("types must match " ++ @typeName(ptr.child) ++ " and " ++ @typeName(Target)),
        else => @compileError("only pointer types: " ++ @typeName(T)),
    }
    var size: u64 = (obj.len * @sizeOf(Target) + @sizeOf(Object) - 1) / @sizeOf(Object);
    size = largerPowerOf2(size * 2);
    // ToDo: use Format to round down to fit - i.e. consider number of footer words
    const maxLength: usize = HeapHeader.maxLength;
    if (size > maxLength - 4 and size < maxLength * 2 - 8) size = HeapHeader.maxLength;
    return (size * @sizeOf(Object) + @sizeOf(Target) - 1) / @sizeOf(Target);
}
test "growSize" {
    try std.testing.expectEqual(growSize(@as([]const u8, "foo"[0..]), u8), 16);
}
pub fn CompileTimeString(comptime str: []const u8) type {
    const size = str.len;
    const words = (size + @sizeOf(Object) - 1) / @sizeOf(Object);
    const fill = words * @sizeOf(Object) - size;
    const T = [size + fill]u8;
    return extern struct {
        header: HeapHeader align(8),
        chars: T align(8),
        const Self = @This();
        fn hash() u24 {
            var hsh: u64 = 0;
            for (str[0..@min(str.len, 6)]) |p|
                hsh = hsh *% 3 + p;
            return @truncate(hsh);
        }
        pub fn init() *const Self {
            var result = Self{
                .header = HeapHeader.staticHeaderWithClassAllocHash(.String, AllocationInfo.of(str), hash()),
                .chars = [_]u8{0} ** (size + fill),
            };
            for (str, result.chars[0..size]) |c, *r| {
                r.* = c;
            }
            const final = result;
            return &final;
        }
        fn h(self: *const Self) []const u8 {
            return @as([*]const u8, @ptrCast(self))[0 .. (size + 15) / 8 * 8];
        }
        pub fn obj(self: *const Self) HeapObjectConstPtr {
            return @alignCast(@ptrCast(self));
        }
        pub fn asObject(self: *const Self) Object {
            return Object.from(self.obj());
        }
    };
}
pub fn compileStrings(comptime tup: anytype) [tup.len]HeapObjectConstPtr {
    @setEvalBranchQuota(100000);
    comptime var result: [tup.len]HeapObjectConstPtr = undefined;
    inline for (tup, 0..) |name, idx| {
        result[idx] = comptime @as(HeapObjectConstPtr, CompileTimeString(name).init().obj());
    }
    const final = result;
    return final;
}

const abcde = CompileTimeString("abcdefghijklm").init().obj();
const strings = compileStrings(.{
    "Object", "SmallInteger", "Float", "False", "True",
});
test "compile time" {
    if (true) return error.SkipZigTest;
    try std.testing.expect(mem.eql(u8, abcde.asObject().arrayAsSlice(u8), "abcdefghijklm"));
}
test "compile time2" {
    try std.testing.expectEqual(try abcde.size(), 13);
    var buffer: [20]u8 = undefined;
    _ = try abcde.asObject().asZeroTerminatedString(&buffer);
}
test "compile time3" {
    if (!debugError)
        try std.testing.expect(mem.eql(u8, try Object.from(abcde, null).arrayAsSlice(u8), "abcdefghijklm"));
}
test "compile time4" {
    try std.testing.expect(mem.eql(u8, try strings[3].arrayAsSlice(u8), "False"));
}
