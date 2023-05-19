const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const ClassIndex = object.ClassIndex;
const utilities = @import("utilities.zig");
const largerPowerOf2 = utilities.largerPowerOf2;
const inversePhi = utilities.inversePhi;

pub const Format = enum(u8) {
    immediateSizeZero = ImmediateSizeZero,
    immediateByteMax = ImmediateByteMax,
    notIndexable, // this and below have no pointers in array portion
    immediateObjectOne,
    immediateObjectMax = ImmediateObjectMax,
    header, // this is a header word, which points to the proper HeapObject footer word
    directIndexed,
    directIndexedWithPointers, // this and below have no size/pointer
    indexed,
    indexedWithPointers,
    external,
    externalWithPointers, // this and below have no weak queue link
    _externalWeak_, // never created
    externalWeakWithPointers,
    _weak_, // never created
    weakWithPointers,
    _,
    const Self = @This();
    const ImmediateSizeZero: u8 = 0;
    const ByteOffset = ImmediateSizeZero;
    const ImmediateByteMax = NotIndexable - 1;
    const NumberOfBytes = ImmediateByteMax - ByteOffset;
    const NotIndexable = 64;
    const ObjectOffset = NotIndexable;
    const ImmediateObjectOne = ObjectOffset + 1;
    const ImmediateObjectMax = DirectIndexed - 2;
    const NumberOfObjects = ImmediateObjectMax - ObjectOffset;
    const DirectIndexed = Indexed - 2;
    const Indexed = External - 2;
    const External = ExternalWeakWithPointers - 3;
    const ExternalWeakWithPointers = WeakWithPointers - 2;
    const WeakWithPointers = Immutable - 1;
    const Pointers: u8 = 1;
    const Immutable : u8 = 128;
    const Size = union(enum) {
        size: u8,
        indexable,
        notIndexable,
        const Indexable = Size{.indexable={}};
        const NotIndexable = Size{.notIndexable={}};
    };
    pub inline fn size(self: Self) Size {
        const s = @enumToInt(self) & ~Immutable;
        return switch (s) {
            ImmediateSizeZero ... ImmediateByteMax => Size{.size = s},
            ImmediateObjectOne ... ImmediateObjectMax => Size{.size = s & ImmediateByteMax},
            NotIndexable => Size.NotIndexable,
            else => Size.Indexable,
        };
    }
    pub inline fn isObjectSize(s: usize) bool {
        return s <= NumberOfObjects;
    }
    pub inline fn objectSize(s: usize) Self {
        if (s==0) return .immediateSizeZero;
        if (isObjectSize(s))
            return @intToEnum(Self,s-1+ImmediateObjectOne);
        return .indexed;
    }
    pub inline fn isByteSize(s: usize) bool {
        return s <= NumberOfBytes;
    }
    pub inline fn byteSize(s: usize) Self {
        if (isByteSize(s))
            return @intToEnum(Self,s);
        return .indexed;
    }
    pub inline fn isHeader(self: Self) bool {
        return self == .header;
    }
    pub inline fn isIndexableWithPointers(self: Self) bool {
        return self == .indexedWithPointers;
    }
    pub inline fn isIndexable(self: Self) bool {
        return self != .notIndexable;
    }
    pub inline fn isWeak(self: Self) bool {
        return self  == .weakWithPointers;
    }
    pub inline fn hasIndexPointers(self: Self) bool {
        return @enumToInt(self) > Indexed;
    }
//    pub inline fn hasIndexFields(self: Self) bool {
//        return @enumToInt(self) >= Indexed;
//    }
    pub inline fn isExternal(self: Self) Self {
        return switch (@enumToInt(self)) {
            External ... ExternalWeakWithPointers => true,
            else => false};
    }
    pub inline fn immutable(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | Immutable);
    }
    pub inline fn isImmutable(self: Self) bool {
        return @enumToInt(self) & Immutable != 0;
    }
    const Iterator = *const fn(obj:HeapObjectConstPtr) HeapObjectPtrIterator;
    pub fn iterator(self: Self) Iterator {
        switch (self) {
            .immediateSizeZero ... .indexable => return HeapObjectPtrIterator.ivPointers,
            .weak => return HeapObjectPtrIterator.weakDefault,
            else => return HeapObjectPtrIterator.bothPointers
        }
    }
    fn eq(f: Self, v: u8) !void {
        return std.testing.expectEqual(@intToEnum(Self,v),f);
    }
    pub fn allocationInfo(iVars: u12, indexed: ?usize, elementSize: usize, makeWeak: bool) AllocationInfo {
        if (indexed) |nElements| {
            const maxSize = HeapObject.maxLength;
            const arraySize = (nElements*elementSize+@sizeOf(Object)-1)/@sizeOf(Object);
            if (makeWeak) {
                if (iVars+arraySize>maxSize-3)
                    return .{.format=.externalWeakWithPointers,.size=iVars,.sizeField=3};
                return .{.format=.weakWithPointers,.size=iVars+@intCast(u12,arraySize),.sizeField=3};
            }
            if (nElements==0 or (elementSize==1 and nElements<=NumberOfBytes))
                return .{.format=@intToEnum(Self,nElements+ByteOffset),.size=iVars+@intCast(u12,arraySize)};
            if (elementSize==@sizeOf(Object)) {
                if (nElements<=NumberOfObjects)
                    return .{.format=@intToEnum(Self,nElements+ObjectOffset),.size=iVars+@intCast(u12,arraySize)};
                if (iVars==0 and nElements<=maxSize)
                    return .{.format=.directIndexed,.size=@intCast(u12,arraySize)};
            }
            if (iVars+arraySize>maxSize-2)
                return .{.format=.external,.size=iVars,.sizeField=2};
            return .{.format=.indexed,.size=iVars+@intCast(u12,arraySize),.sizeField=2};
        }
        if (makeWeak)
            return .{.format=.weakWithPointers,.size=iVars,.sizeField=3};
        return .{.format=.notIndexable,.size=iVars};
    }
};
pub const AllocationInfo = struct {
    format: Format,
    size: u12,
    sizeField: u8 = 0,
    const Self = @This();
    pub inline fn moreThanOneWord(self: Self) bool {
        return self.sizeField>0;
    }
    pub inline fn objectSize(self: Self, maxLength: u12) !u12 {
        const size = self.size+self.sizeField;
        if (size>maxLength) return error.ObjectTooLarge;
        return size;
    }
    pub inline fn needsExternalAllocation(self: Self) bool {
        return self.format.isExternal();
    }
    pub inline fn fillFooters(self: Self, theHeapObject: HeapObjectPtr, classIndex: u16, age: Age, nElements: usize, elementSize: usize) bool {
        const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(theHeapObject)>>4)*%object.u32_phi_inverse>>8);
        theHeapObject.* =  HeapObject {
            .classIndex = classIndex,
            .hash = hash,
            .objectFormat = self.format,
            .age = age,
            .length = self.size+self.sizeField,
        };
        if (self.sizeField>0) {
            const size = self.sizeField;
            const footers = @ptrCast([*]u64,theHeapObject)-size;
            footers[size-1] = nElements;
            footers[size-2] = @ptrToInt(footers)-elementSize*nElements;
        }
        return self.format.isExternal();
    }
};
test "raw size" {
    const ee = std.testing.expectEqual;
    try ee(@intToEnum(Format,7).size(),Format.Size{.size=7});
    try ee(@intToEnum(Format,125).size(),Format.Size.Indexable);
    try ee(@intToEnum(Format,64).size(),Format.Size.NotIndexable);
    try ee(@intToEnum(Format,70).size(),Format.Size{.size=6});
    try ee(Format.immediateSizeZero.size(),Format.Size{.size=0});
}
test "HeapObject formats" {
    const expect = std.testing.expect;
    try expect(Format.immediateSizeZero.immutable().isImmutable());
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
    try expect(!Format.immediateByteMax.isWeak());
}
test "allocationInfo" {
    const ee = std.testing.expectEqual;
    // allocationInfo(iVars: u12, indexed: ?usize, eSize: ?usize, mSize: ?usize, makeWeak: bool)
    try ee(Format.allocationInfo(10,null,0,false),AllocationInfo{.format=.notIndexable,.size=10});
    try ee(Format.allocationInfo(10,null,0,true),AllocationInfo{.format=.weakWithPointers,.size=10,.sizeField=3});
    try ee(Format.allocationInfo(10,0,0,false),AllocationInfo{.format=.immediateSizeZero,.size=10});
    try ee(Format.allocationInfo(10,9,8,false),AllocationInfo{.format=@intToEnum(Format,Format.ObjectOffset+9),.size=19});
    try ee(Format.allocationInfo(10,9,1,false),AllocationInfo{.format=@intToEnum(Format,Format.ImmediateSizeZero+9),.size=12});
    try ee(Format.allocationInfo(10,9,2,false),AllocationInfo{.format=.indexed,.size=13,.sizeField=2});
    try ee(Format.allocationInfo(10,90,8,false),AllocationInfo{.format=.indexed,.size=100,.sizeField=2});
    try ee(Format.allocationInfo(0,90,8,false),AllocationInfo{.format=.directIndexed,.size=90});
    try ee(Format.allocationInfo(0,90,1,false),AllocationInfo{.format=.indexed,.size=12,.sizeField=2});
    try ee(Format.allocationInfo(0,90,2,false),AllocationInfo{.format=.indexed,.size=23,.sizeField=2});
    try ee(Format.allocationInfo(10,90,8,true),AllocationInfo{.format=.weakWithPointers,.size=100,.sizeField=3});
    try ee(Format.allocationInfo(10,9000,8,false),AllocationInfo{.format=.external,.size=10,.sizeField=2});
    try ee(Format.allocationInfo(10,9000,8,true),AllocationInfo{.format=.externalWeakWithPointers,.size=10,.sizeField=3});
}
pub const Age = enum(u4) {
    onStack,
    nursery,
    nursery1, nursery2, nursery3, nursery4, nurseryLast,
    static,
    global, globalMarked,
    aStruct,
    globalScanned,
    aoo, aooMarked,
    free,
    aooScanned,
    const Static: Age = .static;
    // const ScanMask: u4 = GlobalScanned; // anded with this give 0 or Struct for non-global; Global, GlobalMarked or GlobalScanned for global (AoO or not)
    const Self = @This();
    pub inline fn isAoO(self: Self) bool {
        return switch (self) {
            .aoo, .aooMarked, .aooScanned => true,
            else => false};
    }
    pub inline fn isUnmoving(self: Self) bool {
        return switch(self)  {
            .static, .global, .globalMarked, .aStruct, .globalScanned, .aoo, .aooMarked, .free, .aooScanned => true,
            else => false};
    }
    pub inline fn isGlobal(self: Self) bool {
        return switch(self) {
            .global, .globalMarked, .aStruct, .globalScanned, .aoo, .aooMarked, .free, .aooScanned => true,
            else => false};
    }
    pub inline fn isNonHeap(self: Self) bool {
        return switch (self) {
            .static,.onStack => true,
            else => false};
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
            else => false};
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
    current: [*] const Object,
    beyond: [*] const Object,
    pub fn weakDefault(_:HeapObjectConstPtr) HeapObjectPtrIterator {
        @panic("weakDefault called");
    }
    pub fn noPointers(_:HeapObjectConstPtr) HeapObjectPtrIterator {
        return .{
            .nextPointer = allDone,
            .scanObject = undefined,
            .current = undefined,
            .beyond = undefined,
        };
    }
    pub fn ivPointers(obj:HeapObjectConstPtr) HeapObjectPtrIterator { // only ivs, or both with only iv pointers
        if (!obj.hasInstVarsWithPtrs()) return noPointers(obj);
        const ivs = @ptrCast([*] const Object,obj);
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = ivs+1,
            .beyond = ivs+1+obj.length,
        };
    }
    pub fn arrayPointers(obj:HeapObjectConstPtr) HeapObjectPtrIterator { // only array
        if (!obj.isIndexableWithPtrs()) return noPointers(obj);
        const ivs = @ptrCast([*] const Object,obj);
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = ivs+1,
            .beyond = ivs+1+obj.length,
        };
    }
    pub fn bothPointers(obj:HeapObjectConstPtr) HeapObjectPtrIterator { // both with iv and array pointers
        if (!obj.hasInstVarsWithPtrs()) return bothOnlyArrayPointers(obj);
        if (!obj.isIndexableWithPtrs()) return ivPointers(obj);
        const ivs = @ptrCast([*] const Object,obj);
        return .{
            .nextPointer = firstPointerGroup,
            .scanObject = obj,
            .current = ivs+1,
            .beyond = ivs+1+obj.length,
        };
    }
    pub fn bothOnlyArrayPointers(obj:HeapObjectConstPtr) HeapObjectPtrIterator { // both with no iv pointers
        if (!obj.isIndexableWithPtrs()) return noPointers(obj);
        const ivs = @ptrCast([*] const Object,obj);
        const size = ivs[1+obj.length].u();
        const array = @intToPtr([*]Object,ivs[2+obj.length].u());
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = array,
            .beyond = array+size-1,
        };
    }
    fn lastPointerGroup(self:*Self) ?HeapObjectPtr {
        while (@ptrToInt(self.current)<@ptrToInt(self.beyond)) {
            const obj = self.current[0];
            self.current += 1;
            if (obj.isHeapAllocated())
                return obj.toUnchecked(HeapObjectPtr);
        }
        self.nextPointer = allDone;
        return null;
    }
    fn firstPointerGroup(self:*Self) ?HeapObjectPtr {
        while (@ptrToInt(self.current)<@ptrToInt(self.beyond)) {
            const obj = self.current[0];
            self.current += 1;
            if (obj.isHeapAllocated())
                return obj.toUnchecked(HeapObjectPtr);
        }
        const obj = self.scanObject;
        const ivs = @ptrCast([*]const Object,obj);
        const size = ivs[1+obj.length].u();
        const array = @intToPtr([*]Object,ivs[2+obj.length].u());
        self.current = array;
        self.beyond = array+size-1;
        self.nextPointer = lastPointerGroup;
        return self.lastPointerGroup();
    }
    fn allDone(_:*Self) ?HeapObjectPtr {
        return null;
    }
    //inline
        fn next(self:*Self) ?HeapObjectPtr {
        return self.nextPointer(self);
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
//     o1[4] = @bitCast(Object,@ptrToInt(&o1[5]));
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

pub const AllocErrors = error {Fail,HeapFull,NotIndexable,ObjectTooLarge};

pub const HeapObjectArray = [*]align(@alignOf(u64)) HeapObject;
pub const HeapObjectSlice = []align(@alignOf(u64)) HeapObject;
pub const HeapObjectPtr = *align(@alignOf(u64)) HeapObject;
pub const HeapObjectConstPtr = *align(@alignOf(u64)) const HeapObject;
pub const HeapObject = packed struct(u64) {
        classIndex: u16,
        hash: u24,
        objectFormat: Format,
        age: Age,
        length: u12,

    const immediateLength: u16 = 4095; // all immediate objects (except doubles) have this as top 12 bits
    const forwardLength: u16 = 4094;
    pub const maxLength = 4093;
    pub const includesHeader = true;
    pub fn iterator(self: HeapObjectConstPtr) Format.Iterator {
        return self.objectFormat.iterator();
    }
    pub fn makeIterator(self: HeapObjectConstPtr) HeapObjectPtrIterator {
        return self.objectFormat.iterator()(self);
    }
    const partialHeader = @bitCast(u64,HeapObject{.classIndex=0,.hash=0,.objectFormat=.header,.age=.onStack,.length=0});
    pub inline fn partialOnStack(selfOffset: u16) HeapObject {
        return @bitCast(HeapObject,partialHeader | @as(u64,selfOffset)<<16);
    }
    pub inline fn partialWithLength(size: u12) HeapObject {
        return HeapObject{.classIndex=0,.hash=0,.objectFormat=.header,.age=.static,.length=size};
    }
    pub inline fn realHeapObject(self: HeapObjectConstPtr) HeapObjectPtr {
        const result = if (self.objectFormat.isHeader())
            @ptrCast(HeapObjectPtr,@ptrCast(HeapObjectArray,@constCast(self))+self.length+1)
            else @constCast(self);
        return result;
    }
    inline fn init(length : u12, format : Format, classIndex : u16, hash: u24, age: Age) HeapObject {
        return HeapObject {
            .classIndex = classIndex,
            .hash = hash,
            .objectFormat = format,
            .age = age,
            .length = length,
        };
    }
    pub inline fn calcHeapObject(iVars: u12, classIndex: u16, hash: u24, age: Age, indexed: ?usize, elementSize: usize, makeWeak: bool) !HeapObject {
        const aI = comptime Format.allocationInfo(iVars,indexed,elementSize,makeWeak);
        if (aI.moreThanOneWord()) return error.DoesntFit;
        return HeapObject {
            .classIndex = classIndex,
            .hash = hash,
            .objectFormat = aI.format,
            .age = age,
            .length = aI.size,
        };
    }
    pub inline fn setFooters(self: HeapObjectPtr, iVars: u12, classIndex: u16, hash: u24, age: Age, indexed: ?usize, elementSize: ?usize, mSize: ?usize, makeWeak: bool) void {
        return Format.allocationInfo(iVars,indexed,elementSize,mSize,makeWeak).fillFooters(self,classIndex,hash,age,indexed,elementSize);
    }
    pub inline fn setFields(self: HeapObjectPtr, fill: Object, age: ?Age) void {
        if (fill==Nil and !self.format.isExternal()) {
            unreachable;
            //mem.set(Object,result.wholeObjectSlice(),fill);
            //return;
        } else {
            unreachable;
        }
        _=age;// if (age) |newAge| self.age = newAge;
    }
    pub inline fn isOnStack(self: HeapObjectConstPtr) bool {
        return self.age.isOnStack();
    }
    pub inline fn isUnmoving(self: HeapObjectConstPtr) bool {
        return self.age.isUnmoving();
    }
    pub inline fn isStack(self: HeapObjectConstPtr) bool {
        return self.age.isStack();
    }
    pub inline fn forwardedTo(self: HeapObjectConstPtr) HeapObjectConstPtr {
        return @intToPtr(HeapObjectConstPtr,@intCast(u64,@intCast(i64,@bitCast(u64,self.*)<<16)>>16));
    }
    pub inline fn isForwarded(self: HeapObjectConstPtr) bool {
        return self.length==forwardLength;
    }
    pub inline fn forwarded(self: HeapObjectConstPtr) HeapObjectConstPtr {
        if (self.isForwarded()) {
            return self.forwardedTo();
        }
        return self;
    }
    pub inline fn asSlice(self: HeapObjectConstPtr) ![]Object {
        const head = self.*;
        const size = head.length;
        if (size==forwardLength) {
            const realSelf = self.forwardedTo();
            const start = @intToPtr([*]Object,@ptrToInt(realSelf)-@sizeOf(Object)*realSelf.length);
            return start[0..size];
        } else {
            const start = @intToPtr([*]Object,@ptrToInt(self)-@sizeOf(Object)*size);
            return start[0..size];
        }
    }
    pub inline fn asSliceWithoutHeader(self: HeapObjectConstPtr) ![]Object {
        const head = self.*;
        const size = head.length;
//        std.io.getStdErr().writer().print("\naASWH {}",.{head}) catch unreachable;
        if (size==forwardLength) {
            const realSelf = self.forwardedTo();
            const start = @intToPtr([*]Object,@ptrToInt(realSelf)-@sizeOf(Object)*realSelf.length);
            return start[1..size];
        } else {
            const start = @intToPtr([*]Object,@ptrToInt(self)-@sizeOf(Object)*size);
            return start[1..size];
        }
    }
    pub inline fn arrayAsSlice(self: HeapObjectConstPtr,comptime T:type) ![]T {
        const head = self.*;
//        std.io.getStdErr().writer().print("\naAS {}",.{head}) catch unreachable;
        if (head.length==forwardLength) {
            const realSelf = self.forwardedTo();
            return realSelf.arrayAsSlice_(realSelf.*,T);
        } else
            return self.arrayAsSlice_(head,T);
    }
    inline fn arrayAsSlice_(self: HeapObjectConstPtr, head: HeapObject, comptime T:type) ![]T {
        //        if (head.age.isOnStack()) unreachable;
//        std.io.getStdErr().writer().print("\nsize={} 0x{x} {}",.{head.objectFormat.size(),@ptrToInt(self),head}) catch unreachable;
        switch (head.objectFormat.size()) {
            .notIndexable => return error.NotIndexable,
            .size => |s| {
                const oa = @intToPtr([*]T,@ptrToInt(self)-@sizeOf(T)*s);
                return oa[0..s];
            },
            .indexable => {
                const oa = @intToPtr([*]usize,@ptrToInt(self)-@sizeOf(usize)*2);
                return @intToPtr([*]T,oa[0])[0..oa[1]];
            },
        }
    }
    pub inline fn arraySize(maybeForwarded: HeapObjectConstPtr) !usize {
        const self = maybeForwarded.forwarded();
        switch (self.objectFormat.size()) {
            .notIndexable => return error.NotIndexable,
            .size => |s| {
                return s;
            },
            .indexable => {
                const oa = @intToPtr([*]u64,@ptrToInt(self)-@sizeOf(u64));
                return oa[0];
            },
        }
    }
    pub fn growSizeX(maybeForwarded: HeapObjectConstPtr, stepSize: usize) !usize {
        const self = maybeForwarded.forwarded();
        const form = self.objectFormat;
        if (!form.isIndexable()) return error.NotIndexable;
        var size: usize = self.length;
        if (form.hasInstVars()) {
            const oa = @intToPtr([*]u64,@ptrToInt(self));
            size = form.wordSize(oa[size+1]);
        }
        size = largerPowerOf2(size * 2);
        if (size>HeapObject.maxLength and size<HeapObject.maxLength*2) size = HeapObject.maxLength;
        return (form.getSize()*size+stepSize-1)/stepSize*stepSize;
    }
    pub inline fn inHeapSize(maybeForwarded: HeapObjectConstPtr) usize {
        const self = maybeForwarded.forwarded();
        const form = self.objectFormat;
        const size = self.length;
        if (form.isIndexable() and form.hasInstVars()) {
            var oa = @intToPtr([*]u64,@ptrToInt(self))+size;
            return size+3+if (oa[2]!=@ptrToInt(oa+3)) 0 else form.wordSize(oa[1]);
        }
        return size+1;
    }
    pub inline fn isIndirect(maybeForwarded: HeapObjectConstPtr) bool {
        const self = maybeForwarded.forwarded();
        const form = self.objectFormat;
        var size : usize = self.length;
        if (!form.isIndexable()) return false;
        if (form.hasInstVars()) {
            var oa = @intToPtr([*]u64,@ptrToInt(self))+size;
            return oa[2]!=@ptrToInt(oa+3);
        }
        return false;
    }
    pub inline fn isRaw(self: HeapObjectConstPtr) bool {
        return self.objectFormat.isRaw();
    }
    pub inline fn asObject(self: HeapObjectConstPtr) Object {
        return Object.from(self);
    }
    pub inline fn asObjectConstPtr(self: HeapObjectConstPtr) [*]const Object {
        return @ptrCast([*]const Object,self);
    }
    pub inline fn asObjectPtr(self: HeapObjectPtr) [*]Object {
        return @ptrCast([*]Object,self);
    }
    pub inline fn fromObjectPtr(op:  [*]const Object) HeapObjectArray {
        return @intToPtr(HeapObjectArray,@ptrToInt(op));
    }
    pub inline fn o(self: HeapObject) Object {
        return @bitCast(Object,self);
    }
    pub inline fn asObjectArray(self: HeapObjectConstPtr) [*]align(@alignOf(u64)) Object {
        return @intToPtr([*]align(@alignOf(u64)) Object, @ptrToInt(self)) + 1;
    }
    pub inline fn setHash(self: HeapObjectPtr,hash: u24) HeapObject {
        self.hash=hash;
        return self.*;
    }
    pub inline fn setNArgs(self: HeapObjectPtr,args: u8) HeapObject {
        self.hash=(self.hash & 0xffff)+(args<<16);
        return self.*;
    }
    pub inline fn getClass(self: HeapObjectConstPtr) ClassIndex {
        return self.classIndex;
    }
    pub inline fn hash16(self: HeapObject) u16 {
        return @truncate(u16,@bitCast(u64,self)>>16);
    }
    pub inline fn hasInstVars(self: HeapObjectConstPtr) bool {
        return self.objectFormat.hasInstVars();
    }
    pub inline fn hasInstVarsWithPtrs(self: HeapObjectConstPtr) bool {
        return self.objectFormat.hasInstVarsWithPtrs();
    }
    pub inline fn isIndexable(self: HeapObjectConstPtr) bool {
        return self.objectFormat.isIndexable();
    }
    pub inline fn isIndexableWithPtrs(self: HeapObjectConstPtr) bool {
        return self.objectFormat.isIndexableWithPtrs();
    }
    pub inline fn instVars(self: HeapObjectConstPtr) []Object {
        if (self.objectFormat.hasInstVars()) {
            const size = self.length;
            return self.asObjectArray()[0..size];
        } else return &[0]Object{};
    }
    fn @"format FUBAR"(
        self: HeapObjectConstPtr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        _ = fmt;
        _ = options;
        if (self.objectFormat.isRaw()) {
            if (self.objectFormat.is8() and self.classIndex==object.String_I) {
                try writer.print("{s}",.{self.indexables(u8)});
            } else if (self.objectFormat.is8()) {
                try writer.print("raw[{}]",.{self.indexables(u8).len});
            } else if (self.objectFormat.is16()) {
                try writer.print("raw[{}]",.{self.indexables(u16).len});
            } else if (self.objectFormat.is32()) {
                try writer.print("raw[{}]",.{self.indexables(u32).len});
            } else try writer.print("raw[{}]",.{self.indexables(u64).len});
        } else {
            var blank = false;
            const ivs = self.instVars();
            if (ivs.len>0) {
                try writer.print("#(",.{});
                for (ivs) |item| {
                    if (blank) try writer.print(" ",.{});
                    blank = true;
                    try writer.print("{}",.{item});
                }
                try writer.print(")",.{});
            }
            if (self.objectFormat.isIndexable()) {
                const idx = self.indexables(Object);
                if (blank) try writer.print(" ",.{});
                blank = false;
                try writer.print("{c}",.{'{'});
                for (idx) |item| {
                    if (blank) try writer.print(" ",.{});
                    blank = true;
                    try writer.print("{}",.{item});
                }
                try writer.print("{c}",.{'}'});
            }
        }
    }
};
pub fn growSize(obj: anytype, comptime Target:type) !usize {
    const T = @TypeOf(obj);
    if (T == Object) return growSize(obj.arrayAsSlice(Target),Target);
    switch (@typeInfo(T)) {
        .Pointer => |ptr| if(ptr.child != Target) @compileError("types must match " ++ @typeName(ptr.child) ++ " and " ++ @typeName(Target)),
        else => @compileError("only pointer types: " ++ @typeName(T)),
    }
    var size = (obj.len * @sizeOf(Target) + @sizeOf(Object) - 1)/@sizeOf(Object);
    size = largerPowerOf2(size * 2);
    // ToDo: use Format to round down to fit - i.e. consider number of footer words
    if (size>HeapObject.maxLength and size<HeapObject.maxLength*2) size = HeapObject.maxLength;
    return (size*@sizeOf(Object)+@sizeOf(Target)-1)/@sizeOf(Target);
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
fn hash24(str: [] const u8) u24 {
    const phi: u32 = inversePhi(u24);
    var hash = phi*%@truncate(u32,str.len+%1);
    for (str,0..) |c,idx| {
        if (idx>9) break;
        hash +%= phi*%c;
    }
    return @truncate(u24,hash);
}
pub fn CompileTimeString(comptime str: [] const u8) type {
    const size = str.len;
    const words = (size+@sizeOf(Object)-1)/@sizeOf(Object);
    const fill = words*@sizeOf(Object)-size;
    const hash = hash24(str);
    return extern struct {
        chars: [size+fill]u8,
        footer: HeapObject,
        const Self = @This();
        pub fn init() Self {
            var result = Self {
                .footer = footer(words,@intToEnum(Format,size),object.String_I,hash,Age.static),
                .chars = [_]u8{0}**(size+fill),
            };
            for (str,result.chars[fill..]) |c,*r| {
                r.*=c;
            }
            return result;
        }
        fn h(self: * const Self) []const u8 {
            return @ptrCast([*]const u8,self)[0..(size+15)/8*8];
        }
        fn obj(self: * const Self) HeapObjectConstPtr {
            return @ptrCast(*const HeapObject,&self.footer);
        }
        pub fn asObject(self: * const Self) Object {
            return Object.from(self.obj());
        }
    };
}
pub fn compileStrings(comptime tup: anytype) [tup.len] HeapObjectConstPtr {
    @setEvalBranchQuota(3000);
    comptime var result : [tup.len] HeapObjectConstPtr = undefined;
    inline for (tup,0..) |name,idx| {
        result[idx] = comptime (&CompileTimeString(name).init()).obj();
    }
    return result;
}

const abcde = CompileTimeString("abcdefghijklm").init();
const strings = compileStrings(.{ // must be in same order as above
    "Object", "SmallInteger", "Float", "False", "True",
});
test "compile time" {
//    const abcde_: []const u8 = "abcdefghijklm";
//    try std.testing.expectEqual(abcde_, abcde.asObject().arrayAsSlice(u8));
    std.debug.print("abcde: {any}\n",.{abcde.obj().*});
    std.debug.print("abcde: {any}\n",.{abcde.asObject().arrayAsSlice(i8)});
    std.debug.print("abcde: {any}\n",.{try abcde.obj().arrayAsSlice(i8)});
    std.debug.print("abcde: {any}\n",.{abcde.h()});
    std.debug.print("abcde: {any}\n",.{abcde.asObject()});
    std.debug.print("strings[3]: {any}\n",.{strings[3].*});
    std.debug.print("strings[3]: {any}\n",.{strings[3].arrayAsSlice(i8)});
}
