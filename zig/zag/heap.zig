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
    immediateByteMax = NotIndexable - 1,
    notIndexable, // this and below have no pointers in array portion
    immediateObjectOne,
    immediateObjectMax = ImmediateObjectMax, // this and below have no size/pointer
    context,
    indexed,
    indexedWithPointers, // this and below have no weak queue
    _weak_, // never created
    weakWithPointers,
    _,
    const Self = @This();
    const ImmediateSizeZero: u8 = 0;
    const ImmediateByteMax = NotIndexable - 1;
    const NotIndexable = 64;
    const ImmediateObjectOne = NotIndexable + 1;
    const ImmediateObjectMax = Indexed - 2;
    const Indexed = WeakWithPointers - 3;
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
        return s <= ImmediateObjectMax-ImmediateObjectOne+1;
    }
    pub inline fn objectSize(s: usize) Self {
        if (s==0) return .immediateSizeZero;
        return @intToEnum(Self,s-1+ImmediateObjectOne);
    }
    pub inline fn isByteSize(s: usize) bool {
        return s <= ImmediateByteMax-ImmediateSizeZero;
    }
    pub inline fn byteSize(s: usize) Self {
        return @intToEnum(Self,s);
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
    pub inline fn hasIndexFields(self: Self) bool {
        return @enumToInt(self) >= Indexed;
    }
    pub inline fn immutable(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | Immutable);
    }
    pub inline fn isImmutable(self: Self) bool {
        return @enumToInt(self) & Immutable != 0;
    }
    const Iterator = *const fn(obj:HeapConstPtr) HeapPtrIterator;
    pub fn iterator(self: Self) Iterator {
        switch (self) {
            .immediateSizeZero ... .indexable => return HeapPtrIterator.ivPointers,
            .weak => return HeapPtrIterator.weakDefault,
            else => return HeapPtrIterator.bothPointers
        }
    }
    fn eq(f: Self, v: u8) !void {
        return std.testing.expectEqual(@intToEnum(Self,v),f);
    }
};
test "raw size" {
    try std.testing.expectEqual(@intToEnum(Format,7).size(),Format.Size{.size=7});
    try std.testing.expectEqual(@intToEnum(Format,125).size(),Format.Size.Indexable);
    try std.testing.expectEqual(@intToEnum(Format,64).size(),Format.Size.NotIndexable);
    try std.testing.expectEqual(@intToEnum(Format,70).size(),Format.Size{.size=6});
    try std.testing.expectEqual(Format.immediateSizeZero.size(),Format.Size{.size=0});
}

test "header formats" {
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
pub const Age = enum(u4) {
    incompleteContext = IncompleteContext,
    nursery = NurseryFirst,
    global = Global,
    aoo = AoO, aooMarked = AoOMarked, aooScanned = AoOScanned,
    static = Static,
    aStruct = Struct,
    free = Free,
    _,
    const NurseryFirst: u4 = 0;
    const NurseryLast: u4 = 5;
    const IncompleteContext: u4 = 6;
    const Static: u4 = 7;
    const Global: u4 = 8;
    const GlobalMarked: u4 = 9;
    const Struct: u4 = 10;
    const GlobalScanned: u4 = 11;
    const AoO : u4 = 12;
    const AoOMarked : u4 = 13;
    const Free : u4 = 14;
    const AoOScanned : u4 = 15;
    const ScanMask: u4 = GlobalScanned; // anded with this give 0 or Struct for non-global; Global, GlobalMarked or GlobalScanned for global (AoO or not)
    const Self = @This();
    pub inline fn isAoO(self: Self) bool {
        return switch (self) {
            .aoo, .aooMarked, .aooScanned => true,
            else => false,
        };
    }
    pub inline fn isUnmoving(self: Self) bool {
        return @enumToInt(self) >= Static;
    }
    pub inline fn isGlobal(self: Self) bool {
        return @enumToInt(self) >= Global;
    }
    pub inline fn isNonHeap(self: Self) bool {
        return switch (@enumToInt(self)) {
            Static,IncompleteContext => true,
            else => false,
        };
    }
    pub inline fn isStatic(self: Self) bool {
        return self == .static;
    }
    pub inline fn isIncompleteContext(self: Self) bool {
        return self == .incompleteContext;
    }
    pub inline fn marked(self: Self) Self {
        return switch (self) {
            .global,.aoo => @intToEnum(Self,@enumToInt(self) | 1),
            else => self,
        };
    }
    pub inline fn scanned(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | 2);
    }
    // Note: assigning a ptr to a scanned object must block for collection
};
pub const HeapPtrIterator = struct {
    const Self = @This();
    nextPointer: *const fn (*Self) ?HeapPtr,
    scanObject: HeapConstPtr,
    current: [*] const Object,
    beyond: [*] const Object,
    pub fn weakDefault(_:HeapConstPtr) HeapPtrIterator {
        @panic("weakDefault called");
    }
    pub fn noPointers(_:HeapConstPtr) HeapPtrIterator {
        return .{
            .nextPointer = allDone,
            .scanObject = undefined,
            .current = undefined,
            .beyond = undefined,
        };
    }
    pub fn ivPointers(obj:HeapConstPtr) HeapPtrIterator { // only ivs, or both with only iv pointers
        if (!obj.hasInstVarsWithPtrs()) return noPointers(obj);
        const ivs = @ptrCast([*] const Object,obj);
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = ivs+1,
            .beyond = ivs+1+obj.length,
        };
    }
    pub fn arrayPointers(obj:HeapConstPtr) HeapPtrIterator { // only array
        if (!obj.isIndexableWithPtrs()) return noPointers(obj);
        const ivs = @ptrCast([*] const Object,obj);
        return .{
            .nextPointer = lastPointerGroup,
            .scanObject = undefined,
            .current = ivs+1,
            .beyond = ivs+1+obj.length,
        };
    }
    pub fn bothPointers(obj:HeapConstPtr) HeapPtrIterator { // both with iv and array pointers
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
    pub fn bothOnlyArrayPointers(obj:HeapConstPtr) HeapPtrIterator { // both with no iv pointers
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
    fn lastPointerGroup(self:*Self) ?HeapPtr {
        while (@ptrToInt(self.current)<@ptrToInt(self.beyond)) {
            const obj = self.current[0];
            self.current += 1;
            if (obj.isHeapAllocated())
                return obj.toUnchecked(HeapPtr);
        }
        self.nextPointer = allDone;
        return null;
    }
    fn firstPointerGroup(self:*Self) ?HeapPtr {
        while (@ptrToInt(self.current)<@ptrToInt(self.beyond)) {
            const obj = self.current[0];
            self.current += 1;
            if (obj.isHeapAllocated())
                return obj.toUnchecked(HeapPtr);
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
    fn allDone(_:*Self) ?HeapPtr {
        return null;
    }
    //inline
        fn next(self:*Self) ?HeapPtr {
        return self.nextPointer(self);
    }
};
// test "heapPtrIterator" {
//     const testing = std.testing;
//     var h1 = header(0x17, Format.objectNP, 0x27, 0x129,Age.nursery);
//     var h2 = header(0x0, Format.objectP, 0x27, 0x129,Age.nursery);
//     var o1 = [_]Object{Nil,Nil,h1.asObject(),True,h1.asObject(),h2.asObject(),True};
//     const ho1 = @ptrCast(HeapPtr,&o1);
//     var i = h1.makeIterator();
//     try testing.expectEqual(i.next(),null);
//     i = h2.makeIterator();
//     try testing.expectEqual(i.next(),null);
//     o1[0] = header(@sizeOf(@TypeOf(o1))/8-1,Format.objectP, 0x27, 0x129,Age.nursery).o();
//     i = HeapPtrIterator.noPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapPtrIterator.ivPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h2);
//     try testing.expectEqual(i.next(),null);
//     o1[0] = header(@sizeOf(@TypeOf(o1))/8-1,Format.arrayP, 0x27, 0x129,Age.nursery).o();
//     i = HeapPtrIterator.noPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapPtrIterator.ivPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapPtrIterator.arrayPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h2);
//     try testing.expectEqual(i.next(),null);
//     o1[3] = @bitCast(Object,@as(u64,2));
//     o1[4] = @bitCast(Object,@ptrToInt(&o1[5]));
//     o1[0] = header(2,Format.bothOP, 0x27, 0x129,Age.nursery).o();
//     i = HeapPtrIterator.ivPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapPtrIterator.bothPointers(ho1);
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next(),null);
//     i = HeapPtrIterator.arrayPointers(ho1);
//     try testing.expectEqual(i.next(),null);
//     o1[0] = header(2,Format.bothPP, 0x27, 0x129,Age.nursery).o();
//     i = ho1.makeIterator();
//     try testing.expectEqual(i.next().?,&h1);
//     try testing.expectEqual(i.next().?,&h2);
//     try testing.expectEqual(i.next(),null);
// }

pub const AllocErrors = error {Fail,HeapFull,NotIndexable};

pub const HeaderArray = [*]align(@alignOf(u64)) Header;
pub const HeaderSlice = []align(@alignOf(u64)) Header;
pub const HeapPtr = *align(@alignOf(u64)) Header;
pub const HeapConstPtr = *align(@alignOf(u64)) const Header;
pub const Header = packed struct(u64) {
        classIndex: u16,
        hash: u24,
        objectFormat: Format,
        age: Age,
        length: u12,

    const immediateLength: u16 = 4095; // all immediate objects (except doubles) have this as top 12 bits
    const forwardLength: u16 = 4094;
    pub const maxLength = @min(4093,std.math.maxInt(u12)-3); // reserve space for size, address, weakLink
    pub const includesHeader = true;
    pub fn iterator(self: HeapConstPtr) Format.Iterator {
        return self.objectFormat.iterator();
    }
    pub fn makeIterator(self: HeapConstPtr) HeapPtrIterator {
        return self.objectFormat.iterator()(self);
    }
    pub inline fn partialOnStack(selfOffset: u16) Header {
        return @bitCast(Header,@as(u64,selfOffset)<<16);
    }
    inline fn init(length : u12, format : Format, classIndex : u16, hash: u24, age: Age) Header {
        return Header {
            .classIndex = classIndex,
            .hash = hash,
            .objectFormat = format,
            .age = age,
            .length = length,
        };
    }
    pub inline fn isOnStack(self: HeapConstPtr) bool {
        _ = self; unreachable;
//        return self.age.isOnStack();
    }
    pub inline fn isUnmoving(self: HeapConstPtr) bool {
        return self.age.isUnmoving();
    }
    pub inline fn isStack(self: HeapConstPtr) bool {
        return self.age.isStack();
    }
    pub inline fn isIncompleteContext(self: HeapConstPtr) bool {
        return self.age.isIncompleteContext();
    }
    pub inline fn forwardedTo(self: HeapConstPtr) HeapConstPtr {
        return @intToPtr(HeapConstPtr,@intCast(u64,@intCast(i64,@bitCast(u64,self.*)<<16)>>16));
    }
    pub inline fn isForwarded(self: HeapConstPtr) bool {
        return self.length==forwardLength;
    }
    pub inline fn forwarded(self: HeapConstPtr) HeapConstPtr {
        if (self.isForwarded()) {
            return self.forwardedTo();
        }
        return self;
    }
    pub inline fn asSlice(self: HeapConstPtr) ![]Object {
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
    pub inline fn arrayAsSlice(self: HeapConstPtr,comptime T:type) ![]T {
        const head = self.*;
        if (head.length==forwardLength) {
            const realSelf = self.forwardedTo();
            return realSelf.arrayAsSlice_(realSelf.*,T);
        } else
            return self.arrayAsSlice_(head,T);
    }
    inline fn arrayAsSlice_(self: HeapConstPtr, head: Header, comptime T:type) ![]T {
        if (head.age.isIncompleteContext()) unreachable;
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
    pub inline fn arraySize(maybeForwarded: HeapConstPtr) !usize {
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
    pub fn growSizeX(maybeForwarded: HeapConstPtr, stepSize: usize) !usize {
        const self = maybeForwarded.forwarded();
        const form = self.objectFormat;
        if (!form.isIndexable()) return error.NotIndexable;
        var size: usize = self.length;
        if (form.hasInstVars()) {
            const oa = @intToPtr([*]u64,@ptrToInt(self));
            size = form.wordSize(oa[size+1]);
        }
        size = largerPowerOf2(size * 2);
        if (size>Header.maxLength and size<Header.maxLength*2) size = Header.maxLength;
        return (form.getSize()*size+stepSize-1)/stepSize*stepSize;
    }
    pub inline fn inHeapSize(maybeForwarded: HeapConstPtr) usize {
        const self = maybeForwarded.forwarded();
        const form = self.objectFormat;
        const size = self.length;
        if (form.isIndexable() and form.hasInstVars()) {
            var oa = @intToPtr([*]u64,@ptrToInt(self))+size;
            return size+3+if (oa[2]!=@ptrToInt(oa+3)) 0 else form.wordSize(oa[1]);
        }
        return size+1;
    }
    pub inline fn isIndirect(maybeForwarded: HeapConstPtr) bool {
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
    pub inline fn isRaw(self: HeapConstPtr) bool {
        return self.objectFormat.isRaw();
    }
    pub inline fn asObject(self: HeapConstPtr) Object {
        return Object.from(self);
    }
    pub inline fn asObjectConstPtr(self: HeapConstPtr) [*]const Object {
        return @ptrCast([*]const Object,self);
    }
    pub inline fn asObjectPtr(self: HeapPtr) [*]Object {
        return @ptrCast([*]Object,self);
    }
    pub inline fn fromObjectPtr(op:  [*]const Object) HeaderArray {
        return @intToPtr(HeaderArray,@ptrToInt(op));
    }
    pub inline fn o(self: Header) Object {
        return @bitCast(Object,self);
    }
    pub inline fn asObjectArray(self: HeapConstPtr) [*]align(@alignOf(u64)) Object {
        return @intToPtr([*]align(@alignOf(u64)) Object, @ptrToInt(self)) + 1;
    }
    pub inline fn setHash(self: HeapPtr,hash: u24) Header {
        self.hash=hash;
        return self.*;
    }
    pub inline fn setNArgs(self: HeapPtr,args: u8) Header {
        self.hash=(self.hash & 0xffff)+(args<<16);
        return self.*;
    }
    pub inline fn getClass(self: HeapConstPtr) ClassIndex {
        return self.classIndex;
    }
    pub inline fn hash16(self: Header) u16 {
        return @truncate(u16,@bitCast(u64,self)>>16);
    }
    pub inline fn hasInstVars(self: HeapConstPtr) bool {
        return self.objectFormat.hasInstVars();
    }
    pub inline fn hasInstVarsWithPtrs(self: HeapConstPtr) bool {
        return self.objectFormat.hasInstVarsWithPtrs();
    }
    pub inline fn isIndexable(self: HeapConstPtr) bool {
        return self.objectFormat.isIndexable();
    }
    pub inline fn isIndexableWithPtrs(self: HeapConstPtr) bool {
        return self.objectFormat.isIndexableWithPtrs();
    }
    pub inline fn instVars(self: HeapConstPtr) []Object {
        if (self.objectFormat.hasInstVars()) {
            const size = self.length;
            return self.asObjectArray()[0..size];
        } else return &[0]Object{};
    }
    fn @"format FUBAR"(
        self: HeapConstPtr,
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
    if (size>Header.maxLength and size<Header.maxLength*2) size = Header.maxLength;
    return (size*@sizeOf(Object)+@sizeOf(Target)-1)/@sizeOf(Target);
}
test "growSize" {
    //try std.testing.expectEqual(growSize(@as([]const u8,"foo"[0..]),u8),8);
}
pub const header = Header.init;
// test "Header structure" {
//     const testing = std.testing;
//     try testing.expectEqual(@sizeOf(Header),8);
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
        header: Header,
        const Self = @This();
        pub fn init() Self {
            var result = Self {
                .header = header(words,@intToEnum(Format,size),object.String_I,hash,Age.static),
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
        fn obj(self: * const Self) HeapConstPtr {
            return @ptrCast(*const Header,&self.header);
        }
        pub fn asObject(self: * const Self) Object {
            return Object.from(self.obj());
        }
    };
}
pub fn compileStrings(comptime tup: anytype) [tup.len] HeapConstPtr {
    @setEvalBranchQuota(3000);
    comptime var result : [tup.len] HeapConstPtr = undefined;
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
