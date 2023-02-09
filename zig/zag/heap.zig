const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const pow2 = @import("utilities.zig").largerPowerOf2;

pub const Format = enum(u8) {
    none = 0,
    objectP = InstVarsWithPtrs,
    arrayP = IndexableWithPtrs,
    bothPP = InstVarsWithPtrs + IndexableWithPtrs,
    weak = Weak,
    objectNP = InstVars,
    arrayNP = Indexable_64,
    bothNP = InstVars + Indexable_64,
    bothOP = InstVarsWithPtrs + Indexable_64,
    bothAP = InstVars + IndexableWithPtrs,
    _,
    const Self = @This();
    const InstVars : u8 = 32;
    const InstVarsPtrs : u8 = 64;
    const IndexablePtrs : u8 = 16;
    const InstVarsWithPtrs = InstVars+InstVarsPtrs;
    const IndexableWithPtrs = Indexable_64+IndexablePtrs;
    const Weak : u8 = 64;
    const Indexable_64 : u8 = 1;
    const Indexable_32 : u8 = 2;
    const Indexable_16 : u8 = 4;
    const Indexable_8 : u8 = 8;
    const IndexableSizes : u8 = 15;
    const IndexableFormat : u8 = 31;
    const Immutable : u8 = 128;
    fn calcSizes() [IndexableSizes+1]u8 {
        var sizes : [IndexableSizes+1]u8 = undefined;
        for (sizes) |*s,i| {
            const typ = i & IndexableSizes;
            s.* =
                if (typ<Indexable_32) 1
                else if (typ<Indexable_16) 2
                else if (typ<Indexable_8) 4
                else 8;
        }
        return sizes;
    }
    const fieldSizes = calcSizes();
    fn calcPartials() [IndexableSizes+1]u8 {
        var partials: [IndexableSizes+1]u8 = undefined;
        for (partials) |*partial,i| {
            const typ: u8 = @truncate(u8,i) & IndexableSizes;
            partial.* =
                if (typ>Indexable_32 and typ<Indexable_16) typ&1
                else if (typ>Indexable_16 and typ<Indexable_8) typ&3
                else if (typ>Indexable_8) typ&7
                else 0;
        }
        return partials;
    }
    const fieldPartials = calcPartials();
    pub inline fn size(self: Self, n: usize) usize {
        const f = @enumToInt(self) & IndexableFormat;
        return n*fieldSizes[f]-fieldPartials[f];
    }
    pub inline fn wordSize(self: Self, n: usize) usize {
        const fs = fieldSizes[@enumToInt(self) & IndexableFormat];
        return (n+fs-1)/fs;
    }
    pub inline fn getSize(self: Self) usize {
        return fieldSizes[@enumToInt(self) & IndexableFormat];
    }
    pub inline fn setWeak(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) & ~InstVars | Weak);
    }
    pub inline fn setObject(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | InstVars);
    }
    pub inline fn withoutIndexable(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) & ~IndexableFormat);
    }
    pub inline fn plusIndexable(self: Self, n: u8) Self {
        return @intToEnum(Self,@enumToInt(self.withoutIndexable())+n);
    }
    pub inline fn justIndexable(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) & IndexableFormat);
    }
    pub inline fn setArray(self: Self) Self {
        return self.plusIndexable(IndexableWithPtrs);
    }
    pub inline fn raw(self: Self, comptime T : type, s : usize) Self {
        switch (T) {
            u8,i8 => return self.plusIndexable(Indexable_8 + @intCast(u8,(-@intCast(isize,s))&7)),
            u16,i16 => return self.plusIndexable(Indexable_16 + @intCast(u8,(-@intCast(isize,s))&3)),
            u32,i32,f32 => return self.plusIndexable(Indexable_32 + @intCast(u8,(-@intCast(isize,s))&1)),
            u64,i64,f64,Object => return self.plusIndexable(Indexable_64),
            else => return self,
        }
    }
    pub inline fn immutable(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | Immutable);
    }
    pub inline fn hasInstVars(self: Self) bool {
        return @enumToInt(self) & InstVars != 0;
    }
    pub inline fn isIndexable(self: Self) bool {
        return @enumToInt(self) & IndexableFormat != 0;
    }
    pub inline fn hasBoth(self: Self) bool {
        return @enumToInt(self) & InstVars+IndexableFormat == InstVars+Indexable_64;
    }
    pub inline fn isWeak(self: Self) bool {
        return @enumToInt(self) & Weak+InstVars == Weak;
    }
    pub inline fn isPointerFree(self: Self) bool {
        return @enumToInt(self) & InstVarsPtrs+IndexablePtrs == 0;
    }
    pub inline fn hasPointers(self: Self) bool {
        return ~self.isPointerFree();
    }
    pub inline fn isRaw(self: Self) bool {
        return @enumToInt(self.justIndexable()) >= Indexable_64 and  @enumToInt(self.justIndexable()) < IndexablePtrs;
    }
    pub inline fn is64(self: Self) bool {
        return @enumToInt(self.justIndexable()) == Indexable_64;
    }
    pub inline fn is32(self: Self) bool {
        return @enumToInt(self.justIndexable()) >= Indexable_32 and @enumToInt(self.justIndexable()) <= Indexable_32 + 1;
    }
    pub inline fn is16(self: Self) bool {
        return @enumToInt(self.justIndexable()) >= Indexable_16 and @enumToInt(self.justIndexable()) <= Indexable_16 + 3;
    }
    pub inline fn is8(self: Self) bool {
        return @enumToInt(self.justIndexable()) >= Indexable_8 and @enumToInt(self.justIndexable()) <= Indexable_8 + 7;
    }
    pub inline fn isImmutable(self: Self) bool {
        return @enumToInt(self) & Immutable != 0;
    }
    pub inline fn noBase(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) & Immutable);
    }
    fn eq(f: Self, v: u8) !void {
        return std.testing.expectEqual(@intToEnum(Self,v),f);
    }
};
test "raw formats" {
    try Format.none.raw(u8,0).eq(Format.Indexable_8+0);
    try Format.none.raw(u8,1).eq(Format.Indexable_8+7);
    try Format.none.raw(u8,2).eq(Format.Indexable_8+6);
    try Format.none.raw(u8,3).eq(Format.Indexable_8+5);
    try Format.none.raw(u8,4).eq(Format.Indexable_8+4);
    try Format.none.raw(u8,5).eq(Format.Indexable_8+3);
    try Format.none.raw(u8,6).eq(Format.Indexable_8+2);
    try Format.none.raw(u8,7).eq(Format.Indexable_8+1);
    try Format.none.raw(u8,8).eq(Format.Indexable_8+0);
    try std.testing.expect(Format.none.raw(u8,0).is8());
    try Format.none.raw(u16,0).eq(Format.Indexable_16+0);
    try Format.none.raw(u16,1).eq(Format.Indexable_16+3);
    try Format.none.raw(u16,2).eq(Format.Indexable_16+2);
    try Format.none.raw(u16,3).eq(Format.Indexable_16+1);
    try Format.none.raw(u16,4).eq(Format.Indexable_16+0);
    try std.testing.expect(Format.none.raw(u16,0).is16());
    try Format.none.raw(u32,0).eq(Format.Indexable_32+0);
    try Format.none.raw(u32,1).eq(Format.Indexable_32+1);
    try Format.none.raw(u32,2).eq(Format.Indexable_32+0);
    try std.testing.expect(Format.none.raw(u32,0).is32());
    try Format.none.raw(u64,0).eq(Format.Indexable_64+0);
    try Format.none.raw(u64,1).eq(Format.Indexable_64+0);
    try std.testing.expect(Format.none.raw(u64,0).is64());
}
test "raw size" {
    const ee = std.testing.expectEqual;
    try ee(Format.none.raw(u8,24).size(3),24);
    try ee(Format.none.raw(u8,23).size(3),23);
    try ee(Format.none.raw(u8,22).size(3),22);
    try ee(Format.none.raw(u8,21).size(3),21);
    try ee(Format.none.raw(u8,20).size(3),20);
    try ee(Format.none.raw(u8,19).size(3),19);
    try ee(Format.none.raw(u8,18).size(3),18);
    try ee(Format.none.raw(u8,17).size(3),17);
    try ee(Format.none.raw(u8,24).size(3),24);
    try ee(Format.none.raw(u16,12).size(3),12);
    try ee(Format.none.raw(u16,11).size(3),11);
    try ee(Format.none.raw(u16,10).size(3),10);
    try ee(Format.none.raw(u16,9).size(3),9);
    try ee(Format.none.raw(u16,12).size(3),12);
    try ee(Format.none.raw(u32,6).size(3),6);
    try ee(Format.none.raw(u32,5).size(3),5);
    try ee(Format.none.raw(u32,6).size(3),6);
    try ee(Format.none.raw(u64,3).size(3),3);
    try ee(Format.none.raw(u64,3).size(3),3);
}

test "header formats" {
    const expect = std.testing.expect;
    try expect(Format.objectP.hasInstVars());
    try expect(!Format.objectP.isPointerFree());
    try expect(Format.objectNP.isPointerFree());
    try expect(!Format.objectP.isIndexable());
    try expect(!Format.objectP.isWeak());
    try expect(Format.weak.isWeak());
}
pub const Age = enum(u4) {
    incompleteContext = IncompleteContext,
    nursery = Nursery,
    teen = FirstTeen,
    global = Global,
    aoo = AoO,
    static = Static,
    free = Free,
    _,
    const IncompleteContext: u1 = 0;
    const Stack: u4 = 1;
    const Nursery: u4 = 2;
    const FirstTeen: u4 = 3;
    const LastTeen: u4 = 7;
    const Global: u4 = 8;
    const GlobalMarked: u4 = 9;
    const Static: u4 = 10;
    const GlobalScanned: u4 = 11;
    const AoO : u4 = 12;
    const AoOMarked : u4 = 13;
    const Free : u4 = 14;
    const AoOScanned : u4 = 15;
    const ScanMask: u4 = GlobalScanned; // anded with this give 0 or Static for non-global; Global, GlobalMarked or GlobalScanned for global (AoO or not)
    const Self = @This();
    pub inline fn isAoO(self: Self) bool {
        return @enumToInt(self)>=AoO;
    }
    pub inline fn isGlobalOrStatic(self: Self) bool {
        return @enumToInt(self)>=Global;
    }
    pub inline fn isGlobal(self: Self) bool {
        return @enumToInt(self)>=Global and @enumToInt(self)!=Static;
    }
    pub inline fn isInStack(self: Self) bool {
        return @enumToInt(self) <= Stack;
    }
    pub inline fn isIncompleteContext(self: Self) bool {
        return @enumToInt(self) <= IncompleteContext;
    }
    pub inline fn marked(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | 1);
    }
    pub inline fn scanned(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | 2);
    }
    // Note: assigning a ptr to a scanned object must block for collection
};
pub const HeapPtrIterator = struct {
    const Self = @This();
    nextPointer: *const fn (*Self) ?HeapPtr,
    scanObject: HeapPtr,
    current: [*]Object,
    beyond: [*]Object,
    pub fn noPointers(obj:HeapPtr) HeapPtrIterator {
        return .{
            .nextPointer = allDone,
            .scanObject = obj,
            .current = undefined,
            .beyond = undefined,
        };
    }
    pub fn objectPointers(obj:HeapPtr) HeapPtrIterator {
        const ivs = @ptrCast([*]Object,obj);
        return .{
            .nextPointer = justObjects,
            .scanObject = obj,
            .current = ivs+1,
            .beyond = ivs+1+obj.length,
        };
    }
    fn justObjects(self:*Self) ?HeapPtr {
        while (@ptrToInt(self.current)<@ptrToInt(self.beyond)) {
            const obj = self.current[0];
            self.current += 1;
            if (obj.isHeapAllocated())
                return obj.toUnchecked(HeapPtr);
        }
        self.nextPointer = allDone;
        return null;
    }
    inline fn next(self:*Self) ?HeapPtr {
        return self.nextPointer(self);
    }
    fn allDone(_:*Self) ?HeapPtr {
        return null;
    }
};
test "heapPtrIterator" {
    const testing = std.testing;
    var h1 = header(0x17, Format.objectNP, 0x27, 0x129,Age.teen);
    var h2 = header(0x0, Format.objectP, 0x27, 0x129,Age.teen);
    var o1 = [_]Object{Nil,Nil,h1.asObject(),True,h2.asObject(),h1.asObject()};
    var i = HeapPtrIterator.noPointers(&h1);
    try testing.expectEqual(i.next(),null);
    i = HeapPtrIterator.objectPointers(&h2);
    try testing.expectEqual(i.next(),null);
    o1[0] = header(@sizeOf(@TypeOf(o1))/8-1,Format.objectP, 0x27, 0x129,Age.teen).o();
    i = HeapPtrIterator.noPointers(@ptrCast(HeapPtr,&o1));
    try testing.expectEqual(i.next(),null);
    i = HeapPtrIterator.objectPointers(@ptrCast(HeapPtr,&o1));
    try testing.expectEqual(i.next().?,&h1);
    try testing.expectEqual(i.next().?,&h2);
    try testing.expectEqual(i.next().?,&h1);
    try testing.expectEqual(i.next(),null);
}
pub const HeaderArray = [*]align(@alignOf(u64)) Header;
pub const HeapPtr = *align(@alignOf(u64)) Header;
pub const HeapConstPtr = *align(@alignOf(u64)) const Header;
pub const Header = packed struct(u64) {
        classIndex: u16,
        hash: u24,
        objectFormat: Format,
        age: Age,
        length: u12,

    const immediateLength: u16 = 4095;
    const forwardLength: u16 = 4094;
    pub const maxLength = @min(4093,@import("arenas.zig").heapAllocationSize-1);
    pub const includesHeader = true;
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
    pub inline fn isInStack(self: HeapConstPtr) bool {
        return self.age.isInStack();
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
        var ptr = self;
        while (ptr.isForwarded()) {
            ptr = ptr.forwardedTo();
        }
        return ptr;
    }
    pub inline fn arrayAsSlice(maybeForwarded: HeapConstPtr,comptime T:type) ![]T {
        const self = maybeForwarded.forwarded();
        var size: usize = self.length;
        const form = self.objectFormat;
        if (!form.isIndexable()) return error.NotIndexable;
        var oa = @intToPtr([*]u64,@ptrToInt(self));
        if (form.hasInstVars()) {
            oa += size;
            size = oa[1];
            return @intToPtr([*]T,oa[2])[0..size];
        }
        oa += 1;
        switch (T) {
            Object,i64,u64,f64 => {
                return @ptrCast([*]T,oa)[0..size];
            },
            i32,u32,f32 => {
                return @ptrCast([*]T,oa)[0..size*2-(@enumToInt(form)&1)];
            },
            i16,u16 => {
                return @ptrCast([*]T,oa)[0..size*4-(@enumToInt(form)&3)];
            },
            i8,u8 => {
                return @ptrCast([*]T,oa)[0..size*8-(@enumToInt(form)&7)];
            },
            else => @panic("can't get arrayAsSlice for " ++ @typeName(T)),
        }
    }
    pub inline fn arraySize(maybeForwarded: HeapConstPtr) !usize {
        const self = maybeForwarded.forwarded();
        const form = self.objectFormat;
        if (!form.isIndexable()) return error.NotIndexable;
        const size = self.length;
        if (form.hasInstVars()) {
            const oa = @intToPtr([*]u64,@ptrToInt(self));
            return oa[size+1];
        }
        return form.size(size);
    }
    pub fn growSize(maybeForwarded: HeapConstPtr, stepSize: usize) !usize {
        const self = maybeForwarded.forwarded();
        const form = self.objectFormat;
        if (!form.isIndexable()) return error.NotIndexable;
        var size: usize = self.length;
        if (form.hasInstVars()) {
            const oa = @intToPtr([*]u64,@ptrToInt(self));
            size = form.wordSize(oa[size+1]);
        }
        size = @import("utilities.zig").largerPowerOf2(size * 2);
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
    pub inline fn isIndexable(self: HeapConstPtr) bool {
        return self.objectFormat.isIndexable();
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
            if (self.objectFormat.is8() and self.classIndex==class.String) {
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
pub const header = Header.init;
test "Header structure" {
    const testing = std.testing;
    try testing.expectEqual(@sizeOf(Header),8);
    const hdr = header(0x17, Format.objectNP, 0x27, 0x129,Age.teen);
    try testing.expectEqual(hdr.o().u(),0x0173200001290027);
}
fn hash24(str: [] const u8) u24 {
    const phi: u32 = @import("utilities.zig").inversePhi(u24);
    var hash = phi*%@truncate(u32,str.len+%1);
    for (str) |c,idx| {
        if (idx>9) break;
        hash +%= phi*%c;
    }
    return @truncate(u24,hash);
}
pub fn CompileTimeString(comptime str: [] const u8) type {
    const size = str.len;
    const hash = hash24(str);
    return struct {
        header: Header,
        chars: [size]u8,
        const Self = @This();
        pub fn init() Self {
            var result = Self {
                .header = header((size+7)/8,Format.none.raw(u8,size),class.String_I,hash,Age.static),
                .chars = [_]u8{0}**size,
            };
            for (str) |c,idx| {
                result.chars[idx]=c;
            }
            return result;
        }
        fn h(self: * const Self) []const u8 {
            return @ptrCast([*]const u8,self)[0..(size+15)/8*8];
        }
        fn obj(self: * const Self) HeapConstPtr {
            return @ptrCast(*const Header,self);
        }
        pub fn asObject(self: * const Self) Object {
            return Object.from(@ptrCast(*const Header,self));
        }
    };
}
pub fn compileStrings(comptime tup: anytype) [tup.len] HeapConstPtr {
    @setEvalBranchQuota(3000);
    comptime var result : [tup.len] HeapConstPtr = undefined;
    inline for (tup) |name,idx| {
        result[idx] = comptime @ptrCast(HeapConstPtr,@alignCast(8,&CompileTimeString(name).init()));
    }
    return result;
}

// const abcde = CompileTimeString("abcdefghijk").init();
// test "compile time" {
//     std.testing.expectEqual("abcdefghijk"[0..],abcde.asObject().arrayAsSlice(i8));
//     std.debug.print("abcde: {any}\n",.{abcde.obj().*});
//     std.debug.print("abcde: {any}\n",.{abcde.asObject().arrayAsSlice(i8)});
//     std.debug.print("abcde: {any}\n",.{abcde.h()});
//     std.debug.print("abcde: {any}\n",.{abcde.asObject()});
// }
