const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const thread = @import("thread.zig");
const object = @import("object.zig");
const Object = object.Object;
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
    const IndexableFormat : u8 = 31;
    const Immutable : u8 = 128;
    fn calcSizes() [IndexableFormat+1]u8 {
        var sizes : [IndexableFormat+1]u8 = undefined;
        for (sizes) |*size,i| {
            const typ = i & (IndexableFormat>>1);
            if (typ<Indexable_32) size.* = 1
                else if (typ<Indexable_16) size.* = 2
                else if (typ<Indexable_8) size.* = 4
                else size.* = 8;
        }
        return sizes;
    }
    const fieldSizes = [_]u8{calcSizes()};
    fn calcPartials() [IndexableFormat+1]u8 {
        var partials : [IndexableFormat+1]u8 = undefined;
        for (partials) |*partial,i| {
            const typ = i & (IndexableFormat/2);
            if (typ<Indexable_32) partial.* = 0
            else if (typ<Indexable_16) partial.* = Indexable_32-typ
            else if (typ<Indexable_8) partial.* = Indexable_16-typ
            else partial.* = Indexable_8-typ;
        }
        return partials;
    }
    const fieldPartials = [_]u8{calcPartials()};
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
    pub inline fn raw(self: Self, comptime T : type, size : usize) Self {
        switch (T) {
            u8,i8 => return self.plusIndexable(Indexable_8 + @intCast(u8,(-@intCast(isize,size))&7)),
            u16,i16 => return self.plusIndexable(Indexable_16 + @intCast(u8,(-@intCast(isize,size))&3)),
            u32,i32,f32 => return self.plusIndexable(Indexable_32 + @intCast(u8,(-@intCast(isize,size))&1)),
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
test "header formats" {
    const expect = std.testing.expect;
    try expect(Format.objectP.hasInstVars());
    try expect(!Format.objectP.isPointerFree());
    try expect(Format.objectNP.isPointerFree());
    try expect(!Format.objectP.isIndexable());
    try expect(!Format.objectP.isWeak());
    try expect(Format.weak.isWeak());
}
pub const HeaderArray = [*]align(@alignOf(u64)) Header;
pub const HeapPtr = *align(@alignOf(u64)) Header;
pub const HeapConstPtr = *align(@alignOf(u64)) const Header;
pub const Age = enum(u4) {
    stack = Stack,
    nursery = Nursery,
    teen = FirstTeen,
    global = Global,
    aoo = AoO,
    static = Static,
    free = Free,
    _,
    const Stack: u4 = 0;
    const Nursery: u4 = 1;
    const FirstTeen: u4 = 2;
    const LastTeen: u4 = 7;
    const Global: u4 = 8;
    const GlobalMarked: u4 = 9;
    const GlobalScanned: u4 = 11;
    const Static: u4 = 10;
    const AoO : u4 = 12;
    const AoOMarked : u4 = 13;
    const Free : u4 = 14;
    const AoOScanned : u4 = 15;
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
        return @enumToInt(self) == Stack;
    }
    pub inline fn marked(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | 1);
    }
    pub inline fn scanned(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | 2);
    }
    // Note: assigning a ptr to a scanned object must block for collection
};
pub const Header = packed struct(u64) {
        classIndex: u16,
        hash: u24,
        objectFormat: Format,
        age: Age,
        length: u12,

    const immediateLength: u16 = 4095;
    const forwardLength: u16 = 4094;
    const indirectLength: u16 = 4093;
    pub const maxLength = @min(4092,@import("arenas.zig").heapAllocationSize-1);
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
    pub inline fn forwardedTo(self: HeapConstPtr) HeapConstPtr {
        return @intToPtr(HeapConstPtr,@intCast(u64,@intCast(i64,@bitCast(u64,self.*)<<16)>>16));
    }
    pub inline fn isForwarded(self: HeapConstPtr) bool {
        return self.length==forwardLength;
    }
    pub inline fn isForwardedOrIndirect(self: HeapConstPtr) bool {
        return self.length>=forwardLength;
    }
    pub inline fn forwarded(self: HeapConstPtr) HeapConstPtr {
        var ptr = self;
        while (ptr.isForwarded()) {
            ptr = ptr.forwardedTo();
        }
        return ptr;
    }
    pub inline fn isIndirect(self: HeapConstPtr) bool {
        return self.length>=indirectLength and self.length<forwardLength;
    }
    pub fn arrayAsSlice(self: HeapConstPtr, comptime T: type) ![]T {
        const ptr = self.forwarded();
        const form = ptr.objectFormat;
        if (!form.isIndexable()) return error.NotIndexable;
        var size: usize = ptr.length;
        var oa = ptr.asObjectArray();
        if (T==Object)
            return oa[0..size];
        if (form.isRaw()) {
            const formi = @enumToInt(form);
            if (ptr.isIndirect()) {
                size = @bitCast(usize,oa[0]);
                oa += 1;
                @panic("incomplete");
            }
            return mem.bytesAsSlice(
                T,
                if (formi>=Format.Indexable_8) @ptrCast([*]u8,oa)[0..size*8-(formi&7)]
                    else if (formi>=Format.Indexable_16) @ptrCast([*]u8,oa)[0..size*8-(formi&3)*2]
                    else if (formi>=Format.Indexable_32) @ptrCast([*]u8,oa)[0..size*8-(formi&1)*4]
                    else @ptrCast([*]u8,oa)[0..size*8]);
        }
        @panic("arrayAsSlice for non-raw");
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
    pub inline fn indexables(self: HeapConstPtr,comptime T:type) []T {
        const form = self.objectFormat;
        if (!form.isIndexable()) return &[0]T{};
        var size: usize = self.length;
        var oa = @intToPtr([*]u64,@ptrToInt(self))+1;
        if (form.hasInstVars()) {
            oa += size;
            size = oa[0];
            oa += 1;
        } else if (size >= indirectLength) {
            size = oa[0];
            oa += 1;
        }
        if (size >= indirectLength) oa = @intToPtr([*]u64,oa[0]);
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
            else => unreachable,
        }
    }
    pub inline fn inHeapSize(self: HeapConstPtr) usize {
        const form = self.objectFormat;
        var size : usize = self.length;
        if (!form.isIndexable()) return size+1;
        var oa = @intToPtr([*]u64,@ptrToInt(self))+1;
        if (form.hasInstVars()) {
            oa += size;
            size = oa[0];
            oa += 1;
        } else if (size >= indirectLength) {
            size = oa[0];
            oa += 1;
        }
        oa = oa + (if (size >= indirectLength) 1 else size);
        return (@ptrToInt(oa)-@ptrToInt(self))/@sizeOf(Object);
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
    const hdr = header(0x17, Format.objectNP, 0x23, 0x123,Age.teen);
    try testing.expectEqual(hdr.o().u(),0x0172200001230023);
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
                .header = header((size+7)/8,Format.arrayNP.raw(u8,size),class.String_I,hash,Age.static),
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

const abcde = CompileTimeString("abcde").init();
test "compile time" {
//    std.debug.print("abcde: {any}\n",.{abcde.h()});
//    std.debug.print("abcde: {any}\n",.{abcde.asObject()});
}
