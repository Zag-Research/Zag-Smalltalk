const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const class = @import("class.zig");
const native_endian = @import("builtin").target.cpu.arch.endian();
pub const Format = enum(u8) {
    none = 0,
    object = InstVars,
    array = Indexable,
    both = InstVars + Indexable,
    weak = InstVars + Indexable + Weak,
    objectNP = InstVars + PointerFree,
    arrayNP = Indexable + PointerFree,
    bothNP = InstVars + Indexable + PointerFree,
    raw64 = Indexable_64,
    _,
    const Self = @This();
    const InstVars : u8 = 1;
    const Indexable : u8 = 2;
    const Weak : u8 = 4;
    const PointerFree : u8 = 8;
    const RawData : u8 = 16;
    const Indexable_64 : u8 = 17;
    const Indexable_32 : u8 = 18;
    const Indexable_16 : u8 = 20;
    const Indexable_8 : u8 = 24;
    const BaseFormat : u8 = 31;
    const Immutable : u8 = 32;
    const Global : u8 = 64;
    fn calcSizes() [32]u8 {
        var sizes : [32]u8 = undefined;
        var i:u8 = 0;
        while (i<=BaseFormat) : (i += 1) {
           if (i<Indexable_32) sizes[i]=1
            else if (i<Indexable_16) sizes[i]=2
            else if (i<Indexable_8) sizes[i]=4
            else sizes[i]=8;
        }
        return sizes;
    }
    const fieldSizes : [32]u8 = calcSizes();
    fn calcPartials() [32]u8 {
        var partials : [32]u8 = undefined;
        var i:u8 = 0;
        while (i<=BaseFormat) : (i += 1) {
           if (i<Indexable_32) partials[i]=0
            else if (i<Indexable_16) partials[i]=Indexable_32-i
            else if (i<Indexable_8) partials[i]=Indexable_16-i
            else partials[i]=Indexable_8-i;
        }
        return partials;
    }
    const fieldPartials : [32]u8 = calcPartials();
    pub inline fn weak(self: Self) Self {
        return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat) + InstVars + Indexable + Weak);
    }
    pub inline fn noBase(self: Self) Self {
        return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat));
    }
    pub inline fn object(self: Self) Self {
        return @intToEnum(Self,@bitCast(u8,self) | InstVars);
    }
    pub inline fn array(self: Self) Self {
        return @intToEnum(Self,@bitCast(u8,self) | Indexable);
    }
    pub inline fn raw(self: Self, comptime T : type, size : usize) Self {
        switch (T) {
            u8,i8 => {return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat) + Indexable_8 + ((-@intCast(isize,size))&7));},
            u16,i16 => {return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat) + Indexable_16 + ((-@intCast(isize,size))&3));},
            u32,i32,f32 => {return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat) + Indexable_32 + ((-@intCast(isize,size))&1));},
            u64,i64,f64 => {return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat) + Indexable_64);},
            else => {return self;},
        }
    }
    pub inline fn hasInstVars(self: Self) bool {
        return @bitCast(u8, self) & InstVars+RawData == InstVars;
    }
    pub inline fn isIndexable(self: Self) bool {
        return @bitCast(u8, self) & Indexable+RawData != 0;
    }
    pub inline fn hasBoth(self: Self) bool {
        return @bitCast(u8, self) & InstVars+Indexable+RawData == InstVars+Indexable;
    }
    pub inline fn isWeak(self: Self) bool {
        return @bitCast(u8, self) & Weak+RawData == Weak;
    }
    pub inline fn isPointerFree(self: Self) bool {
        const base = @bitCast(u8, self) & BaseFormat;
        return base >= PointerFree;
    }
    pub inline fn hasPointers(self: Self) bool {
        return ~self.isPointerFree();
    }
    pub inline fn isRaw(self: Self) bool {
        const base = @bitCast(u8, self) & BaseFormat;
        return base >= Indexable_64;
    }
    pub inline fn is64(self: Self) bool {
        const base = @bitCast(u8, self) & BaseFormat;
        return base == Indexable_64;
    }
    pub inline fn is32(self: Self) bool {
        const base = @bitCast(u8, self) & BaseFormat;
        return base >= Indexable_32 and base <= Indexable_32 + 1;
    }
    pub inline fn is16(self: Self) bool {
        const base = @bitCast(u8, self) & BaseFormat;
        return base >= Indexable_16 and base <= Indexable_16 + 3;
    }
    pub inline fn is8(self: Self) bool {
        const base = @bitCast(u8, self) & BaseFormat;
        return base >= Indexable_8 and base <= Indexable_8 + 7;
    }
    pub inline fn isImmutable(self: Self) bool {
        return @bitCast(u8, self) & Immutable != 0;
    }
    pub inline fn isGlobal(self: Self) bool {
        return @bitCast(u8, self) & Global != 0;
    }
};
fn eq(v:u8,f:Format) !void {
    return std.testing.expectEqual(v,@enumToInt(f));
}
test "raw formats" {
    try eq(24,Format.none.raw(u8,0));
    try eq(31,Format.none.raw(u8,1));
    try eq(30,Format.none.raw(u8,2));
    try eq(29,Format.none.raw(u8,3));
    try eq(28,Format.none.raw(u8,4));
    try eq(27,Format.none.raw(u8,5));
    try eq(26,Format.none.raw(u8,6));
    try eq(25,Format.none.raw(u8,7));
    try eq(24,Format.none.raw(u8,8));
}
test "header formats" {
    const expect = std.testing.expect;
    try expect(Format.object.hasInstVars());
    try expect(!Format.object.isPointerFree());
    try expect(Format.objectNP.isPointerFree());
    try expect(!Format.object.isIndexable());
    try expect(Format.raw64.isPointerFree());
    try expect(!Format.raw64.isWeak());
    try expect(!Format.object.isWeak());
    try expect(Format.weak.isWeak());
}
pub const HeapPtr = *Header;
pub const HeapConstPtr = *const Header;
const heapMethods = struct {
    pub inline fn isIndexable(self: HeapConstPtr) bool {
        return self.objectFormat.isIndexable();
    }
    pub inline fn isRaw(self: HeapConstPtr) bool {
        return self.objectFormat.isRaw();
    }
    pub inline fn asObject(self: Header) Object {
        return @bitCast(Object,self);
    }
    pub inline fn asObjectArray(self: HeapConstPtr) [*]Object {
        return @intToPtr([*]Object,@ptrToInt(self))+1;
    }
    pub inline fn setHash(self: HeapPtr,hash: u24) Header {
        self.hash=hash;
        return self.*;
    }
    pub inline fn setNArgs(self: HeapPtr,args: u8) Header {
        self.hash=(self.hash & 0xffff)+(args<<16);
        return self.*;
    }
    pub inline fn instVars(self: HeapConstPtr) []Object {
        if (self.objectFormat.hasInstVars()) {
            const size = self.length;
            return self.asObjectArray()[0..size];
        } else return &[0]Object{};
    }
    pub inline fn indexables(self: HeapConstPtr,comptime T:type) []T {
        const form = self.objectFormat;
        const size = self.length;
        const oa = self.asObjectArray();
        switch (T) {
            Object => {
                if (form.hasInstVars()) {
                    const array_size = @bitCast(usize,oa[size]);
                    return oa[size+1..size+array_size+1];
                } else {
                    return oa[0..size];
                }
            },
            i64,u64,f64 => {
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
            else => {@panic("Not Implemented");},
        }
       
    }
    pub inline fn derefForwarded(self: HeapPtr) HeapPtr {
        return @intToPtr(HeapPtr,-@bitCast(i64,self.*));
    }
    pub inline fn totalSize(self: HeapPtr) usize {
        const form = self.objectFormat;
        var size : usize = self.length;
        if (form.isRaw()) {
            if (size>=32767) unreachable;
        } else if (form.hasBoth()) {
            size += @bitCast(u64,self.asObjectArray()[size])+1;
        }
        return size+1;
    }
    pub fn format(
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
const Header = switch (native_endian) {
    .Big => packed struct {
        length: u16 align(8),
        objectFormat: Format,
        hash: u24,
        classIndex: u16,
        usingnamespace heapMethods;
    },
    .Little => packed struct {
        classIndex: u16 align(8),
        hash: u24,
        objectFormat: Format,
        length: u16,
        usingnamespace heapMethods;
    },
};
pub inline fn header(length : u16, format : Format, classIndex : u16, hash: u24) Header {
    return Header {
        .length = length,
        .objectFormat = format,
        .hash = hash,
        .classIndex = classIndex,
    };
}
test "Header structure" {
    const expect = std.testing.expect;
    try expect(@sizeOf(Header) == 8);
    const hdr = header(17, Format.object, 35,0x123);
    try expect(@bitCast(u64, hdr) == 0x0011010001230023);
}
pub const NurseryArena = Arena {
    .vtable = nurseryVtable,
    .heap = undefined,
    .tos = undefined,
    .allocated = undefined,
    .collectTo = undefined,
    .size = 2048,
};
const nurseryVtable =  Arena.Vtable {
    .getGlobal = getGlobalNext,
};
fn getGlobalNext(self: *Arena) *Arena {
    if (self.collectTo) | ptr | return ptr.getGlobal();
    @panic("nothing to collect to");
}
pub const TeenArena = Arena {
    .vtable = teenVtable,
    .heap = undefined,
    .tos = undefined,
    .allocated = undefined,
    .collectTo = undefined,
    .size = 4096,
};
const teenVtable =  Arena.Vtable {
    .getGlobal = getGlobalNext,
};
const GlobalArena = Arena {
    .vtable = globalVtable,
    .heap = undefined,
    .tos = undefined,
    .allocated = undefined,
    .collectTo = undefined,
    .size = 32768,
};
const globalVtable =  Arena.Vtable {
    .getGlobal = getGlobalSelf,
};
pub const TestArena = Arena {
    .vtable = testVtable,
    .heap = undefined,
    .tos = undefined,
    .allocated = undefined,
    .collectTo = undefined,
    .size = 2048,
};
const testVtable =  Arena.Vtable {
    .getGlobal = getGlobalSelf,
};
fn getGlobalSelf(self: *Arena) *Arena {
    return self;
}

pub const Arena = struct {
    vtable: Vtable,
    heap: HeapPtr,
    tos: [*]Object,
    allocated: []Object,
    collectTo: ?*Arena,
    size: usize,
    const Self = Arena;
    const Vtable = struct {
        getGlobal : arenaStar_to_arenaStar,
    };
    const arenaStar_to_arenaStar = fn (self: *Arena) *Arena;
    const vtable = Vtable {
        .getGlobal = getGlobalV,
    };
    fn getGlobalV(_: *Arena) *Arena {
        @panic("missing vtable:getGlobal");
    }
    pub fn getGlobal(self: *Arena) *Arena {
        return self.vtable.getGlobal(self);
    }
    pub fn setCollectTo(self: *Arena, collectTo: ?*Arena) void {
        self.collectTo = collectTo;
    }
    pub fn init(self: *const Arena, collectTo: ?*Arena) !Arena {
        const allocated = std.heap.page_allocator.alloc(Object,self.size) catch |err| return err;
        //try std.io.getStdOut().writer().print("allocated ptr=0x{x:0>16} len={}\n",.{@ptrToInt(allocated.ptr),allocated.len});
        return Arena {
            .vtable = self.vtable,
            .heap = @ptrCast(HeapPtr,allocated.ptr),
            .tos = allocated.ptr+allocated.len,
            .collectTo = collectTo,
            .allocated = allocated,
            .size = self.size,
        };
    }
    pub fn with(self: *const Arena, expected: []Object) !Arena {
        const size = expected.len+4;
        const allocated = std.heap.page_allocator.alloc(Object,size) catch |err| return err;
        //const stdout =  std.io.getStdOut().writer();
        //try stdout.print("allocated ptr=0x{x} len={}\n",.{@ptrToInt(allocated.ptr),allocated.len});

        //try stdout.print("heap=0x{x:0>16} tos=0x{x:0>16}\n",.{@ptrToInt(allocated.ptr+1),@ptrToInt(allocated.ptr+1+expected.len)});
        return Arena {
            .vtable = self.vtable,
            .heap = @ptrCast(HeapPtr,allocated.ptr+1),
            .tos = allocated.ptr+1+expected.len,
            .collectTo = null,
            .allocated = allocated,
            .size = size,
        };
    }
    pub fn deinit(self : *Self) void {
        //std.io.getStdOut().writer().print("deallocate ptr=0x{x:0>16} len={}\n",.{@ptrToInt(self.allocated.ptr),self.allocated.len}) catch unreachable;
        std.heap.page_allocator.free(self.allocated);
        self.* = undefined;
    }
    fn verify(self: *Self, expected: []Object) !void {
        const stdout =  std.io.getStdOut().writer();
        if (@ptrCast([*]Object,self.heap)!=self.tos) {
            stdout.print("heap=0x{x:0>16} tos=0x{x:0>16}\n",.{@ptrToInt(self.heap),@ptrToInt(self.tos)}
                         ) catch unreachable;
            return error.HeapDidnotEndUpAtEnd;
        }
        var ok=true;
        for (expected) |item, index| {
            if (@bitCast(u64,self.allocated[index+1]) != @bitCast(u64,item)) {
                if (item.is_double() and std.math.fabs(item.to(f64))<0.00001) {
                    try stdout.print("comparing[{}] expected=0x{x:0>16} output=0x{x:0>16}\n",
                                 .{index,@bitCast(u64,item),@bitCast(u64,self.allocated[index+1])});
                } else try stdout.print("comparing[{}] expected={} output={}\n",.{index,item,self.allocated[index+1]});
                ok = false;
            }
        }
        if (!ok) return error.OutputDiffered;
    }
    pub fn allocObject(self : *Self, classIndex : class.ClassIndex, format: Format, iv_size : usize, array_size : usize) HeapPtr {
        var form = format.noBase();
        var totalSize = iv_size + array_size + 1;
        if (iv_size>0) form=form.object();
        if (array_size>0) {
            form=form.array();
            if (array_size>32766) form=form.object(); // big arrays use Format.both
        }
        if (format.isWeak()) form=form.weak();
        if (form.hasBoth()) totalSize += 1;
        const size = if (form.hasInstVars()) iv_size else array_size;
        return self.alloc(classIndex, form, iv_size, array_size, size, totalSize, object.Nil);
    }
    pub fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, comptime T: type) HeapPtr {
        const objectWidth = @sizeOf(Object);
        const width = @sizeOf(T);
        const asize = (array_size*width+objectWidth-width)/objectWidth;
        const form = Format.none.raw(T,array_size);
        const size = @minimum(asize,32767);
        var totalSize = asize+(if (size<asize) @as(usize,2) else 1);
        return self.alloc(classIndex, form, 0, asize, size, totalSize, object.ZERO); //,&[0]Object{});
    }
    inline fn alloc(self: *Self, classIndex: class.ClassIndex, form: Format, iv_size: usize, asize: usize, size: usize, totalSize: usize, fill: Object) HeapPtr {
        const result = self.heap;
        self.heap = @ptrCast(HeapPtr,@ptrCast([*]Header,self.heap) + totalSize);
        const hash = if (builtin.is_test) 0 else @intCast(u24,@ptrToInt(result)%16777213);
        const head = header(@intCast(u16,size),form,classIndex,hash);
        result.* = @bitCast(Object,head);
        mem.set(Object,@ptrCast([*]Object,result)[1..totalSize],fill);
        if (totalSize>size+1) @ptrCast([*]Object,result)[iv_size+1] = @bitCast(Object,asize);
        return @ptrCast(HeapPtr,result);
    }
    pub inline fn allocString(self: *Self, source: []const u8) HeapPtr {
        const result = self.allocRaw(class.String,source.len,u8);
        //const stdout = std.io.getStdOut().writer();
        //stdout.print("result={} ptr=0x{x:0>16} len={}\n",.{result,@ptrToInt(result.indexables(u8).ptr),result.indexables(u8).len}) catch unreachable;
        mem.copy(u8,result.indexables(u8),source);
        return result;
    }
    pub inline fn allocGlobalString(self: *Self, source: []const u8) HeapPtr {
        return self.getGlobal().allocString(source);
    }
};
test "four object allocator" {
    // const stdout = std.io.getStdOut().writer();
    const testing = std.testing;
    const h1 = header(3,Format.object,42,0);
    const h2 = header(1,Format.both,43,0);
    const h3 = header(2,Format.array,44,0);
    const h4 = header(2,Format.raw64,45,0);
    const expected = ([_]Object{
        h1.asObject(),True,Nil,False,
        h2.asObject(),True,@bitCast(Object,@as(u64,1)),False,
        h3.asObject(),Nil,True,
        h4.asObject(),@bitCast(Object,@as(i64,0)),@bitCast(Object,@as(i64,1)),
    })[0..];
    var testArena = try TestArena.with(expected);
    const obj1 = testArena.allocObject(42,Format.none,3,0);
    try testing.expectEqual(@as(usize,4),obj1.totalSize());
    const obj2 = testArena.allocObject(43,Format.none,1,1);
    try testing.expectEqual(@as(usize,4),obj2.totalSize());
    const obj3 = testArena.allocObject(44,Format.none,0,2);
    try testing.expectEqual(@as(usize,3),obj3.totalSize());
    const obj4 = testArena.allocRaw(45,2,u64);
    try testing.expectEqual(@as(usize,3),obj4.totalSize());
    const ivs4 = obj4.instVars();
    try testing.expectEqual(@as(usize,0),ivs4.len);
    const idx4 = obj4.indexables(i64);
    try testing.expectEqual(@as(usize,2),idx4.len);
    idx4[1]=1;
    const ivs3 = obj3.instVars();
    try testing.expectEqual(@as(usize,0),ivs3.len);
    const idx3 = obj3.indexables(Object);
    try testing.expectEqual(@as(usize,2),idx3.len);
    idx3[1]=True;
    const ivs2 = obj2.instVars();
    try testing.expectEqual(@as(usize,1),ivs2.len);
    ivs2[0]=True;
    const idx2 = obj2.indexables(Object);
    try testing.expectEqual(@as(usize,1),idx2.len);
    idx2[0]=False;
    const ivs1 = obj1.instVars();
    try testing.expectEqual(@as(usize,3),ivs1.len);
    ivs1[0]=True;
    ivs1[2]=False;
    // try stdout.print("obj1*=0x{x:0>16}: {}\n",.{@ptrToInt(obj1),obj1});
    // try stdout.print("obj2*=0x{x:0>16}: {}\n",.{@ptrToInt(obj2),obj2});
    // try stdout.print("obj3*=0x{x:0>16}: {}\n",.{@ptrToInt(obj3),obj3});
    // try stdout.print("obj4*=0x{x:0>16}: {}\n",.{@ptrToInt(obj4),obj4});
    try testArena.verify(expected);
}
test "string allocator" {
    //const stdout = std.io.getStdOut().writer();
    const testing = std.testing;
    const h1 = header(1,Format.none.raw(u8,5),class.String,0);
    const h2 = header(2,Format.none.raw(u8,5),class.String,0);
    const expected = ([_]Object{
        h1.asObject(),object.fromLE(0x0000006f6c6c6568),
        h2.asObject(),object.fromLE(0x646e612073696874),object.fromLE(0x0000007461687420),
    })[0..];
    var testArena = try TestArena.with(expected);
    const obj1 = testArena.allocGlobalString("hello"[0..]);
    try testing.expectEqual(@as(usize,5),obj1.indexables(u8).len);
    //try stdout.print("obj1*=0x{x:0>16}: \"{}\"\n",.{@ptrToInt(obj1),obj1});
    const obj2 = testArena.allocGlobalString("this and that"[0..]);
    try testing.expectEqual(@as(usize,13),obj2.indexables(u8).len);
    //try stdout.print("obj2*=0x{x:0>16}: \"{}\"\n",.{@ptrToInt(obj2),obj2});
    try testArena.verify(expected);
}
