const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const thread = @import("thread.zig");
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const native_endian = builtin.target.cpu.arch.endian();
const pow2 = @import("utilities.zig").largerPowerOf2;

pub const Format = enum(u8) {
    none = 0,
    object = InstVarsWithPtrs,
    array = IndexableWithPtrs,
    both = InstVarsWithPtrs + IndexableWithPtrs,
    weak = Weak,
    objectNP = InstVars,
    arrayNP = Indexable_64,
    bothNP = InstVars + Indexable_64,
    immutable = Immutable,
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
    pub inline fn weak(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) & ~InstVars | Weak);
    }
    pub inline fn object(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) | InstVars);
    }
    pub inline fn withoutIndexable(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) & ~IndexableFormat);
    }
    pub inline fn plusIndexable(self: Self, n: u8) Self {
        return @intToEnum(Self,@enumToInt(self.withoutIndexable())+n);
    }
    pub inline fn justIndexable(self: Self) Self {
        return @intToEnum(Self,@enumToInt(self) & (IndexableFormat+Immutable));
    }
    pub inline fn array(self: Self) Self {
        return self.plusIndexable(IndexableWithPtrs);
    }
    pub inline fn raw(self: Self, comptime T : type, size : usize) Self {
        switch (T) {
            u8,i8 => return self.plusIndexable(Indexable_8 + @intCast(u8,(-@intCast(isize,size))&7)),
            u16,i16 => return self.plusIndexable(Indexable_16 + @intCast(u8,(-@intCast(isize,size))&3)),
            u32,i32,f32 => return self.plusIndexable(Indexable_32 + @intCast(u8,(-@intCast(isize,size))&1)),
            u64,i64,f64 => return self.plusIndexable(Indexable_64),
            else => return self,
        }
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
    try expect(Format.object.hasInstVars());
    try expect(!Format.object.isPointerFree());
    try expect(Format.objectNP.isPointerFree());
    try expect(!Format.object.isIndexable());
    try expect(!Format.object.isWeak());
    try expect(Format.weak.isWeak());
}
pub const HeaderArray = [*]align(@alignOf(u64)) Header;
pub const HeapPtr = *align(@alignOf(u64)) Header;
pub const HeapConstPtr = *align(@alignOf(u64)) const Header;
const heapMethods = struct {
    const forwardLength : u16 = 4095;
    const indirectLength : u16 = 4094;
    const maxLength = @minimum(indirectLength-1,pow2(@as(u16,HeapAllocation.maxObjects/2)));
    pub inline fn forwardedTo(self: HeapConstPtr) HeapConstPtr {
        return @intToPtr(HeapConstPtr,@intCast(u64,@intCast(i64,@bitCast(u64,self.*)<<16)>>16));
    }
    pub inline fn isForwarded(self: HeapConstPtr) bool {
        return self.length==forwardLength;
    }
    pub inline fn isForwardedOrExchanged(self: HeapConstPtr) bool {
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
    pub fn arrayAsSlice(self: HeapConstPtr, comptime T: type) []T {
        const scale = @sizeOf(Object)/@sizeOf(T);
        const ptr = self.forwarded();
        const form = ptr.objectFormat;
        if (form.isRaw()) {
            const formi = @enumToInt(form);
            var size :usize = ptr.length;
            var oa = ptr.asObjectArray();
            if (ptr.isIndirect()) {
                size = @bitCast(usize,oa[0]);
                oa += 1;
                @panic("incomplete");
            }
            return mem.bytesAsSlice(
                T,
                if (formi>=Format.Indexable_8) @ptrCast([*]T,oa)[0..size*scale-(formi&7)]
                    else if (formi>=Format.Indexable_16) @ptrCast([*]T,oa)[0..size*scale-(formi&3)]
                    else if (formi>=Format.Indexable_32) @ptrCast([*]T,oa)[0..size*scale-(formi&1)]
                    else @ptrCast([*]T,oa)[0..size*scale]);
        } else {
            const size = ptr.length;
            const oa = ptr.asObjectArray();
            return mem.bytesAsSlice(
                T,
                if (!form.isIndexable()) (&[0]T{})
                    else if (form.hasInstVars()) @ptrCast([*]T,oa+size+1)[0..@bitCast(usize,oa[size])*scale]
                    else @ptrCast([*]T,oa)[0..size*scale]);
        }
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
    pub inline fn asObjectPtr(self: HeapConstPtr) [*]Object {
        return @bitCast([*]Object,self);
    }
    pub inline fn fromObjectPtr(op:  [*]const Object) [*]Header {
        return @intToPtr([*]Header,@ptrToInt(op));
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
        oa += if (size >= indirectLength) 1 else size;
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
pub const Age = enum(u4) {
    stack = Stack,
    nursery = Nursery,
    teen = FirstTeen,
    global = Global,
    static = Static,
    _,
    const Stack: u4 = 0;
    const Nursery: u4 = 1;
    const FirstTeen: u4 = 2;
    const LastTeen: u4 = 7;
    const Global: u4 = 8;
    const Static: u4 = 15;
    const Self = @This();
    pub fn isGlobal(self: Self) bool {
        return @enumToInt(self)>=Global;
    }
};
pub const Header = switch (native_endian) {
    .Big => packed struct {
        length: u12, // align(8),
        age: Age,
        objectFormat: Format,
        hash: u24,
        classIndex: u16,
        usingnamespace heapMethods;
        pub const includesHeader = true;
    },
    .Little => packed struct {
        classIndex: u16, // align(8),
        hash: u24,
        objectFormat: Format,
        age: Age,
        length: u12,
        usingnamespace heapMethods;
        pub const includesHeader = true;
    },
};
pub inline fn header(length : u12, format : Format, classIndex : u16, hash: u24, age: Age) Header {
    return Header {
        .length = length,
        .age = age,
        .objectFormat = format,
        .hash = hash,
        .classIndex = classIndex,
    };
}
test "Header structure" {
    const testing = std.testing;
    try testing.expectEqual(@sizeOf(Header),8);
    const hdr = header(17, Format.object, 35,0x123,Age.teen);
    try testing.expectEqual(hdr.o().u(),0x0112600001230023);
}
pub inline fn arenaFree(stackPointer: [*]const Object, heapPointer: [*]Header) isize {
    return @divFloor(@bitCast(isize,(@ptrToInt(stackPointer)-%@ptrToInt(heapPointer))),@sizeOf(Object));
}
test "arenaFree" {
    const testing = std.testing;
    const stack: [10]Object align(8) =undefined;
    const s1: [*]const Object = @ptrCast([*]const Object,&stack[1]);
    const s5 = s1+4;
    const hp: [*]Header = Header.fromObjectPtr(s1+2);
    try testing.expectEqual(arenaFree(s5,hp),2);
    try testing.expectEqual(arenaFree(s1,hp),-2);
}
fn instead(int: u32,int2: u32, buf: []u8) []const u8 {
    return std.fmt.bufPrint(buf, "{} instead of {}", .{int,int2}) catch unreachable;
}
fn checkEqual(int: u32,int2: u32) ?[]const u8 {
    if (int==int2) return null;
    var buf: [20]u8 = undefined;
    return std.fmt.bufPrint(buf[0..], "{} instead of {}", .{int,int2}) catch unreachable;
}

const threadAvail = thread.avail_size;
const nursery_size = threadAvail/7/@sizeOf(Object);
const teen_size = (threadAvail-@sizeOf(NurseryArena))/2/@sizeOf(Object);
pub const NurseryArena = extern struct {
    const Self = @This();
    vtable:  Arena.Vtable,
    hp: [*]Header,
    sp: [*]Object,
    heapArea: [nursery_size-field_size/@sizeOf(Header)]Header,
    const field_size = @sizeOf(Arena.Vtable)+@sizeOf([*]Header)+@sizeOf([*]Object);
    comptime {
        if (checkEqual(@sizeOf(NurseryArena),nursery_size*@sizeOf(Header))) |s|
            @compileError("Modify NurseryArena.heap to make @sizeOf(NurseryArena) == " ++ s);
    }
    pub fn setThread(_: *Self, _: *thread.Thread) void {
    }
    pub fn init() NurseryArena {
        var result = NurseryArena {
            .vtable = vtable,
            .heapArea = undefined,
            .hp = undefined,
            .sp = undefined,
        };
        result.hp = @ptrCast([*]Header,&result.heapArea[0]);
        result.sp = result.endOfStack();
        return result;
    }
    pub inline fn endOfStack(self: *Self) [*]Object {
        return @intToPtr([*]Object,@ptrToInt(self.hp))+nursery_size;
    }
    const vtable =  Arena.Vtable {
        .alloc = alloc,
    };
    pub inline fn asArena(self: *Self) *Arena {
        return @ptrCast(*Arena,self);
    }
    pub inline fn getHp(self: *Self) [*]Header {
        return self.hp;
    }
    fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
        _ = arena; _ = totalSize;
        unreachable;
        // const self = @ptrCast(*Self,@alignCast(@alignOf(Self),arena));
        // const result = @alignCast(@alignOf([*]Header),self.hp);
        // const end = @ptrCast(HeapPtr,result + totalSize);
        // if (self.space()<0) {
        //     const stdout = std.io.getStdOut().writer();
        //     stdout.print("classIndex={} totalSize={} end=0x{X:0>16} toh=0x{X:0>16}\n",.{classIndex,totalSize,@ptrToInt(end),@ptrToInt(self.toh)}) catch unreachable;
        //     return error.HeapFull;
        // }
        // self.hp = end;
        // const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        // const head = header(@intCast(u12,size),form,classIndex,hash,age);
        // result.* = @bitCast(Object,head);
        // mem.set(Object,@ptrCast([*]Object,result)[1..totalSize],fill);
        // if (totalSize>size+1) @ptrCast([*]Object,result)[iv_size+1] = @bitCast(Object,asize);
        // return @ptrCast(HeapPtr,result);
    }
};

pub const TeenArena = extern struct {
    const Self = @This();
    vtable:  Arena.Vtable,
    heap: [teen_size-field_size/@sizeOf(Header)]Header,
    const field_size = @sizeOf(Arena.Vtable);
    comptime {
        if (checkEqual(@sizeOf(TeenArena),teen_size*@sizeOf(Header))) |s|
            @compileError("Modify TeenArena.heap to make @sizeOf(TeenArena) == " ++ s);
    }
    pub fn init() TeenArena {
        var result = TeenArena {
            .vtable = vtable,
            .heap = undefined,
        };
        return result;
    }
    pub fn setOther(_: *Self, _: *Self) void {
    }
    const vtable =  Arena.Vtable {
        .alloc = alloc,
    };
    pub inline fn asArena(self: *Self) *Arena {
        return @ptrCast(Arena,self);
    }
    fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
        _ = arena; _ = totalSize;
        unreachable;
    }
};
const HeapAllocation = extern struct {
    flags: u64,
    next: ?*HeapAllocation,
    mem: [size]u8,
    const Self = @This();
    const size = std.mem.page_size - @sizeOf(?*u8);
    const maxObjects = size/@sizeOf(Header);
    fn sweep(self: *Self) void {
        var ptr = @ptrCast([*]Header,&self.mem[0]);
        const end = ptr+maxObjects;
        while (ptr<end) {
            unreachable;
        }
    }
};
pub var globalArena = GlobalArena.init();
pub const GlobalArena = struct {
    const Self = @This();
    vtable:  Arena.Vtable,
    freeLists: [nFreeLists]FreeListPtr,
    pub fn init() GlobalArena {
        var result = GlobalArena {
            .vtable = Arena.Vtable {
                .alloc = alloc,
            },
            .freeLists = [_]FreeListPtr{null}**nFreeLists,
        };
        return result;
    }
    pub inline fn asArena(self: *Self) *Arena {
        return @ptrCast(*Arena,self);
    }
    fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
        _ = arena; _ = totalSize;
        unreachable;
    }
   pub fn promote(obj: Object) !Object {
       if (!obj.isHeap()) return obj;
       unreachable;
//       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
//       return result.asObject();
    }
    const FreeList = struct {
        header: Header,
        next: FreeListPtr,
    };
    const FreeListPtr = ?*FreeList;
    const nFreeLists = @ctz(u16,heapMethods.maxLength)+1;
    const allocationUnit = heapMethods.maxLength; // size in u64 units including the header
    fn findAllocationList(target: u16) usize {
        if (target > 1<<(nFreeLists-1)) return 0;
        return @maximum(1,@ctz(u16,pow2(target)));
    }
};
test "findAllocationList" {
    const ee = std.testing.expectEqual;
    const findAllocationList = GlobalArena.findAllocationList;
    try ee(findAllocationList(1),1);
    try ee(findAllocationList(2),1);
    try ee(findAllocationList(3),2);
    try ee(findAllocationList(4),2);
    try ee(findAllocationList(400),0);
}
pub const Arena = packed struct {
    vtable: Vtable,
    const Self = Arena;
    const Vtable = packed struct {
        alloc: fn(self: *Self, totalSize: usize) anyerror!HeapPtr,
    };
    const arenaStar_to_arenaStar = fn (self: *const Arena) *Arena;
    const vtable = Vtable {
        .alloc = undefined,
    };
    // pub fn space(self: *const Arena) isize {
    //     return @intCast(i64,@ptrToInt(self.toh))-@intCast(i64,@ptrToInt(self.heap));
    // }
    // pub fn setCollectTo(self: *Arena, collectTo: ?*Arena) void {
    //     self.collectTo = collectTo;
    // }
    // fn with(self: *const Arena, expected: []Object) !Arena {
    //     const size = expected.len+4;
    //     const allocated = try std.heap.page_allocator.alignedAlloc(Object,Object.alignment,size);
    //     //const stdout =  std.io.getStdOut().writer();
    //     //try stdout.print("allocated ptr=0x{x} len={}\n",.{@ptrToInt(allocated.ptr),allocated.len});

    //     //try stdout.print("heap=0x{x:0>16} tos=0x{x:0>16}\n",.{@ptrToInt(allocated.ptr+1),@ptrToInt(allocated.ptr+1+expected.len)});
    //     return Arena {
    //         .vtable = self.vtable,
    //         .heap = @ptrCast(HeapPtr,allocated.ptr+1),
    //         .toh = allocated.ptr+1+expected.len,
    //         .collectTo = null,
    //         .allocated = allocated,
    //         .size = size,
    //     };
    // }
    // pub fn deinit(self : *Self) void {
    //     //std.io.getStdOut().writer().print("deallocate ptr=0x{x:0>16} len={}\n",.{@ptrToInt(self.allocated.ptr),self.allocated.len}) catch unreachable;
    //     std.heap.page_allocator.free(self.allocated);
    //     self.* = undefined;
    // }
    fn alloc(self: *Arena, classIndex: class.ClassIndex, form: Format, iv_size: usize, asize: usize, size: usize, totalSize: usize, fill: Object, age: Age) !HeapPtr{
        const result = @ptrCast([*]Object,try self.vtable.alloc(self,totalSize));
        const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        const head = header(@intCast(u12,size),form,classIndex,hash,age);
        mem.set(Object,result[1..totalSize],fill);
        if (totalSize>size+1) result[iv_size+1] = @bitCast(Object,asize);
        result[0] = @bitCast(Object,head);
        return @ptrCast(HeapPtr,result);
    }
    pub fn allocObject(self : *Self, classIndex : class.ClassIndex, format: Format, iv_size : usize, array_size : usize, age: Age) !HeapPtr {
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
        return self.alloc(classIndex, form, iv_size, array_size, size, totalSize, object.Nil,age);
    }
    pub fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, comptime T: type, age: Age) !HeapPtr {
        const objectWidth = @sizeOf(Object);
        const width = @sizeOf(T);
        const asize = (array_size*width+objectWidth-width)/objectWidth;
        const form = Format.none.raw(T,array_size);
        const size = @minimum(asize,heapMethods.maxLength);
        var totalSize = asize+@as(usize,if (size<asize) 2 else 1);
        return self.alloc(classIndex, form, 0, asize, size, totalSize, object.ZERO,age);
    }
    pub fn allocStruct(self : *Self, classIndex : class.ClassIndex, width : usize, comptime T: type, fill: Object, age: Age) !*T {
        const objectWidth = @sizeOf(Object);
        const asize = (width+objectWidth-1)/objectWidth;
        const form = Format.object;
        var totalSize = asize+1;
        return @ptrCast(*T,@alignCast(8,try self.alloc(classIndex, form, asize, 0, asize, totalSize, fill,age)));
    }
};
fn TempArena(size: usize) type {
    return extern struct{
        const Self = @This();
        vtable : Arena.Vtable,
        heap: [size]Object,
        ptr: usize,
        pub fn init() Self {
            return Self {
                .vtable = Arena.Vtable {
                    .alloc = alloc,
                },
                .heap = undefined,
                .ptr = 0,
            };
        }
        pub inline fn asArena(self: *Self) *Arena {
            return @ptrCast(*Arena,self);
        }
        fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
            const self = @ptrCast(*Self,@alignCast(@alignOf(Self),arena));
            if (self.ptr+totalSize>size) {
                std.debug.print("ptr = {} totalSize = {} size = {}\n",.{self.ptr,totalSize,size});
                return error.HeapFull;
            }
            const result = @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),&self.heap[self.ptr]));
            self.ptr += totalSize;
            return result;
        }
        fn verify(self: *Self, expected: []Object) !void {
            if (self.ptr!=size) {
                std.debug.print("ptr = {} expected {}\n",.{self.ptr,size});
                return error.HeapDidnotEndUpAtEnd;
            }
            var ok=true;
            for (expected) |item, index| {
                if (!self.heap[index].equals(item) and
                        !(item.isDouble() and std.math.fabs(self.heap[index].to(f64)-item.to(f64))<0.00001)) {
                    std.debug.print("comparing[{}] expected={} output={}\n",.{index,item,self.heap[index]});
                    ok = false;
                }
            }
            if (!ok) return error.OutputDiffered;
        }
    };
}
test "temp arena" {
    const testing = std.testing;
    var arena = TempArena(5).init();
    const testArena = arena.asArena();
    const obj1 : HeapPtr = try testArena.allocObject(42,Format.none,3,0,Age.stack);
    try testing.expectEqual(@alignOf(@TypeOf(obj1)),Object.alignment);
    try testing.expect(obj1.asObject().isHeap());
    try testing.expectEqual(obj1.inHeapSize(),4);
    try testing.expectError(error.HeapFull,testArena.allocObject(42,Format.none,3,0,Age.stack));
}
test "slicing" {
    const testing = std.testing;
    var arena = TempArena(16).init();
    const testArena = arena.asArena();
    const hp0 = try testArena.allocObject(42,Format.none,1,0,Age.stack);
    try testing.expectEqual(hp0.arrayAsSlice(u8).len,0);
    const hp1 = try testArena.allocObject(42,Format.none,1,2,Age.stack);
    const obj1 = hp1.asObject();
    try testing.expectEqual(hp1.arrayAsSlice(u8).len,16);
    try testing.expectEqual(obj1.arrayAsSlice(u8).len,16);
    try testing.expectEqual(hp1.arrayAsSlice(u8),obj1.arrayAsSlice(u8));
}
    
test "four object allocator" {
    const testing = std.testing;
    const h1 = header(3,Format.object,42,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h1).inHeapSize(),4);
    const h2 = header(1,Format.both,43,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h2).inHeapSize(),4);
    const h3 = header(2,Format.array,44,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h3).inHeapSize(),3);
    const h4 = header(2,Format.array,44,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h3).inHeapSize(),3);
    const expected = ([_]Object{
        h1.o(),True,Nil,False,
        h2.o(),True,@bitCast(Object,@as(u64,1)),False,
        h3.o(),Nil,True,
        h4.o(),@bitCast(Object,@as(u64,0)),@bitCast(Object,@as(u64,1)),
    })[0..];
    var arena = TempArena(expected.len).init();
    const testArena = arena.asArena();
    const obj1 = try testArena.allocObject(42,Format.none,3,0,Age.stack);
    try testing.expectEqual(obj1.inHeapSize(),4);
    const obj2 = try testArena.allocObject(43,Format.none,1,1,Age.stack);
    try testing.expectEqual(obj2.inHeapSize(),4);
    const obj3 = try testArena.allocObject(44,Format.none,0,2,Age.stack);
    try testing.expectEqual(obj3.inHeapSize(),3);
    const obj4 = try testArena.allocRaw(45,2,u64,Age.stack);
    try testing.expectEqual(obj4.inHeapSize(),3);
    const ivs4 = obj4.instVars();
    try testing.expectEqual(ivs4.len,0);
    const idx4 = obj4.indexables(i64);
    try testing.expectEqual(idx4.len,2);
    idx4[1]=1;
    const ivs3 = obj3.instVars();
    try testing.expectEqual(ivs3.len,0);
    const idx3 = obj3.indexables(Object);
    try testing.expectEqual(idx3.len,2);
    idx3[1]=True;
    const ivs2 = obj2.instVars();
    try testing.expectEqual(ivs2.len,1);
    ivs2[0]=True;
    const idx2 = obj2.indexables(Object);
    try testing.expectEqual(idx2.len,1);
    idx2[0]=False;
    const ivs1 = obj1.instVars();
    try testing.expectEqual(ivs1.len,3);
    ivs1[0]=True;
    ivs1[2]=False;
    // try stdout.print("obj1*=0x{x:0>16}: {}\n",.{@ptrToInt(obj1),obj1});
    // try stdout.print("obj2*=0x{x:0>16}: {}\n",.{@ptrToInt(obj2),obj2});
    // try stdout.print("obj3*=0x{x:0>16}: {}\n",.{@ptrToInt(obj3),obj3});
    // try stdout.print("obj4*=0x{x:0>16}: {}\n",.{@ptrToInt(obj4),obj4});
    try arena.verify(expected);
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
            var result : Self = .{
                .header = header((size+7)/8,Format.immutable.raw(u8,size),class.String_I,hash,Age.static),
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
    std.debug.print("abcde: {any}\n",.{abcde.h()});
    std.debug.print("abcde: {any}\n",.{abcde.asObject()});
}
