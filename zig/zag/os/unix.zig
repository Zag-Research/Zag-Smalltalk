// Originally from https://github.com/microsoft/mimalloc/blob/master/src/prim/unix/prim.c
const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const builtin = @import("builtin");
const os = std.os;
const c = std.c;
//const SC = os.SC;
const SC_PAGESIZE = 29;
extern "c" fn sysconf(sc: c_int) i64;

pub fn BlockAllocation(comptime Block: type) type {
    return struct {
        prealloc: []Block,
        const Self = @This();
        pub fn new() !Self {
            return .{.prealloc = &[0]Block{}};
        }
        fn reset(self: *Self) void {
            self.prealloc = &[0]Block{};
        }
        pub fn alloc(self: *Self) !*Block {
            if (self.prealloc.len==0) self.prealloc = try reserve(Block);
            const next = &self.prealloc[0];
            self.prealloc = self.prealloc[1..];
            return next;
        }
    };
}
const allocMultiple = 4;
var next_mmap_addr_hint: ?[*]align(mem.page_size) u8 = null;

fn reserve(comptime T: type) ![]T {
    const size = @sizeOf(T);
    if (size>>@ctz(@as(usize,size))!=1) @compileError("type must have a power-of-2 size");
    if (size<mem.page_size) @compileError("type must have a size >= than mem.page_size");
    const supersize = if (os.MAP.NORESERVE>0) 1<<31 else size*allocMultiple;
    std.debug.print("supersize = {}\n",.{supersize});
    
    if (builtin.os.tag == .windows) @compileError("no windows support");

    const hint = @atomicLoad(@TypeOf(next_mmap_addr_hint), &next_mmap_addr_hint, .Unordered);
    const slice = try os.mmap(
        hint,
        supersize,
        os.PROT.READ | os.PROT.WRITE,
        os.MAP.PRIVATE | os.MAP.ANONYMOUS | os.MAP.NORESERVE,
        -1,
        0,
    );
    std.debug.print("supersize={x}, len={x}\n",.{supersize,slice.len});
    const addr = @ptrToInt(slice.ptr);
    const first = mem.alignForward(addr, size);
    const end = @ptrToInt(slice.ptr+slice.len);
    const last = mem.alignBackward(end, size);
    std.debug.print("addr={x}, first={x}, last={x}, end={x}, last-first={x}\n",.{addr,first,last,end,last-first});
    _ = @cmpxchgStrong(@TypeOf(std.heap.next_mmap_addr_hint), &std.heap.next_mmap_addr_hint, hint, @intToPtr(@TypeOf(next_mmap_addr_hint),last), .Monotonic, .Monotonic);
    if (addr<first) os.munmap(slice[0..first-addr]);
    if (last<end) os.munmap(@intToPtr([*]align(mem.page_size)u8,last)[0..end-last]);
    const res = @intToPtr([*]T,first);
    return res[0..(end-first)/size];
}

test "nothing" {
    const Test = [512*64]usize;
    const BAType = BlockAllocation(Test);
    var blockAllocator = try BAType.new();
    var block0 = try blockAllocator.alloc();
    const other1 = try os.mmap(null,100,os.PROT.READ | os.PROT.WRITE,os.MAP.PRIVATE | os.MAP.ANONYMOUS,-1,0,);
    const other2 = try os.mmap(null,100,os.PROT.READ | os.PROT.WRITE,os.MAP.PRIVATE | os.MAP.ANONYMOUS,-1,0,);
    std.debug.print("other1.ptr={x} len={}\n",.{@ptrToInt(other1.ptr),other1.len});
    std.debug.print("other2.ptr={x} len={}\n",.{@ptrToInt(other2.ptr),other2.len});
    var block1 = try blockAllocator.alloc();
    var block2 = try blockAllocator.alloc();
    var block3 = try blockAllocator.alloc();
    var block4 = try blockAllocator.alloc();
    blockAllocator.reset();
    var block5 = try blockAllocator.alloc();
    var block6 = try blockAllocator.alloc();
    var block7 = try blockAllocator.alloc();
    const other3 = try os.mmap(null,100,os.PROT.READ | os.PROT.WRITE,os.MAP.PRIVATE | os.MAP.ANONYMOUS,-1,0,);
    const other4 = try os.mmap(null,100,os.PROT.READ | os.PROT.WRITE,os.MAP.PRIVATE | os.MAP.ANONYMOUS,-1,0,);
    std.debug.print("other3.ptr={x} len={}\n",.{@ptrToInt(other3.ptr),other3.len});
    std.debug.print("other4.ptr={x} len={}\n",.{@ptrToInt(other4.ptr),other4.len});
    block0[5]=42;
    std.debug.print("blocks = {any}\n",.{[_]*Test{block0,block1,block2,block3,block4,}});
    std.debug.print("blocks = {any}\n",.{[_]*Test{block5,block6,block7,}});
}
