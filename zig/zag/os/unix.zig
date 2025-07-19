// Some code from https://github.com/microsoft/mimalloc/blob/master/src/prim/unix/prim.c
const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
pub const page_size = std.heap.page_size_min;
const builtin = @import("builtin");
const os = std.posix;

pub fn MemoryAllocator(comptime Block: type) type {
    return struct {
        prealloc: []align(@sizeOf(Block)) Block,
        const Self = @This();
        pub fn new(baseAddress: usize) Self {
            return .{ .prealloc = @as([*]Block, @ptrFromInt(baseAddress))[0..0] };
        }
        fn reset(self: *Self) void {
            self.prealloc = &[0]Block{};
        }
        pub fn allocBlock(self: *Self) !*align(@sizeOf(Block)) Block {
            // this whole funtion should be locked
            if (self.prealloc.len == 0) {
                const newAlloc = try reserve(Block, @ptrCast(self.prealloc.ptr));
                self.prealloc = newAlloc;
            }
            const next = &self.prealloc[0];
            self.prealloc = self.prealloc[1..];
            return next;
        }
        pub fn allocBlockAtAddress(self: *Self, address: usize) !*align(@sizeOf(Block)) Block {
            while (true) {
                const next = try self.allocBlock();
                if (@intFromPtr(next) == address) return next;
                // if it's too low, discard, and loop
                // if it's too high, we presumably already discarded it, so map directly
                // but for now, it works
                std.debug.print("next: {*} address: {}\n", .{ next, address });
                unreachable;
            }
        }
        pub fn map(_: *Self, size: usize, alignment: ?u32) ![]u8 { // aligned to alignment
            if (alignment) |a|
                return alignedMap(null, size, a);
            return mmap(null, size, -1);
        }
        pub fn mapFile(_: *Self, size: usize, fd: os.fd_t) ![]align(page_size) u8 {
            return mmap(null, size, fd);
        }
        pub fn unmap(_: *Self, slice: []align(page_size) u8) void {
            os.munmap(slice);
        }
    };
}
const allocMultiple = 16;
const pageAddressType = [*]align(page_size) u8;

fn mmap(hint: pageAddressType, size: usize, fd: os.fd_t) ![]align(page_size) u8 {
    if (builtin.os.tag == .windows) @compileError("no windows support"); // see PageAllocator for ideas
    return os.mmap(
        hint,
        size,
        os.PROT.READ | os.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .FIXED = true, .ANONYMOUS = true, .NORESERVE = true },
        fd,
        0,
    );
}
fn alignedMap(hint: pageAddressType, allocation: usize, alignment: usize) ![]u8 {
    // may return alignment smaller than allocation so bump up if need
    assert(@as(usize, alignment) >> @as(u6, @truncate(@ctz(alignment))) == 1); // alignedMap must have a power-of-2 alignment
    assert(alignment >= page_size); // alignedMap must have a alignment >= than page_size
    const slice = try mmap(hint, allocation, -1);
    const addr = @intFromPtr(slice.ptr);
    const first = mem.alignForward(usize, addr, alignment);
    const end = @intFromPtr(slice.ptr + slice.len);
    const last = mem.alignBackward(usize, end, alignment);
    assert(first < last);
    if (addr < first) os.munmap(slice[0 .. first - addr]);
    if (last < end) os.munmap(@as([*]align(page_size) u8, @ptrFromInt(last))[0 .. end - last]);
    return slice[first - addr .. last - addr];
}
fn reserve(comptime T: type, hint: pageAddressType) ![]align(@sizeOf(T)) T {
    const size = @sizeOf(T);
    const supersize = // if (os.MAP.NORESERVE>0) 1<<31 else
        size * allocMultiple;
    const res = try alignedMap(hint, supersize, size);
    return @alignCast(@as([*]T, @ptrCast(@alignCast(res.ptr)))[0 .. res.len / size]);
}

test "simple allocation" {
    // _ = try alignedMap(null,1<<20+1,1<<20);
    if (false) {
        const Test = [512 * 64]usize;
        const MAType = MemoryAllocator(Test);
        var memAlloc = MAType.new();
        var block0 = try memAlloc.allocBlock();
        const other1 = try memAlloc.map(100, null);
        const other2 = try memAlloc.map(100, null);
        std.debug.print("other1.ptr={x} len={}\n", .{ @intFromPtr(other1.ptr), other1.len });
        std.debug.print("other2.ptr={x} len={}\n", .{ @intFromPtr(other2.ptr), other2.len });
        const block1 = try memAlloc.allocBlock();
        const block2 = try memAlloc.allocBlock();
        const block3 = try memAlloc.allocBlock();
        const block4 = try memAlloc.allocBlock();
        memAlloc.reset();
        const block5 = try memAlloc.allocBlock();
        const block6 = try memAlloc.allocBlock();
        const block7 = try memAlloc.allocBlock();
        const other3 = try memAlloc.map(100, null);
        const other4 = try memAlloc.map(100, null);
        std.debug.print("other3.ptr={x} len={}\n", .{ @intFromPtr(other3.ptr), other3.len });
        std.debug.print("other4.ptr={x} len={}\n", .{ @intFromPtr(other4.ptr), other4.len });
        memAlloc.reset();
        const block8 = try memAlloc.allocBlock();
        const block9 = try memAlloc.allocBlock();
        const block10 = try memAlloc.allocBlock();
        const other5 = try memAlloc.map(100, null);
        const other6 = try memAlloc.map(100, null);
        std.debug.print("other5.ptr={x} len={}\n", .{ @intFromPtr(other5.ptr), other3.len });
        std.debug.print("other6.ptr={x} len={}\n", .{ @intFromPtr(other6.ptr), other4.len });
        block0[5] = 42;
        std.debug.print("blocks = {any}\n", .{[_]*Test{
            block0,
            block1,
            block2,
            block3,
            block4,
        }});
        std.debug.print("blocks = {any}\n", .{[_]*Test{
            block5,
            block6,
            block7,
        }});
        std.debug.print("blocks = {any}\n", .{[_]*Test{
            block8,
            block9,
            block10,
        }});
    }
}
