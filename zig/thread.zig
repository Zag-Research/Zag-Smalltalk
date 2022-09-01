const std = @import("std");
const builtin = @import("builtin");
var next_thread_number : u64 = 0;
const Object = @import("object.zig").Object;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const ex = @import("execute.zig");
const Code = ex.Code;
const ContextPtr = ex.ContextPtr;
const tailCall = ex.tailCall;
test "sizes" {
//    try std.testing.expect(Thread.size/@sizeOf(Object)<heap.externalPageSize);
}
pub const Thread = struct {
//    header: heap.Header,
    id : u64,
    nursery : heap.Arena,
//    tean1 : heap.Arena,
//    teen2 : heap.Arena,
    next: ?*Thread,
    const psm1 = std.mem.page_size-1;
    const thread_size = @sizeOf(Thread);
    const size = (thread_size+3000*@sizeOf(Object)+psm1)&-std.mem.page_size;
    const teen_size = size*5/12/@sizeOf(Object)*@sizeOf(Object); 
    const nursery_size = (size-thread_size-teen_size*2)/@sizeOf(Object)*@sizeOf(Object);
    const Self = @This();
    pub fn init() !Self {
        defer next_thread_number += 1;
        const arena = try heap.NurseryArena.init();
        return Self {
            .id = next_thread_number,
            .nursery = arena,
            .next = null,
        };
    }
    pub fn initForTest() !Self {
        if (builtin.is_test) {
            return Self {
                .id = 0,
                .nursery = try heap.TestArena.init(),
                .next = null,
            };
        }
        else unreachable;
    }
    pub fn deinit(self : *Self) void {
        self.heap.deinit();
        self.* = undefined;
    }
    pub inline fn getArena(self: *Self) *heap.Arena {
        return &self.nursery;
    }
    pub inline fn endOfStack(self: *Self) [*]Object {
        return self.getArena().toh;
    }
    pub fn check(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        _ = pc;
        _ = sp;
        _ = hp;
        _ = doCheck;
        _ = thread;
        _ = context;
        @panic("thread check");
    }
    pub fn check1(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        // I am the same as check, except the thread just executed had 1 parameter, so debug should see pc-2
        return @call(tailCall,Thread.check,.{pc,sp,hp,doCheck,thread,context});
    }
};
