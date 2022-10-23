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
    try std.testing.expectEqual(thread_size+(nursery_size+2*teen_size)*@sizeOf(Object),thread_total_size);
}
const thread_total_size = std.mem.page_size;
const thread_size = @sizeOf(_Thread);
pub const teen_size = thread_total_size*3/7/@sizeOf(Object);
pub const nursery_size = (thread_total_size-thread_size-2*teen_size*@sizeOf(Object))/@sizeOf(Object);

const _Thread = packed struct {
    id : u64,
    next: ?*_Thread,
    debug: ?ex.ThreadedFn,
};
pub const Thread = packed struct {
    thread: _Thread,
    nursery : heap.NurseryArena,
    teen1 : heap.TeenArena,
    teen2 : heap.TeenArena,
    const Self = @This();
    pub fn init(self: *Self) void {
        defer next_thread_number += 1;
        self.thread = .{
            .id = next_thread_number,
            .next = null,
            .debug = null,
        };
        self.nursery = heap.NurseryArena.init(self);
        self.teen1 = heap.TeenArena.init(&self.teen2);
        self.teen2 = heap.TeenArena.init(&self.teen1);
    }
    pub fn initForTest(debugger: ?ex.ThreadedFn) !Self {
        if (builtin.is_test) {
            var thr : Self = undefined;
            thr.init();
            thr.thread.debug=debugger;
            return thr;
        }
        else unreachable;
    }
    const checkType = u5;
    const checkMax:checkType = @truncate(checkType,0x7fffffffffffffff);
    pub inline fn needsCheck(self: *const Self) bool {
        return @truncate(checkType,@ptrToInt(self))==0;
    }
    pub inline fn decCheck(self: *Self) *Self {
        if (self.needsCheck()) return self;
        @setRuntimeSafety(false);
        return @intToPtr(*Self,@ptrToInt(self)-1);
    }
    pub inline fn maxCheck(self: *const Self) *Self {
        @setRuntimeSafety(false);
        return @intToPtr(*Self,@ptrToInt(self)|checkMax);
    }
    inline fn ptr(self: *const Self) *Self {
        return @intToPtr(*Self,@ptrToInt(self)&~@as(u64,checkMax));
    }
    pub fn deinit(self : *Self) void {
        self.ptr().heap.deinit();
        self.ptr().* = undefined;
    }
    pub inline fn getArena(self: *const Self) *heap.Arena {
        return &(self.ptr()).nursery;
    }
    pub inline fn endOfStack(self: *const Self) [*]Object {
        return self.ptr().getArena().toh;
    }
    pub fn check(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, self: *Thread, context: ContextPtr, selector: u64) void {
        if (self.ptr().debug) |debugger|
            return  @call(tailCall,debugger,.{pc,sp,hp,self,context,selector});
        @call(tailCall,pc[0].prim,.{pc+1,sp,hp,self,context,selector});
    }
    pub fn checkStack(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: u64) void {
        return @call(tailCall,Thread.check,.{pc,sp,hp,thread,context,selector});
    }
};
test "check flag" {
    const testing = std.testing;
    var thread = Thread.initForTest(null) catch unreachable;
    var thr = &thread;
    try testing.expect(thr.needsCheck());
    const origEOS = thr.endOfStack();
    thr = thr.maxCheck();
    try testing.expect(!thr.needsCheck());
    var count = Thread.checkMax-1;
    while (count>0) : (count -= 1) {
        thr = thr.decCheck();
    }
    try testing.expect(!thr.needsCheck());
    try testing.expectEqual(thr.endOfStack(),origEOS);
    thr = thr.decCheck();
    try testing.expect(thr.needsCheck());
}
