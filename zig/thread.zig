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
pub const Thread = packed struct {
    header: heap.Header,
    id : u64,
    next: ?*Thread,
    debug: ?ex.ThreadedFn,
    nursery : heap.NurseryArena,
    tean1 : heap.TeanArena,
    teen2 : heap.TeanArena,
    const psm1 = std.mem.page_size-1;
    const thread_size = @sizeOf(Thread);
    const size = (thread_size+3000*@sizeOf(Object)+psm1)&-std.mem.page_size;
    const teen_size = size*5/12/@sizeOf(Object)*@sizeOf(Object); 
    const nursery_size = (size-thread_size-teen_size*2)/@sizeOf(Object)*@sizeOf(Object);
    const Self = @This();
    pub fn init(self: *Self) !void {
        defer next_thread_number += 1;
        self.id = next_thread_number;
        self.next = null;
        self.debug = null;
        self.nursery = try heap.NurseryArena.init(self);
        self.teen1 = try heap.TeenArena.init(&self.teen2);
        self.teen2 = try heap.TeenArena.init(&self.teen1);
    }
    pub fn initForTest(debugger: ?ex.ThreadedFn) !Self {
        if (builtin.is_test) {
            return Self {
                .id = 0,
                .nursery = try heap.TestArena.init(null),
                .next = null,
                .debug = debugger,
            };
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
