const std = @import("std");
const builtin = @import("builtin");
var next_thread_number : u64 = 0;
const Object = @import("object.zig").Object;
const arenas = @import("arenas.zig");
const heap = @import("heap.zig");
const dispatch = @import("dispatch.zig");
const HeapPtr = heap.HeapPtr;
const ex = @import("execute.zig");
const Hp = ex.Hp;
const Code = ex.Code;
const ContextPtr = ex.CodeContextPtr;
const tailCall = ex.tailCall;

test "force dispatch load" {
    dispatch.forTest();
}
const thread_total_size = 64*1024; //std.mem.page_size;
pub const Thread = extern struct {
    next: ?*Self,
    id : u64,
    nursery : nurseryType, // align(@alignOf(arenas.NurseryArena)),
//    teen1 : arenas.TeenArena,
//    teen2 : arenas.TeenArena,
    const Self = @This();
    const nurseryType = arenas.NurseryArena(nursery_size);
    const thread_size = @sizeOf(u64)+@sizeOf(?*Self);
    const threadAvail = thread_total_size-thread_size;
    const nursery_size = @min(threadAvail/7/@sizeOf(Object),heap.Header.maxLength);
    const teen_size = (threadAvail-nursery_size)/2/@sizeOf(Object);

    pub fn new() Self {
        return Self {
            .next = undefined,
            .id = undefined,
            .nursery = nurseryType.new(),
//            .teen1 = arenas.TeenArena.new(),
//            .teen2 = arenas.TeenArena.new(),
        };
    }
    pub fn init(self: *Self) void {
        defer next_thread_number += 1;
        self.next = null;
        self.id = next_thread_number;
        self.nursery.init();
//        self.teen1.init(&self.teen2);
//        self.teen2.init(&self.teen1);
    }
    const checkType = u5;
    const checkMax:checkType = @truncate(checkType,0x7fffffffffffffff);
    pub inline fn needsCheck(self: *const Self) bool {
        return @truncate(checkType,@ptrToInt(self))==1;
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
    pub inline fn noCheck(self: *Self) *Self {
        return @intToPtr(*Self,@ptrToInt(self) & ~@as(usize,checkMax));
    }
    inline fn ptr(self: *Self) *Self {
        return @intToPtr(*Self,@ptrToInt(self.noCheck()) // + @sizeOf(heap.Header)
                         );
    }
    pub fn deinit(self : *Self) void {
        self.ptr().heap.deinit();
        self.ptr().* = undefined;
    }
    pub inline fn getHeap(self: *Self) heap.HeaderArray {
        return self.ptr().nursery.getHp();
    }
    pub inline fn getArena(self: *Self) *heap.Arena {
        return self.ptr().nursery.asArena();
    }
    pub inline fn endOfStack(self: *Self) [*]Object {
        return self.ptr().nursery.endOfStack();
    }
    pub inline fn stack(self: *Self, sp: [*]Object) []Object {
        return sp[0..(@ptrToInt(self.endOfStack())-@ptrToInt(sp))/@sizeOf(Object)];
    }
    pub fn check(pc: [*]const Code, sp: [*]Object, hp: Hp, self: *Thread, context: ContextPtr, selectorHash: u32) void {
//        if (self.ptr().debug) |debugger|
//            return  @call(tailCall,debugger,.{pc,sp,hp,self,context,selector});
        @call(tailCall,pc[0].prim,.{pc+1,sp,hp,self,context,selectorHash});
    }
    pub fn checkStack(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        return @call(tailCall,Thread.check,.{pc,sp,hp,thread,context});
    }
};
test "check flag" {
    const testing = std.testing;
    var thread = Thread.new();
    var thr = &thread;
    thr.init();
    try testing.expect(!thr.needsCheck());
    const origEOS = thr.endOfStack();
    thr = thr.maxCheck();
    try testing.expect(!thr.needsCheck());
    var count = Thread.checkMax-1;
    while (count>1) : (count -= 1) {
        thr = thr.decCheck();
    }
    try testing.expect(!thr.needsCheck());
    try testing.expectEqual(thr.endOfStack(),origEOS);
    thr = thr.decCheck();
    try testing.expect(thr.needsCheck());
}
