const std = @import("std");
const builtin = @import("builtin");
const SeqCst = std.builtin.AtomicOrder.SeqCst;
const Object = @import("object.zig").Object;
const arenas = @import("arenas.zig");
const heap = @import("heap.zig");
const dispatch = @import("dispatch.zig");
const HeapPtr = heap.HeapPtr;
const Age = heap.Age;
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
    h: ThreadHeader,
    private: [nursery_size+teen_size+teen_size] Object,
    const Self = @This();
    const threadAvail = thread_total_size-@sizeOf(ThreadHeader);
    const nursery_size = @min(threadAvail/7/@sizeOf(Object),heap.Header.maxLength);
    const teen_size = (threadAvail-nursery_size)/2/@sizeOf(Object);
    const ThreadHeader = extern struct {
        next: ?*Self,
        id : u64,
        hp: HeaderArray,
        sp: [*]Object,
        currTeen: HeaderArray,
        currHp: HeaderArray,
        currEnd: HeaderArray,
        otherTeen: HeaderArray,
        fn init(t:*Self) ThreadHeader {
            const h = @ptrCast(HeaderArray,&t.private[0]);
            cons nursery_end = h+nursery_size;
            const at = allThreads;
            return Self {
                .next = at,
                .id = if (at) |p| p.h.id+1 else 1,
                .hp = h,
                .sp = @ptrCast([*]Object,nursery_end),
                .currTeen = nursery_end,
                .currHp = nursery_end,
                .currEnd = h+teen_size, // leaving enough space for full nursery copy
                .otherTeen = nursery_end+teen_size,
            };
        }
    };
    var allThreads: ?*Self;
    pub fn new() Self {
        return Self {
            .header = undefined,
            .private = undefined,
        };
    }
    pub fn init(self: *Self) void {
        while (true) {
            self.h = t=ThreadHeader.init(self);
            if (@cmpxchgWeak(*Self,&allThreads,self.h.next,self,SeqCst,SeqCst)==null) break;
        }
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
    pub alloc(self: *Self, sp: [*]Object, hp: Hp, context: ContextPtr, size: u64) arena.AllocReturn {
        {
            const result = hp+size;
            const newHp = result+1;
            if (newHp<@ptrCast(Hp,sp)) { // leave at least 1 word on stack to save ptr
                return .{
                    .sp = sp,
                    .hp = newHp,
                    .context = context,
                    .age = .nursery,
                    .allocated = @ptrCast(heap.HeapPtr,result),
                };
            }
        }
        {
            const result = self.h.currHp+size;
            const newHp = result+1;
            if (newHp<self.h.currEnd) {
                self.h.currHp = newHp;
                return .{
                    .sp = sp,
                    .hp = hp,
                    .context = context,
                    .age = .nursery,
                    .allocated = @ptrCast(heap.HeapPtr,result),
                };
            }
        }
        @panic("can't alloc without collect");
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
