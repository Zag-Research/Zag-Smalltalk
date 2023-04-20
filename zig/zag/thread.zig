const std = @import("std");
const builtin = @import("builtin");
const SeqCst = std.builtin.AtomicOrder.SeqCst;
const Object = @import("object.zig").Object;
const arenas = @import("arenas.zig");
const dispatch = @import("dispatch.zig");
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const HeaderArray = heap.HeaderArray;
const Age = heap.Age;
const AllocErrors = heap.AllocErrors;
const ex = @import("execute.zig");
const Hp = ex.Hp;
const Code = ex.Code;
const ContextPtr = ex.CodeContextPtr;
const tailCall = ex.tailCall;
pub const AllocResult = struct {
    sp: [*]Object,
    hp: HeaderArray,
    context: ContextPtr,
    age: Age,
    allocated: HeapPtr,
};
pub const AllocReturn = AllocErrors!AllocResult;

test "force dispatch load" {
    dispatch.forTest();
}
const thread_total_size = 64*1024; //std.mem.page_size;
pub const Thread = extern struct {
    private: [stack_size+nursery_size+nursery_size] Object,
    h: ThreadHeader,
    const Self = @This();
    const threadAvail = thread_total_size-@sizeOf(ThreadHeader);
    const stack_size = @min(threadAvail/7/@sizeOf(Object),heap.Header.maxLength);
    const nursery_size = (threadAvail-stack_size)/2/@sizeOf(Object);
    const ThreadHeader = extern struct {
        next: ?*Self,
        id : u64,
        sp: [*]Object,
        currTeen: HeaderArray,
        currHp: HeaderArray,
        currEnd: HeaderArray,
        otherTeen: HeaderArray,
        fn init(t:*Self) ThreadHeader {
            const h = @ptrCast(HeaderArray,&t.private[0]);
            const stack_end = h+stack_size;
            const at = allThreads;
            return Self {
                .next = at,
                .id = if (at) |p| p.h.id+1 else 1,
                .sp = @ptrCast([*]Object,stack_end),
                .currTeen = stack_end,
                .currHp = stack_end,
                .currEnd = h+nursery_size, // leaving enough space for full nursery copy
                .otherTeen = stack_end+nursery_size,
            };
        }
    };
    var allThreads: ?*Self = null;
    pub fn new() Self {
        return Self {
            .h = undefined,
            .private = undefined,
        };
    }
    pub fn init(self: *Self) void {
        while (true) {
            self.h = ThreadHeader.init(self);
            if (@cmpxchgWeak(?*Self,&allThreads,self.h.next,self,SeqCst,SeqCst)==null) break;
        }
    }
    const checkType = u5;
    const checkMax:checkType = @truncate(checkType,std.mem.page_size-1);
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
        _=self;unreachable;
//        return self.ptr().nursery.getHp();
    }
    pub inline fn getArena(self: *Self) *heap.Arena {
        return self.ptr().nursery.asArena();
    }
    pub inline fn endOfStack(self: *Self) [*]Object {
        return @ptrCast([*]Object,&self.ptr().private[stack_size]);
    }
    pub inline fn stack(self: *Self, sp: [*]Object) []Object {
        return sp[0..(@ptrToInt(self.endOfStack())-@ptrToInt(sp))/@sizeOf(Object)];
    }
    pub fn check(pc: [*]const Code, sp: [*]Object, hp: Hp, self: *Thread, context: ContextPtr, selectorHash: u32) void {
//        if (self.ptr().debug) |debugger|
//            return  @call(tailCall,debugger,.{pc,sp,hp,self,context,selector});
        @call(tailCall,pc[0].prim,.{pc+1,sp,hp,self,context,selectorHash});
    }
    pub fn checkStack(self: Self, sp: [*]Object, words: u64) bool {
        return (sp-words)>=self;
    }
    pub fn alloc(self: *Self, sp: [*]Object, hp: Hp, context: ContextPtr, size: u64) heap.AllocReturn {
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

pub const Arena = extern struct {
    const Self = @This();

    alloc: *const fn (*Self,[*]Object,HeaderArray,ContextPtr,usize,usize) AllocErrors!AllocResult,
    collect: *const fn (*Self,[*]Object,HeaderArray,ContextPtr) AllocErrors!void,

    pub inline fn allocObject(self:*Self, sp:[*]Object, hp:HeaderArray, context:ContextPtr, classIndex:ClassIndex, ivSize:usize) AllocReturn {
        var result = try self.alloc(self,sp,hp,context,ivSize+1,0);
        initAllocation(result.allocated,classIndex, Format.objectNP, ivSize, result.age, Nil);
        return result;
    }
    pub fn allocArray(self:*Self, sp:[*]Object, hp:HeaderArray, context:ContextPtr, classIndex:ClassIndex, ivSize:usize, arraySize:usize, comptime T: type) AllocReturn {
        if (arraySize==0) return self.allocObject(sp,hp,context,classIndex,ivSize);
        const noIVs = ivSize==0;
        var form = (if (noIVs) Format.none else Format.objectNP).raw(T,arraySize);
        const width = @sizeOf(T);
        const aSize = (arraySize*width+objectWidth-width)/objectWidth;
        const fill = if (T==Object) Nil else object.ZERO;
        if (noIVs) {
            if (aSize<Header.maxLength) {
                var result = try self.alloc(self,sp,hp,context,aSize+1,0);
                initAllocation(result.allocated,classIndex, form, aSize, result.age, fill);
                return result;
            }
        }
        var result = try self.alloc(self,sp,hp,context,ivSize+3,aSize);
        const offs = @ptrCast([*]u64,result.allocated)+ivSize+1;
        mem.set(Object,@intToPtr([*]Object,offs[1])[0..aSize],fill);
        offs[0] = arraySize;
        initAllocation(result.allocated,classIndex, form.setObject(), ivSize, result.age, Nil);
        return result;
    }
    inline fn allocStruct(self: *Self, sp:[*]Object, hp:HeaderArray, context:ContextPtr, classIndex: class.ClassIndex, comptime T: type, extra: usize, comptime T2: type) AllocReturn {

        // should call allocObject or allocArray
        
        const ivSize = (@sizeOf(T)+objectWidth-1)/objectWidth-1;
        if (extra==0) return self.allocObject(sp,hp,context,classIndex,ivSize);
        return self.allocArray(sp,hp,context,classIndex,ivSize,extra,T2);
    }
    inline fn initAllocation(result: HeapPtr, classIndex: class.ClassIndex, form: Format, size: usize, age: Age, fill: Object) void {
        const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        mem.set(Object,result.asObjectPtr()[1..size+1],fill);
        result.*=header(@intCast(u12,size),form,classIndex,hash,age);
    }
};

pub fn NurseryArena(comptime stack_size: comptime_int) type {
    return extern struct {
        const Self = @This();
        arena: Arena,
        hp: HeaderArray,
        sp: [*]Object,
        heapArea: [stack_size-field_size]Header,
        const field_size = (@sizeOf(Arena)+@sizeOf(HeaderArray)+@sizeOf([*]Object))/@sizeOf(Header);
        comptime {
            if (checkEqual(@sizeOf(Self),stack_size*@sizeOf(Header))) |s|
                @compileError("Modify NurseryArena.field_size to make @sizeOf(NurseryArena) == " ++ s);
        }
        pub fn new() Self {
            return Self {
                .arena = undefined,
                .hp = undefined,
                .sp = undefined,
                .heapArea = undefined,
            };
        }
        pub fn init(self: *Self) void {
            self.arena = Arena{.alloc=alloc,.collect=collect};
            self.hp = @ptrCast(HeaderArray,@alignCast(@alignOf(u64),&self.heapArea));
            self.sp = self.endOfStack();
        }
        pub fn asArena(self: *Self) *Arena {
            return @ptrCast(*Arena,self);
        }
        pub inline fn endOfStack(self: *Self) [*]Object {
            return @intToPtr([*]Object,@ptrToInt(&self.heapArea))+self.heapArea.len;
        }
        pub inline fn getHp(self: *Self) HeaderArray {
            return self.hp;
        }
        fn allocSlow(_: *Arena, _:[*]Object, _:HeaderArray, _:ContextPtr, _: usize, _: usize) AllocReturn {
            return error.HeapFull;
        }
        fn alloc(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:ContextPtr, heapSize: usize, arraySize: usize) AllocReturn {
            const totalSize = heapSize + arraySize;
            const result = @ptrCast(HeapPtr,hp);
            const end = hp + totalSize;
            if (@ptrToInt(sp)<=@ptrToInt(end)) return allocSlow(arena,sp,hp,context,heapSize,arraySize);
            return .{.sp=sp, .hp=end, .context=context, .age=Age.nursery, .allocated=result,};
        }
        fn collect(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:ContextPtr) AllocErrors!void {
            const self = @ptrCast(*Self,arena);
            _ =  self; _ = sp; _ = hp; _ = context;
            @panic("incomplete");
        }
    };
}
// pub fn TeenArena(comptime nursery_size: comptime_int) type {
//     return extern struct {
//     const Self = @This();
//     arena: Arena,
//     free: HeaderArray,
//     heapArea: [size]Header,
//     const size = nursery_size-field_size;
//     const field_size = (@sizeOf(Arena)+@sizeOf(HeaderArray))/@sizeOf(Header);
//     comptime {
//         if (checkEqual(@sizeOf(TeenArena),nursery_size*@sizeOf(Header))) |s|
//             @compileError("Modify TeenArena.heap to make @sizeOf(TeenArena) == " ++ s);
//     }
//     pub fn new() TeenArena {
//         return Self {
//             .arena = Arena{.alloc=alloc,.collect=collect};
//             .free = undefined,
//             .heap = undefined,
//         };
//     }
//     pub fn init(self: *Self, otherTeenHeap: *Self) void {
//         _ = otherTeenHeap;
//         self.free = @ptrCast(HeaderArray,&self.heap);
//     }
// };
test "fubar" {
    const nType = NurseryArena(1024);
    _ = nType;
}
test "object in nursery arena" {
    const nType = NurseryArena(1024);
    var nursery = nType.new();
    nursery.init();
    var hp = nursery.getHp();
    var sp = nursery.endOfStack();
    var context = Context.init();
    const a = nursery.asArena();
    const r = try a.allocObject(sp,hp,&context,42,5);
    const o = r.allocated;
//    try std.testing.expect(!o.isOnStack());
    try std.testing.expect(!o.isForwarded());
    try std.testing.expect(!o.isIndirect());
    try std.testing.expect(!o.isIndexable());
    try std.testing.expect(!o.isRaw());
    const ivs = o.instVars();
    try std.testing.expect(ivs.len==5);
}
