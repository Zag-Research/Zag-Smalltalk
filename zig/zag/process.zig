const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const SeqCst = std.builtin.AtomicOrder.SeqCst;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const ClassIndex = object.ClassIndex;
const checkEqual = @import("utilities.zig").checkEqual;
//const dispatch = @import("dispatch.zig");
const heap = @import("heap.zig");
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectArray = heap.HeapObjectArray;
const HeapObject = heap.HeapObject;
const footer = heap.footer;
const Age = heap.Age;
const Format = heap.Format;
const allocationInfo = Format.allocationInfo;
const AllocErrors = heap.AllocErrors;
const ContextPtr = *@import("context.zig").Context;
const tailCall: std.builtin.CallModifier = .always_tail;
const Code = @import("execute.zig").Code;
const CodeContextPtr = @import("execute.zig").CodeContextPtr;
pub const AllocResult = struct {
    sp: [*]Object,
    hp: HeapObjectArray,
    context: ContextPtr,
    age: Age,
    allocated: HeapObjectPtr,
};
pub const AllocReturn = AllocErrors!AllocResult;

//test "force dispatch load" {
//    dispatch.forTest();
//}
const process_total_size = 64*1024; // must be more than HeapObject.maxLength*8 so externally allocated
pub const Process = extern struct {
    stack: [stack_size] Object,
    nursery0: [nursery_size] Object,
    nursery1: [nursery_size] Object,
    next: ?*Self,
    id: u64,
    debugFn: ?ThreadedFn,
    sp: [*]Object,
    currHeap: HeapObjectArray,
    currHp: HeapObjectArray,
    currEnd: HeapObjectArray,
    otherHeap: HeapObjectArray,
    trapContextNumber: u64,
    const Self = @This();
    const headerSize = @sizeOf(?*Self)+@sizeOf(u64)+@sizeOf(?ThreadedFn)+@sizeOf([*]Object)+@sizeOf(HeapObjectArray)+@sizeOf(HeapObjectArray)+@sizeOf(HeapObjectArray)+@sizeOf(HeapObjectArray);
    const ThreadedFn = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) [*]Object;
    const processAvail = (process_total_size-headerSize)/@sizeOf(Object);
    const stack_size = processAvail/9;
    const nursery_size = (processAvail-stack_size)/2;
    var allProcesss: ?*Self = null;
    pub fn new() Self {
        return undefined;
    }
    pub fn init(self: *Self) void {
//        @compileLog("stack_size",stack_size);
//        @compileLog("processAvail",processAvail);
//        @compileLog("nursery_size",nursery_size);
        const h = @ptrCast(HeapObjectArray,&self.stack[0]);
        const stack_end = h+stack_size;
        const at = allProcesss;
        self.sp = @ptrCast([*]Object,stack_end);
        self.currHeap = stack_end;
        self.currHp = stack_end;
        self.currEnd = h+nursery_size; // leaving enough space for full nursery copy
        self.otherHeap = stack_end+nursery_size;
        while (true) {
            self.next = at;
            self.id = if (at) |p| p.id+1 else 1;
            if (@cmpxchgWeak(?*Self,&allProcesss,self.next,self,SeqCst,SeqCst)==null) break;
        }
        self.trapContextNumber = 0;
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
    pub inline fn noCheck(self: *const Self) *Self {
        return @intToPtr(*Self,@ptrToInt(self) & ~@as(usize,checkMax));
    }
    pub inline fn debugger(self: *Self) ?ThreadedFn {
        return self.debugFn;
    }
    inline fn ptr(self: *const Self) *Self {
        return @intToPtr(*Self,@ptrToInt(self.noCheck()) // + @sizeOf(heap.HeapObject)
                         );
    }
    pub fn deinit(self : *Self) void {
        self.ptr().* = undefined;
    }
    pub inline fn endOfStack(self: *const Self) [*]Object {
        return @ptrCast([*]Object,&self.ptr().stack[0])+stack_size;
    }
    pub inline fn getStack(self: *const Self, sp: [*]Object) []Object {
        return sp[0..(@ptrToInt(self.endOfStack())-@ptrToInt(sp))/@sizeOf(Object)];
    }
    pub inline fn allocStack(self: *Self, sp: [*]Object, words: u64, contextMutable: *ContextPtr) [*]Object {
        const newSp = sp - words;
        if (@ptrToInt(newSp)>@ptrToInt(self)) return newSp;
        return self.allocStack_(sp,words,contextMutable);
    }
    fn allocStack_(self: *Self, sp: [*]Object, words: u64, contextMutable: *ContextPtr) [*]Object {
        const newSp = sp - words;
        if (@ptrToInt(newSp)>@ptrToInt(self)) return newSp;
        _ = contextMutable;
        @panic("move stack and cp");
    }
    //allocationInfo(iVars: u12, indexed: ?usize, eSize: ?usize, makeWeak: bool)
    //fillFooters(self: Self, theHeapObject: HeapObjectPtr, classIndex: u16, age: Age, nElements: usize, elementSize: usize)
    pub fn alloc(self: *Self, sp: [*]Object, context: ContextPtr, classIndex: u16, iVars: u12, indexed: ?usize, elementSize: usize, makeWeak: bool) heap.AllocReturn {
        const aI = allocationInfo(iVars,indexed,elementSize,makeWeak);
        if (aI.objectSize(nursery_size/4)) |size| {
            const result = self.currHp+size;
            const newHp = result+1;
            if (newHp<self.currEnd) {
                self.currHp = newHp;
                aI.fillFooters(result,classIndex,.nursery,indexed orelse 0,elementSize);
                return .{
                    .sp = sp,
                    .context = context,
                    .age = .nursery,
                    .allocated = @ptrCast(heap.HeapObjectPtr,result),
                };
            }
        } else |_| {
            //const size = aI.objectSize(HeapObject.maxLength) catch unreachable;
            unreachable;
        }
        @panic("can't alloc without collect");
    }
};
test "check flag" {
    const testing = std.testing;
    var process = Process.new();
    var thr = &process;
    thr.init();
    try testing.expect(!thr.needsCheck());
    const origEOS = thr.endOfStack();
    thr = thr.maxCheck();
    try testing.expect(!thr.needsCheck());
    var count = Process.checkMax-1;
    while (count>1) : (count -= 1) {
        thr = thr.decCheck();
    }
    try testing.expect(!thr.needsCheck());
    try testing.expectEqual(thr.endOfStack(),origEOS);
    thr = thr.decCheck();
    try testing.expect(thr.needsCheck());
}

// pub const ArenaX = extern struct {
//     const Self = @This();

//     alloc: *const fn (*Self,[*]Object,HeapObjectArray,ContextPtr,usize,usize) AllocErrors!AllocResult,
//     collect: *const fn (*Self,[*]Object,HeapObjectArray,ContextPtr) AllocErrors!void,

//     pub inline fn allocObject(self:*Self, sp:[*]Object, hp:HeapObjectArray, context:ContextPtr, classIndex:ClassIndex, ivSize:usize) AllocReturn {
//         var result = try self.alloc(self,sp,hp,context,ivSize+1,0);
//         initAllocation(result.allocated,classIndex, Format.objectNP, ivSize, result.age, Nil);
//         return result;
//     }
//     pub fn allocArray(self:*Self, sp:[*]Object, hp:HeapObjectArray, context:ContextPtr, classIndex:ClassIndex, ivSize:usize, arraySize:usize, comptime T: type) AllocReturn {
//         if (arraySize==0) return self.allocObject(sp,hp,context,classIndex,ivSize);
//         const noIVs = ivSize==0;
//         var form = (if (noIVs) Format.none else Format.objectNP).raw(T,arraySize);
//         const width = @sizeOf(T);
//         const aSize = (arraySize*width+@sizeOf(Object)-width)/@sizeOf(Object);
//         const fill = if (T==Object) Nil else object.ZERO;
//         if (noIVs) {
//             if (aSize<HeapObject.maxLength) {
//                 var result = try self.alloc(self,sp,hp,context,aSize+1,0);
//                 initAllocation(result.allocated,classIndex, form, aSize, result.age, fill);
//                 return result;
//             }
//         }
//         var result = try self.alloc(self,sp,hp,context,ivSize+3,aSize);
//         const offs = @ptrCast([*]u64,result.allocated)+ivSize+1;
//         mem.set(Object,@intToPtr([*]Object,offs[1])[0..aSize],fill);
//         offs[0] = arraySize;
//         initAllocation(result.allocated,classIndex, form.setObject(), ivSize, result.age, Nil);
//         return result;
//     }
//     inline fn allocStruct(self: *Self, sp:[*]Object, hp:HeapObjectArray, context:ContextPtr, classIndex: ClassIndex, comptime T: type, extra: usize, comptime T2: type) AllocReturn {

//         // should call allocObject or allocArray
        
//         const ivSize = (@sizeOf(T)+@sizeOf(Object)-1)/@sizeOf(Object)-1;
//         if (extra==0) return self.allocObject(sp,hp,context,classIndex,ivSize);
//         return self.allocArray(sp,hp,context,classIndex,ivSize,extra,T2);
//     }
//     inline fn initAllocation(result: HeapObjectPtr, classIndex: ClassIndex, form: Format, size: usize, age: Age, fill: Object) void {
//         const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
//         mem.set(Object,result.asObjectPtr()[1..size+1],fill);
//         result.*=footer(@intCast(u12,size),form,classIndex,hash,age);
//     }
// };
