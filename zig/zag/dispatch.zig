const std = @import("std");
const tailCall: std.builtin.CallModifier = .always_tail;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const Thread = @import("thread.zig").Thread;
const TestExecution = @import("context.zig").TestExecution;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const Hp = heap.HeaderArray;
const builtin = @import("builtin");
const symbol = @import("symbol.zig");
const symbols = symbol.symbols;
const execute = @import("execute.zig");
const Context = execute.Context;
const MethodReturns = execute.MethodReturns;
const ThreadedFn = execute.ThreadedFn;
//const CompiledMethodPtr = execute.CompiledMethodPtr;
const Code = execute.Code;
const CodeContextPtr = execute.CodeContextPtr;
const u32_phi_inverse=@import("utilities.zig").inversePhi(u32);
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const forTest = Dispatch.forTest;
const noArgs = ([0]Object{})[0..];
const Dispatch = extern struct {
    header: heap.Header,
    hash: u32,
    beyond: u32,
    superOrDNU: [2]Code, // could handle DNU separately, but no current reason
    methods: [minHash][*]Code, // this is just the default... normally a larger array
    const Self = @This();
    const minHash = 13; // must be prime
    fn new() Self {
        return .{
            .header = heap.header(@sizeOf(Self)/@sizeOf(Object)-1, heap.Format.objectP, class.Dispatch_I, 42, heap.Age.static),
            .hash = undefined,
            .beyond = undefined,
            .superOrDNU = undefined,
            .methods = undefined,
        };
    }
    inline fn initPrivate(self: *Self, code: [2]Code) void { // should only be used by next three functions or tests
        self.hash = minHash;
        self.beyond = minHash;
        self.superOrDNU = code;
        for (self.methods) |*ptr|
            ptr.* = &self.superOrDNU;
    }
    fn initSuper(self: *Self, superClass: ClassIndex) void {
        self.iniitPrivate(.{Code.prim(&super),Code.uint(superClass)});
    }
    fn initDNU(self: *Self) void {
        self.initPrivate(.{Code.prim(&dnu),Code.uint(0)});
    }
    fn initTest(self: *Self, target: *usize) void {
        self.initPrivate(.{Code.prim(&testIncrement),Code.uint(@ptrToInt(target))});
    }
    pub fn forTest() void {
        var foo = Self.new();
        foo.initDNU();
    }
    fn isDispatch(self: *Self, code: [*]Code) bool {
        const ptr = @ptrToInt(code);
        if (ptr>=@ptrToInt(self) and ptr<=(@ptrToInt(self)+self.hash.length)) return false;
        if (ptr>=@ptrToInt(&super) and ptr<=@ptrToInt(&dnu)) return false;
        if (ptr<4096) return false;
        return true;
    }
    fn isAvailable(self: *Self, code: [*]Code) bool {
        const ptr = @ptrToInt(code);
        if (ptr==@ptrToInt(&self.superOrDNU)) return true;
        return false;
    }
    inline fn lookup(self: *Self, selectorHash: u32) [*]Code {
        return self.lookupAddress(selectorHash).*;
    }
    inline fn lookupAddress(self: *Self, selectorHash: u32) *[*]Code {
        return @ptrCast(*[*]Code,&self.methods[selectorHash*@as(u64,self.hash)>>32]);
    }
    inline fn preHash(selector: Object) u32 {
        return selector.hash32()*%u32_phi_inverse;
    }
    pub fn dispatch(self: *Self, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selector: Object) MethodReturns {
        const hashed = preHash(selector);
        const pc = self.lookup(hashed);
        return @call(tailCall,@ptrCast(*const fn(*Self,[*]Object,Hp,*Thread,CodeContextPtr,Object) MethodReturns,pc[0].prim),.{@ptrCast(*Dispatch,pc+1),sp,hp,thread,context,@bitCast(Object,@as(u64,hashed))});
    }
    fn add(self: *Self, selector: Object, codePtr: [*]Code) !void {
        const hashed = preHash(selector);
        const address = self.lookupAddress(hashed);
        std.debug.print("add selector={} address=0x{x:0>16}\n",.{selector,@ptrToInt(address)});
        const previous = @cmpxchgStrong([*]Code,address,&self.superOrDNU,codePtr,.SeqCst,.SeqCst);
        if (previous==null) return;
        return error.NotAvailable;
    }
    const shifts = .{&shift0,&shift1};
    fn super(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        _ = .{programCounter, sp, hp, thread, context, selectorHash};
        @panic("called super function");
    }
    fn shift0(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        const pc = programCounter[selectorHash&1].codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,hp,thread,context,selectorHash});
    }
    fn shift1(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        const pc = programCounter[(selectorHash>>1)&1].codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,hp,thread,context,selectorHash});
    }
    const primes = .{&prime3,&prime5};
    fn prime3(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        const pc = programCounter[(@as(u64,selectorHash)*3)>>32].codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,hp,thread,context,selectorHash});
    }
    fn prime5(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        const pc = programCounter[(@as(u64,selectorHash)*5)>>32].codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,hp,thread,context,selectorHash});
    }
    fn fail(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        _ = .{programCounter, sp, hp, thread, context, selectorHash};
        if (programCounter[0].uint==0)
            @panic("called fail function");
    }
    // make sure all 2nd-level dispatchers are between shift0 and dnu - so they can be checked by isDispatch
    fn dnu(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        _ = .{programCounter, sp, hp, thread, context, selectorHash};
        @panic("called dnu function");
    }
    fn testIncrement(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
        _ = .{sp, hp, thread, context, selectorHash};
        @intToPtr(*usize,programCounter[0].uint).* += 1;
    }
};
fn testYourself(programCounter: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr, selectorHash: u32) MethodReturns {
    _ = .{sp, hp, thread, context};
    if (selectorHash!=Dispatch.preHash(symbols.yourself)) @panic("hash doesn't match");
    @intToPtr(*usize,programCounter[0].uint).* += 2;
}
test "add methods" {
    var tE = TestExecution.new();
    tE.init();
    var dispatch = Dispatch.new();
    var temp: usize = 0;
    var code1 = [_]Code{Code.prim(&testYourself),Code.uint(@ptrToInt(&temp))};
    dispatch.initTest(&temp);
    try dispatch.add(symbols.yourself,&code1);
    try std.testing.expectEqual(dispatch.add(symbols.yourself,&code1),error.NotAvailable);
    dispatch.dispatch(tE.sp,tE.hp,&tE.thread,&tE.ctxt,symbols.yourself);
    try std.testing.expectEqual(temp,2);
    dispatch.dispatch(tE.sp,tE.hp,&tE.thread,&tE.ctxt,symbols.self);
    try std.testing.expectEqual(temp,3);
    try dispatch.add(symbols.@"~~",&code1);
    std.debug.print("yourself = {}\n",.{Dispatch.preHash(symbols.yourself)%13});
    var n:u24 = 1;
    while (n<40) : (n += 1) {
        std.debug.print("n{} = {}\n",.{n,Dispatch.preHash(symbol.symbol1(n))%13});
    }
}
pub const DispatchPtr = *Dispatch;
const ClassDispatch = extern struct {
    dispatch: ?*Dispatch,
    hash: u32,
    const Self = @This();
    const init:Self =  .{.dispatch = null,.hash=0};
    fn store(self:*Self,value:Self) void {
        @atomicStore(u128, @ptrCast(*u128,self), @bitCast(u128,value), std.builtin.AtomicOrder.Unordered);
    }
};
test "128-bit atomic store" {
    var foo = ClassDispatch.init;
    const bar = ClassDispatch {.dispatch = null,.hash=0};
    foo.store(bar);
    try std.testing.expectEqual(foo,bar);
}
const max_classes = class.ReservedNumberOfClasses;
var classDispatch : [max_classes]ClassDispatch = undefined;
inline fn bumpSize(size:u16) u16 {
    return size*2;
}
inline fn initialSize(size:usize) u16 {
    return @import("utilities.zig").largerPowerOf2(@max(@intCast(u16,size),4));
}
const Fix = struct {index:u16,a:u16,b:u16,c:u16};
const CF = struct{size:u16,hash:u32};
const WC = struct{size:u16,hash:u32,fix:[]Fix};
const TableStructureResult = union(enum) {
    conflictFree: CF,
    withConflicts: WC,
    const Self = TableStructureResult;
    inline fn hash(self: Self) u32 {
        return switch (self) {
            .conflictFree => |cf| cf.hash,
            .withConflicts => |wc| wc.hash,
        };
    }
    inline fn size(self: Self) u16 {
        return switch (self) {
            .conflictFree => |cf| cf.size,
            .withConflicts => |wc| wc.size,
        };
    }
};
fn stage2a(thread : *Thread, self: Object, selector: Object, ci:ClassIndex) MethodReturns {
    //    testNormal(_: Object, self: Object, _: Object, _ : *Context, null, null);
    _ = .{thread,self,selector,ci};
    @panic("stage2a");
}
// fn DispatchMethods(comptime T: type, comptime extractHash: fn(T) u32, comptime maxSize: comptime_int) type {
//     return struct{
//         const Self = @This();
//         fn findTableSize(sm: []const T, extra: ?T,fix: *[15]Fix) !TableStructureResult {
//             var minSizeConflicts: u32 = maxSize;
//             var conflictSize: u16 = 0;
//             var bestConflictRand: u32 = 0;
//             var used : [maxSize]u8 = undefined;
//             var size = initialSize(sm.len);
//             const limitSize = @min(@max(initialSize(sm.len*4),17),maxSize);
//             while (size<limitSize) : (size = bumpSize(size)) {
//                 var minConflicts: u32 = maxSize;
//                 var bestRand: u32 = 0;
//                 var tries: u32 = 1;
//                 while (tries<=65) : (tries += 1) {
//                     const rand = tries *% u32_phi_inverse & ~@as(u32,31) | @truncate(u5,@clz(@as(u32,size))+1);
//                     for (used[0..size]) |*b| b.* = 0;
//                     if (extra) |key| {
//                         const hash = extractHash(key) *% rand >> @truncate(u5,rand);
//                         used[hash] = 1;
//                     }
//                     for (sm) |key| {
//                         const hash = extractHash(key) *% rand >> @truncate(u5,rand);
//                         used[hash] += 1;
//                     }
//                     var conflicts: u32 = 0;
//                     for (used[0..size]) |count| {
//                         if (count>1) conflicts += count;
//                     }
//                     if (conflicts>0) {
//                         if (minConflicts > conflicts) {
//                             minConflicts = conflicts;
//                             bestRand = rand;
//                         }
//                     } else {
//                         std.debug.print("table of size {} is {}% efficient with rand={} on try {}\n",.{sm.len,sm.len*100/size,rand,tries});
//                         return TableStructureResult{.conflictFree=.{.size=size,.hash=rand}};
//                     }
//                 }
//                 if (minSizeConflicts > minConflicts) {
//                     minSizeConflicts = minConflicts;
//                     conflictSize = size;
//                     bestConflictRand = bestRand;
//                 }
//             }
//             for (used[0..conflictSize]) |*b| b.* = 0;
//             for (sm) |key| {
//                 const hash = extractHash(key) *% bestConflictRand >> @truncate(u5,bestConflictRand);
//                 used[hash] += 1;
//                 if (used[hash]>3) return error.moreThan3WayConflict;
//             }
//             var i: u8 = 0;
//             var conflicts: u8=0;
//             var level2: u16 = 1;
//             for (sm) |key,index| {
//                 const hash = extractHash(key) *% bestConflictRand >> @truncate(u5,bestConflictRand);
//                 switch (used[hash]) {
//                     0,1 => {},
//                     2,3 => |s| {
//                         conflicts += s-1;
//                         if (conflicts>15) return error.moreThan15Conflicts;
//                         fix[i]=Fix{.index=@truncate(u16,hash),
//                                    .a=@truncate(u16,index),
//                                    .b=0,
//                                    .c=0,};
//                         used[hash] = i+16;
//                         i += 1;
//                         level2 += (s+1)&0xFE;
//                     },
//                     else => |s| {
//                         switch (s>>4) {
//                             1 => fix[s&15].b = @truncate(u16,index),
//                             else => fix[s&15].c = @truncate(u16,index),
//                         }
//                         used[hash] += 16;
//                     },
//                 }
//             }
//             const fixup = fix[0..i];
//             std.debug.print("table of size {}({}) has {} conflicts ({any})({}) with rand=0x{x:8>0}\n",.{conflictSize,sm.len,minSizeConflicts,fixup,i,bestConflictRand});
//             return TableStructureResult{.withConflicts=.{.size=conflictSize+level2,.hash=bestConflictRand,.fix=fixup}};
//         }
//         fn addDispatch(_: *Thread, theClass: ClassIndex, superClass: ClassIndex, symbolMethods: []const CompiledMethodPtr) void {
//             const arena = &@import("arenas.zig").globalArena;//.asArena();
//             var fixup: [15]Fix = undefined;
//             const dispatchSize = Self.findTableSize(symbolMethods,null,&fixup) catch @panic("dispatch conflicts");
//             const rand = dispatchSize.hash();
//             const size = 0;//dispatchSize.size();
//             const strct = arena.allocStruct(class.Dispatch_I,@sizeOf(Dispatch)+size*@sizeOf(CompiledMethodPtr),Dispatch,@bitCast(Object,@ptrToInt(dnu)),8) catch unreachable;
//             strct.hash = rand;
//             strct.super = superClass;
//             const methods=@ptrCast([*]CompiledMethodPtr,@alignCast(@alignOf([*]CompiledMethodPtr),&strct.methods));
//             for (symbolMethods) |*sm| {
//                 const hash = sm.selector.hash *% rand >> @truncate(u5,rand);
//                 methods[hash] = sm.method;
//             }
//             switch (dispatchSize) {
//                 .conflictFree => {},
//                 .withConflicts => |wc| {
//                     //var next = wc.size;
//                     const fix = wc.fix;
//                     var i: u8 = 0;
//                     //var shifts: u64 = 0;
//                     var shift: u6 = 0;
//                     while (i<fix.len) : (shift+=1) {
//                         switch (fix.ptr[i+1]) {
//                         //     2 => {
//                         //         const left = fix.ptr[i+2];
//                         //         const right = fix.ptr[i+3];
//                         //         const xor = symbolMethods[left].selector.hash^symbolMethods[right].selector.hash;
//                         //         const sh = @ctz(u5,xor);
//                         //         shifts |= sh << shift45;
//                         //         if (((fix.ptr[i+2]>>sh)&1)==0) {
//                         //             methods[next] = symbolMethods[left].method;
//                         //             methods[next+1] = symbolMethods[right].method;
//                         //         } else {
//                         //             methods[next] = symbolMethods[right].method;
//                         //             methods[next+1] = symbolMethods[left].method;
//                         //         }
//                         //         next += 2;
//                         //         shift += 1;
//                         //     },
//                             else => @panic("Not implemented"),
//                         }
//                         //methods[wc.hash]=stage2[shift];
//                     }
//                 },
//             }
//             classDispatch[theClass]=strct;
//         }
//     };
// }

// fn id_u32(x:u32) u32 {return x;}
// test "tablesize" {
//     const e1 = [_]u32{6, 518, 38, 2};
//     const e2 = [_]u32{6, 518, 38, 2, 7, 5};
//     const e3 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47};
//     const e4 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47,
//                       4518, 438, 49, 410, 442, 446, 447};
//     const e5 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47,
//                       4518, 438, 49, 410, 442, 446, 447,
//                       36, 3518, 338, 32, 37, 39, 310, 342, 346, 347,
//                       26, 2518, 238, 22, 27, 28, 29, 210, 242, 246, 247,
//                       16, 1518, 138, 12, 17, 18, 19, 110, 142, 146, 147};
//     const stdout = std.io.getStdOut().writer();
//     var fix: [15]Fix = undefined;
//     {
//         const findTableSize2=DispatchMethods(u32,id_u32,35).findTableSize;
//         try stdout.print("\n",.{});
//         try stdout.print("e1: {any}\n",.{try findTableSize2(e1[0..],null,&fix)});
//         try stdout.print("e2: {any}\n",.{try findTableSize2(e2[0..],null,&fix)});
//         try stdout.print("e3: {any}\n",.{try findTableSize2(e3[0..],null,&fix)});
//         try stdout.print("e4: {any}\n",.{try findTableSize2(e4[0..],null,&fix)});
//     }
//     {
//         const findTableSize2=DispatchMethods(u32,id_u32,128).findTableSize;
//         try stdout.print("e5: {any}\n",.{try findTableSize2(e5[0..],null,&fix)});
//     }
//     {
//         const findTableSize2=DispatchMethods(u32,id_u32,17).findTableSize;
//         var e6: [15]u32 = undefined;
//         for (e6) |*v,i| v.*=@truncate(u32,i);
//         try stdout.print("e6: {any}\n",.{try findTableSize2(e6[0..],null,&fix)});
//     }
// }
// fn id_cm(x:CompiledMethodPtr) u32 {return x.selector.hash32();}
// const dispatch=DispatchMethods(CompiledMethodPtr,id_cm,2050);

// // test "timing" {
// //     const stdout = @import("std").io.getStdOut().writer();
// //     const findTableSize=dispatch.findTableSize;
// //     var fix: [12]Fix = undefined;
// //     try stdout.print("methods1: {any}\n",.{(try findTableSize(symbolMethods1[0..],null,&fix))});
// //     try stdout.print("methods2: {any}\n",.{(try findTableSize(symbolMethods2[0..],null,&fix))});
// //     try stdout.print("methods3: {any}\n",.{(try findTableSize(symbolMethods3[0..],null,&fix))});

// // }
// // test "findTableSize" {
// //     const expectEqual = @import("std").testing.expectEqual;
// //     const findTableSize=dispatch.findTableSize;
// //     var fix: [12]Fix = undefined;
// //     try expectEqual((try findTableSize(symbolMethods1[0..],null,&fix)).size(),4);
// //     try expectEqual((try findTableSize(symbolMethods2[0..],null,&fix)).size(),8);
// //     try expectEqual((try findTableSize(symbolMethods3[0..],null,&fix)).size(),16);
// // }
// pub fn addClass(thread: *Thread, className: Object, instanceMethods: []const CompiledMethodPtr, classMethods: []const CompiledMethodPtr) !void {
//     const theClass_I = 42;_=className;//class.getClassIndex(className);
//     const superClass = 0;
//     const theMetaclass_I = 0;
//     dispatch.addDispatch(thread, theClass_I, superClass, instanceMethods);
//     dispatch.addDispatch(thread, theMetaclass_I, superClass, classMethods);
//     return error.UnImplemented;
// }
// pub inline fn call(selector: Object, self: Object, other: Object, cp: *Context) MethodReturns {
//     const theClass = self.get_class();
//     return callClass(selector, self, other, cp, theClass);
// }
// pub inline fn callClass(selector: Object, self: Object, other: Object, cp: *Context, theClass: ClassIndex) MethodReturns {
//     @setRuntimeSafety(false);
//     const disp = classDispatch[theClass];
//     const rand = disp.hash;
//     const index = selector.hash *% rand >> @truncate(u5,rand);
//     return disp.methods[index](selector, self, other, cp, disp, null);
// }
// fn dispatchTableObject(classIndex: ClassIndex) HeapPtr {
//     return @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),classDispatch[classIndex]));
// }
// test "addClass and call" {
// //    const expectEqual = @import("std").testing.expectEqual;
//     var thread = Thread.new();
//     thread.init();
// //    _ = try symbol.init(&thread,250,"");
//     var noMethods = [0]CompiledMethodPtr{};
//     try class.init();
//     try addClass(&thread,symbols.SmallInteger,&noMethods,&noMethods);
// //     const t42 = Object.from(42);
// //     try expectEqual(t42.send(symbols.value,Nil,undefined),MethodReturns{.Normal=Nil});
// }
// // test "lookups of proper methods" {
// //     const expectEqual = @import("std").testing.expectEqual;
// //     var thread = try Thread.initForTest(null);
// //     _ = try symbol.init(&thread,250,"");
// //     try class.init(&thread);
// //     try addClass(&thread,symbols.SmallInteger,symbolMethods2[0..],noMethods);
// //     const t42 = Object.from(42);
// // //    thread.push(Object.from(17));
// //     try expectEqual(t42.send(symbols.value,Nil,undefined),MethodReturns{.Normal=Object.from(1)});
// //     try expectEqual(t42.send(symbols.self,Nil,undefined),MethodReturns{.Normal=Object.from(2)});
// //     try expectEqual(t42.send(symbols.yourself,Nil,undefined),MethodReturns{.Normal=Object.from(3)});
// //     try expectEqual(t42.send(symbols.@"cull:",Nil,undefined),MethodReturns{.Normal=Object.from(4)});
// //     try expectEqual(t42.send(symbols.@"value:",Nil,undefined),MethodReturns{.Normal=Object.from(5)});
// //     try expectEqual(t42.send(symbols.@"cull:cull:cull:cull:",Nil,undefined),MethodReturns{.Normal=Object.from(6)});
// // }
