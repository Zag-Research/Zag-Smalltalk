const std = @import("std");
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const Thread = @import("thread.zig").Thread;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const builtin = @import("builtin");
const symbol = @import("symbol.zig");
const symbols = symbol.symbols;
const execute = @import("execute.zig");
const Context = execute.Context;
const MethodReturns = execute.MethodReturns;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const u32_phi_inverse=@import("utilities.zig").inversePhi(u32);
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter
const noArgs = ([0]Object{})[0..];
pub const SymbolMethod = packed struct{ selector: Object, method: CompiledMethodPtr};
const Dispatch = packed struct {
    header: u64, //Header,
    hash: u32,
    super: u32,
    methods: [1]CompiledMethodPtr, // normally this is many elements, overwriting the remaining fields
    altHash: u64, // but when hash=0, methods contains a single constraint fn that uses altHash to rehash and
    altMethods: [1]CompiledMethodPtr, // altMethods as the many elements dispatch table if the constraints are met
};
pub const DispatchPtr = *Dispatch;
const max_classes = class.ReservedNumberOfClasses;
var classDispatch : [max_classes]*Dispatch = undefined;
inline fn bumpSize(size:u16) u16 {
    return size*2;
}
inline fn initialSize(size:usize) u16 {
    var n = @maximum(@intCast(u16,size),4);
    n -= 1;
    n |= n>>8;
    n |= n>>4;
    n |= n>>2;
    n |= n>>1;
    return n+1;
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
fn DispatchMethods(comptime T: type, extractHash: fn(T) u32, maxSize: comptime_int) type {
    return struct{
        const Self = @This();
        fn findTableSize(sm: []const T, extra: ?T,fix: *[15]Fix) !TableStructureResult {
            // const stdout = @import("std").io.getStdOut().writer();
            var minSizeConflicts: u32 = maxSize;
            var conflictSize: u16 = 0;
            var bestConflictRand: u32 = 0;
            var used : [maxSize]u8 = undefined;
            var size = initialSize(sm.len);
            const limitSize = @minimum(@maximum(initialSize(sm.len*4),17),maxSize);
            while (size<limitSize) : (size = bumpSize(size)) {
                var minConflicts: u32 = maxSize;
                var bestRand: u32 = 0;
                var tries: u32 = 1;
                while (tries<=65) : (tries += 1) {
                    const rand = tries *% u32_phi_inverse & ~@as(u32,31) | @truncate(u5,@clz(u32,size)+1);
                    for (used[0..size]) |*b| b.* = 0;
                    if (extra) |key| {
                        const hash = extractHash(key) *% rand >> @truncate(u5,rand);
                        used[hash] = 1;
                    }
                    for (sm) |key| {
                        const hash = extractHash(key) *% rand >> @truncate(u5,rand);
                        used[hash] += 1;
                    }
                    var conflicts: u32 = 0;
                    for (used[0..size]) |count| {
                        if (count>1) conflicts += count;
                    }
                    if (conflicts>0) {
                        if (minConflicts > conflicts) {
                            minConflicts = conflicts;
                            bestRand = rand;
                        }
                    } else {
                        std.debug.print("table of size {} is {}% efficient with rand={} on try {}\n",.{sm.len,sm.len*100/size,rand,tries});
                        return TableStructureResult{.conflictFree=.{.size=size,.hash=rand}};
                    }
                }
                if (minSizeConflicts > minConflicts) {
                    minSizeConflicts = minConflicts;
                    conflictSize = size;
                    bestConflictRand = bestRand;
                }
            }
            for (used[0..conflictSize]) |*b| b.* = 0;
            for (sm) |key| {
                const hash = extractHash(key) *% bestConflictRand >> @truncate(u5,bestConflictRand);
                used[hash] += 1;
                if (used[hash]>3) return error.moreThan3WayConflict;
            }
            var i: u8 = 0;
            var conflicts: u8=0;
            var level2: u16 = 1;
            for (sm) |key,index| {
                const hash = extractHash(key) *% bestConflictRand >> @truncate(u5,bestConflictRand);
                switch (used[hash]) {
                    0,1 => {},
                    2,3 => |s| {
                        conflicts += s-1;
                        if (conflicts>15) return error.moreThan15Conflicts;
                        fix[i]=Fix{.index=@truncate(u16,hash),
                                   .a=@truncate(u16,index),
                                   .b=0,
                                   .c=0,};
                        used[hash] = i+16;
                        i += 1;
                        level2 += (s+1)&0xFE;
                    },
                    else => |s| {
                        switch (s>>4) {
                            1 => fix[s&15].b = @truncate(u16,index),
                            else => fix[s&15].c = @truncate(u16,index),
                        }
                        used[hash] += 16;
                    },
                }
            }
            const fixup = fix[0..i];
            std.debug.print("table of size {}({}) has {} conflicts ({any})({}) with rand={}\n",.{conflictSize,sm.len,minSizeConflicts,fixup,i,bestConflictRand});
            return TableStructureResult{.withConflicts=.{.size=conflictSize+level2,.hash=bestConflictRand,.fix=fixup}};
        }
        fn addDispatch(thread: *Thread, theClass: ClassIndex, superClass: ClassIndex, symbolMethods: []const SymbolMethod) void {
            const arena = thread.getArena().getGlobal();
            var fixup: [15]Fix = undefined;
            const dispatchSize = Self.findTableSize(symbolMethods,null,&fixup) catch @panic("dispatch conflicts");
            const rand = dispatchSize.hash();
            const size = dispatchSize.size();
            const strct = arena.allocStruct(class.Dispatch_I,@sizeOf(Dispatch)+size*@sizeOf(CompiledMethodPtr),Dispatch,@bitCast(Object,@ptrToInt(dnu)),8) catch unreachable;
            strct.hash = rand;
            strct.super = superClass;
            const methods=@ptrCast([*]CompiledMethodPtr,@alignCast(@alignOf([*]CompiledMethodPtr),&strct.methods));
            for (symbolMethods) |*sm| {
                const hash = sm.selector.hash *% rand >> @truncate(u5,rand);
                methods[hash] = sm.method;
            }
            switch (dispatchSize) {
                .conflictFree => {},
                .withConflicts => |wc| {
                    //var next = wc.size;
                    const fix = wc.fix;
                    var i: u8 = 0;
                    //var shifts: u64 = 0;
                    var shift: u6 = 0;
                    while (i<fix.len) : (shift+=1) {
                        switch (fix.ptr[i+1]) {
                        //     2 => {
                        //         const left = fix.ptr[i+2];
                        //         const right = fix.ptr[i+3];
                        //         const xor = symbolMethods[left].selector.hash^symbolMethods[right].selector.hash;
                        //         const sh = @ctz(u5,xor);
                        //         shifts |= sh << shift45;
                        //         if (((fix.ptr[i+2]>>sh)&1)==0) {
                        //             methods[next] = symbolMethods[left].method;
                        //             methods[next+1] = symbolMethods[right].method;
                        //         } else {
                        //             methods[next] = symbolMethods[right].method;
                        //             methods[next+1] = symbolMethods[left].method;
                        //         }
                        //         next += 2;
                        //         shift += 1;
                        //     },
                            else => @panic("Not implemented"),
                        }
                        //methods[wc.hash]=stage2[shift];
                    }
                },
            }
            classDispatch[theClass]=strct;
        }
    };
}

fn id_u32(x:u32) u32 {return x;}
test "tablesize" {
    const e1 = [_]u32{6, 518, 38, 2};
    const e2 = [_]u32{6, 518, 38, 2, 7, 5};
    const e3 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47};
    const e4 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47,
                      4518, 438, 49, 410, 442, 446, 447};
    const e5 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47,
                      4518, 438, 49, 410, 442, 446, 447,
                      36, 3518, 338, 32, 37, 39, 310, 342, 346, 347,
                      26, 2518, 238, 22, 27, 28, 29, 210, 242, 246, 247,
                      16, 1518, 138, 12, 17, 18, 19, 110, 142, 146, 147};
    const stdout = std.io.getStdOut().writer();
    var fix: [15]Fix = undefined;
    {
        const findTableSize2=DispatchMethods(u32,id_u32,35).findTableSize;
        try stdout.print("\n",.{});
        try stdout.print("e1: {any}\n",.{try findTableSize2(e1[0..],null,&fix)});
        try stdout.print("e2: {any}\n",.{try findTableSize2(e2[0..],null,&fix)});
        try stdout.print("e3: {any}\n",.{try findTableSize2(e3[0..],null,&fix)});
        try stdout.print("e4: {any}\n",.{try findTableSize2(e4[0..],null,&fix)});
    }
    {
        const findTableSize2=DispatchMethods(u32,id_u32,128).findTableSize;
        try stdout.print("e5: {any}\n",.{try findTableSize2(e5[0..],null,&fix)});
    }
    {
        const findTableSize2=DispatchMethods(u32,id_u32,17).findTableSize;
        var e6: [15]u32 = undefined;
        for (e6) |*v,i| v.*=@truncate(u32,i);
        try stdout.print("e6: {any}\n",.{try findTableSize2(e6[0..],null,&fix)});
    }
}
fn id_cm(x:CompiledMethodPtr) u32 {return x.name.hash32();}
const dispatch=DispatchMethods(CompiledMethodPtr,id_cm,2050);

// test "timing" {
//     const stdout = @import("std").io.getStdOut().writer();
//     const findTableSize=dispatch.findTableSize;
//     var fix: [12]Fix = undefined;
//     try stdout.print("methods1: {any}\n",.{(try findTableSize(symbolMethods1[0..],null,&fix))});
//     try stdout.print("methods2: {any}\n",.{(try findTableSize(symbolMethods2[0..],null,&fix))});
//     try stdout.print("methods3: {any}\n",.{(try findTableSize(symbolMethods3[0..],null,&fix))});

// }
// test "findTableSize" {
//     const expectEqual = @import("std").testing.expectEqual;
//     const findTableSize=dispatch.findTableSize;
//     var fix: [12]Fix = undefined;
//     try expectEqual((try findTableSize(symbolMethods1[0..],null,&fix)).size(),4);
//     try expectEqual((try findTableSize(symbolMethods2[0..],null,&fix)).size(),8);
//     try expectEqual((try findTableSize(symbolMethods3[0..],null,&fix)).size(),16);
// }
pub fn addClass(thread: *Thread, className: Object, instanceMethods: []const SymbolMethod, classMethods: []const SymbolMethod) !void {
    const theClass_I = class.getClassIndex(className);
    const superClass = 0;
    const theMetaclass_I = 0;
    dispatch.addDispatch(thread, theClass_I, superClass, instanceMethods);
    dispatch.addDispatch(thread, theMetaclass_I, superClass, classMethods);
    return error.UnImplemented;
}
pub inline fn call(selector: Object, self: Object, other: Object, cp: *Context) MethodReturns {
    const theClass = self.get_class();
    return callClass(selector, self, other, cp, theClass);
}
pub inline fn callClass(selector: Object, self: Object, other: Object, cp: *Context, theClass: ClassIndex) MethodReturns {
    @setRuntimeSafety(false);
    const disp = classDispatch[theClass];
    const rand = disp.hash;
    const index = selector.hash *% rand >> @truncate(u5,rand);
    return disp.methods[index](selector, self, other, cp, disp, null);
}
fn dispatchTableObject(classIndex: ClassIndex) HeapPtr {
    return @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),classDispatch[classIndex]));
}
// test "addClass and call" {
//     const expectEqual = @import("std").testing.expectEqual;
//     var thread = try Thread.initForTest(null);
//     _ = try symbol.init(&thread,250,"");
//     try class.init(&thread);
//     try addClass(&thread,symbols.SmallInteger,symbolMethods1[0..],noMethods);
//     const t42 = Object.from(42);
//     try expectEqual(t42.send(symbols.value,Nil,undefined),MethodReturns{.Normal=Nil});
// }
// test "lookups of proper methods" {
//     const expectEqual = @import("std").testing.expectEqual;
//     var thread = try Thread.initForTest(null);
//     _ = try symbol.init(&thread,250,"");
//     try class.init(&thread);
//     try addClass(&thread,symbols.SmallInteger,symbolMethods2[0..],noMethods);
//     const t42 = Object.from(42);
// //    thread.push(Object.from(17));
//     try expectEqual(t42.send(symbols.value,Nil,undefined),MethodReturns{.Normal=Object.from(1)});
//     try expectEqual(t42.send(symbols.self,Nil,undefined),MethodReturns{.Normal=Object.from(2)});
//     try expectEqual(t42.send(symbols.yourself,Nil,undefined),MethodReturns{.Normal=Object.from(3)});
//     try expectEqual(t42.send(symbols.@"cull:",Nil,undefined),MethodReturns{.Normal=Object.from(4)});
//     try expectEqual(t42.send(symbols.@"value:",Nil,undefined),MethodReturns{.Normal=Object.from(5)});
//     try expectEqual(t42.send(symbols.@"cull:cull:cull:cull:",Nil,undefined),MethodReturns{.Normal=Object.from(6)});
// }
pub fn dnu(selector: Object, self: Object, other: Object, callingContext : *Context, disp: DispatchPtr, expectedSelector: Object, dnuOption: DNUOption) MethodReturns {
    _ = selector;
    _ = self;
    _ = other;
    _ = callingContext;
    _ = disp;
    _ = expectedSelector;
    _ = dnuOption;
    @panic("dnu");
}
const DNU = struct{};
pub const DNUOption = ?*DNU;
