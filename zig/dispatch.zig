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
pub const MethodReturns = union(enum) {
    Normal: Object,
    PrimitiveFailedWithError: Object,
    PrimitiveFailed,
    NonLocal,
    ExceptionSignaled,
    pub fn nonLocal(self : MethodReturns) bool {
        return self>=MethodReturns.NonLocal;
    }
};
pub const DNUState = enum {
    First,
};
const DNUFirst = DNUState.First;
pub const methodT = fn(thread : *Thread, self: Object, selector: Object,dnu: DNUState,classI: ClassIndex) MethodReturns;
const noArgs = ([0]Object{})[0..];
const Context = struct {
    previous: *Context,
    thread: *Thread,
    objects: []Object,
};
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from args if needed thereafter
pub const methodT2 = fn(selector: Object, self: Object, other: Object, context : *Context, args: []Object, dnu: DNUState, classI: ClassIndex) MethodReturns;
pub const SymbolMethod = packed struct{ selector: Object, method: methodT};
const Dispatch = packed struct {
    header: u64, //Header,
    hash: u32,
    super: u32,
    methods: [1]SymbolMethod,
};
const max_classes = class.ReservedNumberOfClasses;
var classDispatch : [max_classes]*Dispatch = undefined;
const u32_phi_inverse=2654435769;
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
fn stage2a(thread : *Thread, self: Object, selector: Object, dnuS:DNUState, ci:ClassIndex) MethodReturns {
    testNormal(thread, self, selector, dnuS, ci);
    @panic("stage2a");
}
fn DispatchMethods(comptime T: type, extractHash: fn(T) u32, maxSize: comptime_int) type {
    return struct{
        const Self = @This();
        fn findTableSize(sm: []const T, extra: ?T,fix: *[12]Fix) !TableStructureResult {
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
                    const rand = tries *%u32_phi_inverse & ~@as(u32,31) | @truncate(u5,@clz(u32,size)+1);
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
                        // stdout.print("table of size {} is {}% efficient with rand={} on try {}\n",.{sm.len,sm.len*100/size,rand,tries}) catch unreachable;
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
                        if (conflicts>12) return error.moreThan12Conflicts;
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
            // stdout.print("table of size {}({}) has {} conflicts ({any})({}) with rand={}\n",.{conflictSize,sm.len,minSizeConflicts,fixup,i,bestConflictRand}) catch unreachable;
            return TableStructureResult{.withConflicts=.{.size=conflictSize+level2,.hash=bestConflictRand,.fix=fixup}};
        }
        const stage2 = [_]methodT{stage2a};
        fn addDispatch(thread: *Thread, theClass: ClassIndex, superClass: ClassIndex, symbolMethods: []const SymbolMethod) void {
            const arena = thread.getArena().getGlobal();
            const dispatchSize = Self.findTableSize(symbolMethods,null);
            const rand = dispatchSize.hash();
            const size = dispatchSize.size();
            const strct = arena.allocStruct(class.Dispatch_I,@sizeOf(Dispatch)+size*@sizeOf(methodT),Dispatch) catch unreachable;
            strct.hash = rand;
            strct.super = superClass;
            const methods=@ptrCast([*]SymbolMethod,&strct.methods);
            for (methods[0..dispatchSize]) |*sm| {
                sm.method=dnu;
            }
            for (symbolMethods) |*sm| {
                const hash = sm.selector.hash *% rand >> @truncate(u5,rand);
                methods[hash] = sm.method;
            }
            switch (dispatchSize) {
                .conflictFree => {},
                .withConflicts => |wc| {
                    var next = wc.size;
                    const fix = wc.fix;
                    var i: u8 = 0;
                    var shifts: u64 = 0;
                    var shift: u6 = 0;
                    while (i<fix.len) : (shift+=1) {
                        switch (fix.prt[i+1]) {
                            2 => {
                                const left = fix.ptr[i+2];
                                const right = fix.ptr[i+3];
                                const xor = symbolMethods[left].selector.hash^symbolMethods[right].selector.hash;
                                const sh = @ctz(u5,xor);
                                shifts |= sh << shift*5;
                                if (((fix.ptr[i+2]>>sh)&1)==0) {
                                    methods[next] = symbolMethods[left].method;
                                    methods[next+1] = symbolMethods[right].method;
                                } else {
                                    methods[next] = symbolMethods[right].method;
                                    methods[next+1] = symbolMethods[left].method;
                                }
                                next += 2;
                                shift += 1;
                            },
                            else => @panic("Not implemented"),
                        }
                        methods[wc.hash]=stage2[shift];
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
    var fix: [12]Fix = undefined;
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
fn id_sm(x:SymbolMethod) u32 {return x.selector.hash;}
const dispatch=DispatchMethods(SymbolMethod,id_sm,2050);

fn testNormal(thread : *Thread, self: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=self;
    return MethodReturns{.Normal=self};
}
fn testNonLocal(thread : *Thread, self: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=self;
    return MethodReturns.NonLocal;
}
fn test1(thread : *Thread, _: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=Object.from(1);
    return MethodReturns{.Normal=Object.from(1)};
}
fn test2(thread : *Thread, _: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=Object.from(2);
    return MethodReturns{.Normal=Object.from(2)};
}
fn test3(thread : *Thread, _: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=Object.from(3);
    return MethodReturns{.Normal=Object.from(3)};
}
fn test4(thread : *Thread, _: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=Object.from(4);
    return MethodReturns{.Normal=Object.from(4)};
}
fn test5(thread : *Thread, _: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=Object.from(5);
    return MethodReturns{.Normal=Object.from(5)};
}
fn test6(thread : *Thread, _: Object, _: Object, _:DNUState, _:ClassIndex) MethodReturns {
    thread.stack()[0]=Object.from(6);
    return MethodReturns{.Normal=Object.from(6)};
}
pub const noMethods = [0]SymbolMethod{};
const symbolMethods1 = [_]SymbolMethod{
    .{.selector=symbols.value,.method=testNormal},
    .{.selector=symbols.self,.method=testNormal},
    .{.selector=symbols.yourself,.method=testNormal},
    .{.selector=symbols.@"cull:",.method=testNormal},
};
const symbolMethods2 = [_]SymbolMethod{
    .{.selector=symbols.value,.method=test1},
    .{.selector=symbols.self,.method=test2},
    .{.selector=symbols.yourself,.method=test3},
    .{.selector=symbols.@"cull:",.method=test4},
    .{.selector=symbols.@"value:",.method=test5}, // additional method forces larger hash
    .{.selector=symbols.@"cull:cull:cull:cull:",.method=test6}, // additional method forces larger hash
};
const symbolMethods3 = [_]SymbolMethod{
    .{.selector=symbols.value,.method=testNormal},
    .{.selector=symbols.self,.method=testNormal},
    .{.selector=symbols.yourself,.method=testNormal},
    .{.selector=symbols.@"cull:",.method=testNormal},
    .{.selector=symbols.@"value:",.method=testNonLocal},
    .{.selector=symbols.@"value:value:",.method=testNormal},
    .{.selector=symbols.@"value:value:value:",.method=testNormal},
    .{.selector=symbols.@"value:value:value:value:",.method=testNormal},
    .{.selector=symbols.@"=",.method=testNormal},
    .{.selector=symbols.size,.method=testNormal},
    .{.selector=symbols.negated,.method=testNormal},
};
test "timing" {
    const stdout = @import("std").io.getStdOut().writer();
    const findTableSize=dispatch.findTableSize;
    var fix: [12]Fix = undefined;
    try stdout.print("methods1: {any}\n",.{(try findTableSize(symbolMethods1[0..],null,&fix))});
    try stdout.print("methods2: {any}\n",.{(try findTableSize(symbolMethods2[0..],null,&fix))});
    try stdout.print("methods3: {any}\n",.{(try findTableSize(symbolMethods3[0..],null,&fix))});

}
test "findTableSize" {
    const expectEqual = @import("std").testing.expectEqual;
    const findTableSize=dispatch.findTableSize;
    var fix: [12]Fix = undefined;
    try expectEqual((try findTableSize(symbolMethods1[0..],null,&fix)).size(),4);
    try expectEqual((try findTableSize(symbolMethods2[0..],null,&fix)).size(),8);
    try expectEqual((try findTableSize(symbolMethods3[0..],null,&fix)).size(),16);
}
pub fn addClass(thread: *Thread, className: Object, instanceMethods: []const SymbolMethod, classMethods: []const SymbolMethod) !void {
    _ = thread;
    _ = className;
    _ = instanceMethods;
    _ = classMethods;
    return error.UnImplemented;
}
pub inline fn call(thread: *Thread, self: Object, selector: Object) MethodReturns {
    const theClass = self.get_class();
    return callClass(thread, theClass, self, selector);
}
pub fn callClass(thread: *Thread, theClass: ClassIndex, self: Object, selector: Object) MethodReturns {
    const disp = classDispatch[theClass];
    const rand = disp.hash;
    const index = selector.hash *% rand >> @truncate(u5,rand);
    const symbolMethodPtr = @ptrCast([*]SymbolMethod,&disp.methods)+index;
    return symbolMethodPtr[0].method(thread,self,selector,DNUFirst,theClass);
}
fn dispatchTableObject(classIndex: ClassIndex) HeapPtr {
    return @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),classDispatch[classIndex]));
}
test "addClass and call" {
    const expectEqual = @import("std").testing.expectEqual;
    var thread = try Thread.initForTest();
    try addClass(&thread,symbols.SmallInteger,symbolMethods1[0..],noMethods[0..]);
    const t42 = Object.from(42);
    thread.push(Object.from(17));
    try expectEqual(call(&thread,t42,symbols.value),MethodReturns{.Normal=Nil});
    try expectEqual(thread.stack()[0],t42);
}
test "lookups of proper methods" {
    const expectEqual = @import("std").testing.expectEqual;
    var thread = try Thread.initForTest();
    try addClass(&thread,symbols.SmallInteger,symbolMethods2[0..],noMethods[0..]);
    const t42 = Object.from(42);
    thread.push(Object.from(17));
    _ = call(&thread,t42,symbols.value);
    try expectEqual(thread.stack()[0],Object.from(1));
    _ = call(&thread,t42,symbols.self);
    try expectEqual(thread.stack()[0],Object.from(2));
    _ = call(&thread,t42,symbols.yourself);
    try expectEqual(thread.stack()[0],Object.from(3));
    _ = call(&thread,t42,symbols.@"cull:");
    try expectEqual(thread.stack()[0],Object.from(4));
    _ = call(&thread,t42,symbols.@"value:");
    try expectEqual(thread.stack()[0],Object.from(5));
    _ = call(&thread,t42,symbols.@"cull:cull:cull:cull:");
    try expectEqual(thread.stack()[0],Object.from(6));
}
fn dnu(thread: *Thread,selector: Object) MethodReturns {
    _ = selector;
    _ = thread;
    unreachable;
}

fn gen_primes(comptime T : type, n_primes: usize) [n_primes]T {
    var p : [n_primes]T = undefined;
    var possible : T = 3;
    var previous : T = 0;
    var i: usize = 0;
    if (n_primes>13) @setEvalBranchQuota(100000);
    next:
        while (true) : (possible += 2) {
            var j: usize = 3;
            while (j*j <= possible) : (j += 2) {
                if (possible%j==0) continue :next;
            }
            if (possible < previous * 13 / 10) continue :next;
            previous = possible;
            p[i] =  possible;
            i += 1;
            if (i>=n_primes) return p;
    }
}
const primes_type = u16;
const prime_values = gen_primes(primes_type,if (builtin.is_test) 15 else 22);
const default_prime = 11959; // max size of dispatch table - must be less than 32767
pub fn next_prime_larger_than(n : primes_type) primes_type {
    var low : usize = 0;
    var high : usize = prime_values.len-1;
    while (low<=high) {
        const mid = (low+high)/2;
        if (mid==0) return prime_values[0];
        if (prime_values[mid]>=n) {
            if (prime_values[mid-1]<n) return prime_values[mid];
            high=mid;
        } else
            low=mid+1;
    }
    return default_prime;
}
test "primes" {
//    const stdout = @import("std").io.getStdOut().writer();
//    try stdout.print("primes: {any}\n",.{prime_values});
    const expectEqual = @import("std").testing.expectEqual;
    try expectEqual(next_prime_larger_than(1),3);
    try expectEqual(next_prime_larger_than(3),3);
    try expectEqual(next_prime_larger_than(6),7);
    try expectEqual(next_prime_larger_than(24),29);
    try expectEqual(next_prime_larger_than(167),167);
    try expectEqual(next_prime_larger_than(224),293);
    if (prime_values.len<22) {
        try expectEqual(next_prime_larger_than(294),default_prime);
    } else
        try expectEqual(next_prime_larger_than(1889),1889);
    try expectEqual(next_prime_larger_than(1890),default_prime);
}
