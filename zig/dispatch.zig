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

pub const MethodReturns = enum {
    Normal,
    PrimitiveFailed,
    NonLocal,
    ExceptionSignaled,
    pub fn nonLocal(self : MethodReturns) bool {
        return self>=MethodReturns.NonLocal;
    }
};
pub const methodT = fn(thread : *Thread, self: Object) MethodReturns;
pub const SymbolMethod = packed struct{ selector: Object, method: methodT};
const extraSlots = 3;
const Dispatch = packed struct {
    header: u64, //Header,
    mod: u32,
    super: u32,
    methods: [extraSlots]SymbolMethod,
};
const max_classes = class.ReservedNumberOfClasses;
var classDispatch : [max_classes]*Dispatch = undefined;
inline fn randomHash(selector: Object, size: u32) u32 {
    const random = @truncate(u32,@bitCast(u64,selector))*%0xa1fdc7a3;
    return random%size;
}
fn findTableSize(sm: []const SymbolMethod) u16 {
    var used : [default_prime+extraSlots]bool = undefined;
    var size = @intCast(u16,sm.len);
    outer: while (size<default_prime) : (size += 1) {
        size = next_prime_larger_than(size);
        for (used[0..size+1]) |*b| b.* = false;
        for (used[size+1..size+extraSlots]) |*b| b.* = true;
        for (sm) |aSymbolMethod| {
            var hash = randomHash(aSymbolMethod.selector,size);
            if (used[hash]) {
                hash += 1;
                if (used[hash]) {
                    if (extraSlots>2) hash += 1;
                    if (used[hash]) {
                        continue :outer;
            }}}
            used[hash] = true;
        }
        return size;
    }
    unreachable;
}

fn testNormal(thread : *Thread, self: Object) MethodReturns {
    thread.stack()[0]=self;
    return MethodReturns.Normal;
}
fn testNonLocal(thread : *Thread, self: Object) MethodReturns {
    thread.stack()[0]=self;
    return MethodReturns.NonLocal;
}
fn test1(thread : *Thread, _: Object) MethodReturns {
    thread.stack()[0]=Object.from(1);
    return MethodReturns.Normal;
}
fn test2(thread : *Thread, _: Object) MethodReturns {
    thread.stack()[0]=Object.from(2);
    return MethodReturns.Normal;
}
fn test3(thread : *Thread, _: Object) MethodReturns {
    thread.stack()[0]=Object.from(3);
    return MethodReturns.Normal;
}
fn test4(thread : *Thread, _: Object) MethodReturns {
    thread.stack()[0]=Object.from(4);
    return MethodReturns.Normal;
}
fn test5(thread : *Thread, _: Object) MethodReturns {
    thread.stack()[0]=Object.from(5);
    return MethodReturns.Normal;
}
fn test6(thread : *Thread, _: Object) MethodReturns {
    thread.stack()[0]=Object.from(6);
    return MethodReturns.Normal;
}
const noMethods = [0]SymbolMethod{};
const symbolMethods1 = [_]SymbolMethod{
    .{.selector=symbol.value,.method=testNormal},
    .{.selector=symbol.self,.method=testNormal},
    .{.selector=symbol.yourself,.method=testNormal},
    .{.selector=symbol.cull_,.method=testNormal},
};
const symbolMethods2 = [_]SymbolMethod{
    .{.selector=symbol.value,.method=test1},
    .{.selector=symbol.self,.method=test2},
    .{.selector=symbol.yourself,.method=test3},
    .{.selector=symbol.cull_,.method=test4},
    .{.selector=symbol.value_,.method=test5}, // additional method forces larger hash
    .{.selector=symbol.cull_cull_cull_cull_,.method=test6}, // additional method forces larger hash
};
const symbolMethods3 = [_]SymbolMethod{
    .{.selector=symbol.value,.method=testNormal},
    .{.selector=symbol.value,.method=testNormal},
    .{.selector=symbol.self,.method=testNormal},
    .{.selector=symbol.yourself,.method=testNormal},
    .{.selector=symbol.cull_,.method=testNormal},
    .{.selector=symbol.value_,.method=testNonLocal},
    .{.selector=symbol.value_value_,.method=testNormal},
    .{.selector=symbol.value_value_value_,.method=testNormal},
    .{.selector=symbol.value_value_value_value_,.method=testNormal},
};
test "findTableSize" {
    const expectEqual = @import("std").testing.expectEqual;
    try expectEqual(findTableSize(symbolMethods1[0..]),5);
    try expectEqual(findTableSize(symbolMethods2[0..]),7);
    try expectEqual(findTableSize(symbolMethods3[0..]),17);
}
pub fn addClass(thread: *Thread, className: Object, instanceMethods: []const SymbolMethod, classMethods: []const SymbolMethod) !void {
    _ = thread;
    _ = className;
    _ = instanceMethods;
    _ = classMethods;
    return error.UnImplemented;
}
pub fn addDispatch(thread: *Thread, theClass: ClassIndex, superClass: ClassIndex, symbolMethods: []const SymbolMethod) void {
    const arena = thread.getArena().getGlobal();
    const dispatchSize = findTableSize(symbolMethods);
    const strct = arena.allocStruct(@truncate(ClassIndex,symbol.indexOf(symbol.Dispatch)),@sizeOf(Dispatch)+dispatchSize*@sizeOf(SymbolMethod),Dispatch) catch unreachable;
    strct.mod = dispatchSize;
    strct.super = superClass;
    const methods=@ptrCast([*]SymbolMethod,&strct.methods);
    for (methods[0..dispatchSize]) |*sm| {
        sm.selector=Nil;
        sm.method=dnu;
    }
    for (symbolMethods) |*sm| {
        var hash = randomHash(sm.selector,dispatchSize);
        if (!methods[hash].selector.equals(Nil)) hash+=1;
        if (extraSlots>2) {if (!methods[hash].selector.equals(Nil)) hash+=1;}
        if (!methods[hash].selector.equals(Nil)) unreachable;
        methods[hash].selector=sm.selector;
        methods[hash].method=sm.method;
    }
    classDispatch[theClass]=strct;
}
pub inline fn call(thread: *Thread, self: Object, selector: Object) MethodReturns {
    const theClass = self.get_class();
    return callClass(thread, theClass, self, selector);
}
pub fn callClass(thread: *Thread, theClass: ClassIndex, self: Object, selector: Object) MethodReturns {
    const dispatch = classDispatch[theClass];
    const hash = randomHash(selector,dispatch.mod);
    const symbolMethodPtr = @ptrCast([*]SymbolMethod,&dispatch.methods)+hash;
    if (selector.equals(symbolMethodPtr[0].selector)) return symbolMethodPtr[0].method(thread,self);
    if (selector.equals(symbolMethodPtr[1].selector)) return symbolMethodPtr[1].method(thread,self);
    if (extraSlots>1 and selector.equals(symbolMethodPtr[2].selector)) return symbolMethodPtr[2].method(thread,self);
    return dnu(thread,selector);
}
fn dispatchTableObject(classIndex: ClassIndex) HeapPtr {
    return @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),classDispatch[classIndex]));
}
test "addClass and call" {
    const expectEqual = @import("std").testing.expectEqual;
    var thread = try Thread.initForTest();
    try addClass(&thread,symbol.SmallInteger,symbolMethods1[0..],noMethods[0..]);
    const t42 = Object.from(42);
    thread.push(Object.from(17));
    try expectEqual(call(&thread,t42,symbol.value),MethodReturns.Normal);
    try expectEqual(thread.stack()[0],t42);
}
test "lookups of proper methods" {
    const expectEqual = @import("std").testing.expectEqual;
    var thread = try Thread.initForTest();
    try addClass(&thread,symbol.SmallInteger,symbolMethods2[0..],noMethods[0..]);
    const t42 = Object.from(42);
    thread.push(Object.from(17));
    _ = call(&thread,t42,symbol.value);
    try expectEqual(thread.stack()[0],Object.from(1));
    _ = call(&thread,t42,symbol.self);
    try expectEqual(thread.stack()[0],Object.from(2));
    _ = call(&thread,t42,symbol.yourself);
    try expectEqual(thread.stack()[0],Object.from(3));
    _ = call(&thread,t42,symbol.cull_);
    try expectEqual(thread.stack()[0],Object.from(4));
    _ = call(&thread,t42,symbol.value_);
    try expectEqual(thread.stack()[0],Object.from(5));
    _ = call(&thread,t42,symbol.cull_cull_cull_cull_);
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
