const std = @import("std");
const Object = @import("object.zig").Object;
const Nil = @import("object.zig").Nil;
const Thread = @import("thread.zig").Thread;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const builtin = @import("builtin");
const class = @import("class.zig");
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
const methodT = fn(thread : *Thread, self: Object) MethodReturns;
pub const SymbolMethod = packed struct{ selector: Object, method: methodT};

const max_classes = 1000;
var classDispatch : [max_classes]HeapPtr = undefined;
inline fn randomHash(selector: Object, size: u16) u32 {
    const random = @truncate(u32,@bitCast(u64,selector))*%0xa1fdc7a3;
    return random%size;
}
const extraSlots = 3;
fn findTableSize(sm: []const SymbolMethod) u16 {
    var used : [default_prime+extraSlots]bool = undefined;
    var size = @intCast(u16,sm.len);
    outer: while (size<=default_prime) : (size += 1) {
        size = next_prime_larger_than(size);
        for (used[0..size+1]) |*b| b.* = false;
        for (used[size+1..size+extraSlots]) |*b| b.* = true;
        for (sm) |aSymbolMethod| {
            var hash = randomHash(aSymbolMethod.selector,size);
            if (used[hash]) {
                hash += 1;
                if (used[hash]) {
                    hash += 1;
                    if (used[hash]) {
                        continue :outer;
            }}}
            used[hash] = true;
        }
        return size+extraSlots;
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
pub const foo = packed struct{ n: u32, t: bool};
const fooX = [_]foo{
    .{1,true},
    .{2,false},
    .{3,true},
};
const symbolMethods = [_]SymbolMethod{
    .{.selector=symbol.value,.method=testNormal},
    .{.selector=symbol.self,.method=testNormal},
    .{.selector=symbol.yourself,.method=testNormal},
    .{.selector=symbol.cull_,.method=testNormal},
    .{.selector=symbol.value_,.method=testNonLocal},
};
const symbolMethods2 = [_]SymbolMethod{
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
    try expectEqual(findTableSize(symbolMethods[0..]),10);
    try expectEqual(findTableSize(symbolMethods2[0..]),20);
}
pub fn addClass(thread: *const Thread, theClass: class.ClassIndex, sm: []const SymbolMethod) void {
    //const stdout =  std.io.getStdOut().writer();
    //stdout.print("return used {any}\n",.{used[0..size+extraSlots]}) catch unreachable;

    //    const arena = thread.getArena().getGlobal();
    _ = thread;
    const dispatchSize = findTableSize(sm)+extraSlots;
    //_ = arena;
    _ = dispatchSize;
    _ = theClass;
    unreachable;
}
pub fn call(thread: *Thread, self: Object, selector: Object) MethodReturns {
    const theClass = self.get_class();
    const dispatch = classDispatch[theClass];
    const hash = randomHash(selector,dispatch.length/2-1);
    const symbolMethodPtr = @ptrCast([*]SymbolMethod,(@ptrCast(heap.HeaderArray,dispatch)+1))+hash;
    if (selector.equals(symbolMethodPtr[0].selector)) return symbolMethodPtr[0].method(thread,self);
    if (selector.equals(symbolMethodPtr[1].selector)) return symbolMethodPtr[1].method(thread,self);
    if (Nil.equals(symbolMethodPtr[1].selector)) return dnu(selector,thread);
    if (selector.equals(symbolMethodPtr[2].selector)) return symbolMethodPtr[2].method(thread,self);
    return dnu(selector,thread);
}
test "addClass and call" {
    const expectEqual = @import("std").testing.expectEqual;
    var thread = try Thread.initForTest();
    addClass(&thread,class.SmallInteger,symbolMethods[0..]);
    const t42 = Object.from(42);
    thread.push(t42);
    try expectEqual(call(&thread,t42,symbol.value),MethodReturns.Normal);
    try expectEqual(thread.stack()[0],symbol.value);
}
fn dnu(selector: Object,thread: *Thread) MethodReturns {
    _ = selector;
    _ = thread;
    unreachable;
}

fn gen_primes(comptime T : type, n_primes: usize) [n_primes]T {
    var p : [n_primes]T = undefined;
    var possible : T = 7;
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
const prime_values = gen_primes(primes_type,if (builtin.is_test) 13 else 20);
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
    const expect = @import("std").testing.expect;
    try expect(next_prime_larger_than(3)==7);
    try expect(next_prime_larger_than(24)==29);
    try expect(next_prime_larger_than(167)==167);
    try expect(next_prime_larger_than(224)==293);
    if (prime_values.len<20) {
        try expect(next_prime_larger_than(294)==default_prime);
    } else
        try expect(next_prime_larger_than(1889)==1889);
    try expect(next_prime_larger_than(1890)==default_prime);
}
