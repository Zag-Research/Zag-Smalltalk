const Object = @import("object.zig").Object;
const Thread = @import("thread.zig").Thread;
pub const MethodReturns = enum {
    Normal,
    PrimitiveFailed,
    NonLocal,
    ExceptionSignaled,
    pub fn nonLocal(self : MethodReturns) bool {
        return self>=MethodReturns.NonLocal;
    }
};
const methodT = fn(thread : *Thread) MethodReturns;
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
const prime_values = gen_primes(u32,13);
pub fn next_prime_larger_than(n : u32) u32 {
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
    return 11959;
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
        try expect(next_prime_larger_than(294)==11959);
    } else
        try expect(next_prime_larger_than(1889)==1889);
    try expect(next_prime_larger_than(1890)==11959);
}
pub const SymbolMethod = struct {symbol: []u8, method: methodT};
