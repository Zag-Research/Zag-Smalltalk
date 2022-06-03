const std = @import("std");
const builtin = @import("builtin");
const TS = enum { prime, any, odd, po2, phi, phi2};
const tableStyle = TS.phi;
const u32_phi_inverse=2654435769;
inline fn randomHash(key: u32, size: u32,shift:u5) u32 {
    const random = switch (tableStyle) {
         . po2 => (key^(key>>5)^(key>>3)) & (size-1),
        // . po2 => (key*%(size-3)) & (size-1),
        // . po2 => key*%0xa1fdc7a3 & (size-1),
        . phi => key*%u32_phi_inverse >> shift,
        . phi2 => (key^(key>>8))*%u32_phi_inverse >> shift,
        else => key%size,
    };
    //@import("std").io.getStdOut().writer().print("key: {any} random: {any}\n",.{key,random}) catch unreachable;
    return random;
}
fn bumpSize(size:u16) u16 {
    return switch (tableStyle) {
        .prime => next_prime_larger_than(size+1),
        .any => size+1,
        .odd => (size+2)|1,
        .po2,.phi,.phi2 => size*2,
    };
}
fn initialSize(size:usize) u16 {
    var n = @intCast(u16,size);
    return switch (tableStyle) {
        .prime => next_prime_larger_than(n),
        .any => n,
        .odd => n|1,
        .po2,.phi,.phi2 => blk: {
            n -= 1;
            n |= n>>8;
            n |= n>>4;
            n |= n>>2;
            n |= n>>1;
            break :blk n+1;
    }};
}
fn findTableSize(sm: []const u32) u16 {
    var used : [default_prime]bool = undefined;
    var size = initialSize(sm.len);
    outer: while (size<default_prime) {
        for (used[0..size+1]) |*b| b.* = false;
        for (sm) |aSymbolMethod| {
            var hash = randomHash(aSymbolMethod,size,@truncate(u5,@clz(u32,size)+1));
            if (used[hash]) {
                size = bumpSize(size);
                continue :outer;
            }
            used[hash] = true;
        }
        @import("std").io.getStdOut().writer().print("for table of size {} tablesize is {}\n",.{sm.len,size}) catch unreachable;
          return size;
    }
    unreachable;
}
fn findTableSize2(sm: []const u32) u16 {
    var used : [default_prime]bool = undefined;
    var tries: u32 = undefined;
    var size = initialSize(sm.len);
    var rand : u32 = default_prime;
    outer: while (size<default_prime) {
        tries = 1;
        @import("std").io.getStdOut().writer().print("size={}\n",.{size}) catch unreachable;
        findMultiplier: while (tries<=10) : (tries += 1) {
            rand = rand *%0xa1fdc7a3;
            @import("std").io.getStdOut().writer().print("  try={} rand={}\n",.{tries,rand}) catch unreachable;
            for (used[0..size+1]) |*b| b.* = false;
            for (sm) |key| {
                // var hash = randomHash(aSymbolMethod,size,@truncate(u5,@clz(u32,size)+1));
                const hash1 = ((key ^ 12345) *% rand) >> 3;
                const hash = hash1 & (size-1);
                @import("std").io.getStdOut().writer().print("    key={} hash={} hash1={}\n",.{key,hash,hash1}) catch unreachable;
                if (used[hash]) continue :findMultiplier;
                used[hash] = true;
            }
            break :outer;
        }
        size = bumpSize(size);
    }
    if (size<default_prime) {
        @import("std").io.getStdOut().writer().print("for table of size {} tablesize is {} after {} tries\n",.{sm.len,size,tries}) catch unreachable;
        return size;
    }
    unreachable;
}

pub const noMethods = [0]u32{};
const e1 = [_]u32{6, 518, 38, 2};
const e2 = [_]u32{6, 518, 38, 2, 7, 5};
const e3 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47};
test "timing" {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("e1: {any}\n",.{findTableSize(e1[0..])});
    try stdout.print("e2: {any}\n",.{findTableSize(e2[0..])});
    try stdout.print("e3: {any}\n",.{findTableSize(e3[0..])});
}
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const ts=std.time.nanoTimestamp;
    var key:u32 = 12345;
    var count:u64 = 1_000_000;
    var size:u32 = 0;
    var loop=count*10;
    while (loop>0) : (loop-=1) {
        size=1000;
        while (size>0) : (size-=1) {
            key = key&size;
    }}
    loop=count;
    var start=ts();
    while (loop>0) : (loop-=1) {
        size=1000;
        while (size>0) : (size-=1) {
            key = key&size;
    }}
    const base = ts()-start;
    try stdout.print("base: {d:12}\n",.{base});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=1000;
        while (size>0) : (size-=1) {
            key = key%size;
    }}
    try stdout.print("mod:  {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=1000;
        while (size>0) : (size-=1) {
            key= (key^(key>>5)^(key>>3)) & size;
    }}
    try stdout.print("xor:  {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=1000;
        while (size>0) : (size-=1) {
            key=key^(key>>5);
            key=key^(key>>3);
            key= key & size;
    }}
    try stdout.print("xor=: {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=1000;
        while (size>0) : (size-=1) {
            key= key*%12345 & size;
    }}
    try stdout.print("mult: {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=1000;
        const sh:u5 = @truncate(u5,@clz(u32,size));
        while (size>0) : (size-=1) {
            key= key*%2654435769 >> sh;
    }}
    try stdout.print("phi:  {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=1000;
        const sh:u5 = @truncate(u5,@clz(u32,size));
        while (size>0) : (size-=1) {
            key= randomHash(key,size,sh);
    }}
    try stdout.print("phi+: {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
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
