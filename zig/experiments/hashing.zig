const std = @import("std");
const os = std.os;
const builtin = @import("builtin");
const u32_phi_inverse=2654435769;
inline fn bumpSize(size:u16) u16 {
    return size*2;
}
inline fn initialSize(size:usize) u16 {
    var n = @intCast(u16,size);
    n -= 1;
    n |= n>>8;
    n |= n>>4;
    n |= n>>2;
    n |= n>>1;
    return n+1;
}
fn findTableSize(sm: []const u32) u16 {
    var used : [default_prime]bool = undefined;
    var size = initialSize(sm.len);
    outer: while (size<default_prime) {
        for (used[0..size+1]) |*b| b.* = false;
        for (sm) |aSymbolMethod| {
            var hash = aSymbolMethod*%u32_phi_inverse & ~@as(u32,31) | @truncate(u5,@clz(u32,size)+1);
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
const Fix = struct {index:u16,a:u16,b:u16,c:u16};
const CF = struct{size:u16,hash:u32};
const WC = struct{size:u16,hash:u32,fix:[]Fix};
const TableStructureResult = union(enum) {
    conflictFree: CF,
    withConflicts: WC,
};
fn Dispatch(comptime T: type, extractHash: fn(T) u32, maxSize: comptime_int) type {
    return struct{
        fn findTableSize(sm: []const T,fix: *[12]Fix) !TableStructureResult {
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
            var extra: u16 = 1;
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
                        extra += (s+1)&0xFE;
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
            return TableStructureResult{.withConflicts=.{.size=conflictSize+extra,.hash=bestConflictRand,.fix=fixup}};
        }
    };
}
pub const noMethods = [0]u32{};
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
fn id(x:u32) u32 {return x;}
test "tablesize" {
    const stdout = std.io.getStdOut().writer();
    var fix: [12]Fix = undefined;
    {
        const findTableSize2=Dispatch(u32,id,35).findTableSize;
        try stdout.print("\n",.{});
        try stdout.print("e1: {any}\n",.{try findTableSize2(e1[0..],&fix)});
        try stdout.print("e2: {any}\n",.{try findTableSize2(e2[0..],&fix)});
        try stdout.print("e3: {any}\n",.{try findTableSize2(e3[0..],&fix)});
        try stdout.print("e4: {any}\n",.{try findTableSize2(e4[0..],&fix)});
    }
    {
        const findTableSize2=Dispatch(u32,id,128).findTableSize;
        try stdout.print("e5: {any}\n",.{try findTableSize2(e5[0..],&fix)});
    }
    {
        const findTableSize2=Dispatch(u32,id,17).findTableSize;
        var e6: [15]u32 = undefined;
        for (e6) |*v,i| v.*=@truncate(u32,i);
        try stdout.print("e6: {any}\n",.{try findTableSize2(e6[0..],&fix)});
    }
}
test "timing" {
    try time(400_000);
}
pub fn main() !void {
    try time(1_000_000*os.argv.len);
}
fn time(count: u64) !void {
    const stdout = std.io.getStdOut().writer();
    const ts=std.time.nanoTimestamp;
    var key:u32 = 12345;
    var size:u32 = 0;
    var loop=count*10;
    const mask = initialSize(@truncate(u16,count));
    while (loop>0) : (loop-=1) {
        key ^= @truncate(u32,loop)^mask;
        while (size>0) : (size-=1) {
            key +=size;
            key = key&mask;
    }}
    loop=count;
    var start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        while (size>0) : (size-=1) {
            key +=size;
            key = key&mask;
    }}
    const base = ts()-start;
    try stdout.print("base: {d:12}\n",.{base});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        while (size>0) : (size-=1) {
            key +=size;
            key = key%mask;
    }}
    try stdout.print("mod:  {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        while (size>0) : (size-=1) {
            key +=size;
            key= (key^(key>>5)^(key>>3)) & size;
    }}
    try stdout.print("xor:  {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        while (size>0) : (size-=1) {
            key +=size;
            key=key^(key>>5);
            key=key^(key>>3);
            key= key & size;
    }}
    try stdout.print("xor=: {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        while (size>0) : (size-=1) {
            key +=size;
            key= key*%12345 & size;
    }}
    try stdout.print("mult: {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        const sh:u5 = @truncate(u5,@clz(u32,size));
        while (size>0) : (size-=1) {
            key +=size;
            key= key*%2654435769 >> sh;
    }}
    try stdout.print("phi:  {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        const rand =u32_phi_inverse & ~@as(u32,31) | @truncate(u5,@clz(u32,size)+1);
        while (size>0) : (size-=1) {
            key +=size;
            key= key *% rand >> @truncate(u5,rand);
    }}
    try stdout.print("phix: {d:12} {d:12} {any}\n",.{ts()-start,ts()-start-base,key});
    loop=count;
    start=ts();
    while (loop>0) : (loop-=1) {
        size=@truncate(u32,loop)%2000;
        key ^= @truncate(u32,loop)^mask;
        const rand = u32_phi_inverse & ~@as(u32,31) | @truncate(u5,@clz(u32,size)+1);
        while (size>0) : (size-=1) {
            key +=size;
            key = key *% rand >> @truncate(u5,rand);
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
