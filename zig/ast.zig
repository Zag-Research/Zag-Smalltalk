const a = 1;
pub const b = 2;
pub const c = 3;
pub const NEGATIVE_INF = 0xfff0000000000000;
pub const False = @bitCast(Object,@as(u64,0xfff4000000000000));
pub const True = @bitCast(Object,@as(u64,0xfff6000001000001));
pub const Nil = @bitCast(Object,@as(u64,0xfff8000000000000));
const INT_MINVAL = 0xfffe000000000000;
const INT_ZERO =  0xffff000000000000;
const INT_MAXVAL = 0xffffffffffffffff;
const native_endian = @import("builtin").target.cpu.arch.endian();
pub const Header = switch (native_endian) {
    .Big => 
        packed struct {
            numSlots: u16,
            format : u8,
            hash : u24,
            classIndex : u16,
    },
    .Little =>
        packed struct {
            classIndex : u16,
            hash : u24,
            format : u8,
            numSlots: u16,
    },
};
test "Header structure" {
    const expect = @import("std").testing.expect;
    try expect(@sizeOf(Header) == 8);
    var header = Header{ .numSlots = 17, .format = 10, .hash=0x123, .classIndex = 35 };
    var asInt = @bitCast(u64, header);
    try expect(asInt == 0x00110a0001230023);
}

const objectMethods = struct {
    pub fn is_int(self : Object) callconv(.Inline) bool {
        return @bitCast(u64,self)>=INT_MINVAL;
    }
    pub fn is_double(self : Object) callconv(.Inline) bool {
        return @bitCast(u64,self)>=NEGATIVE_INF;
    }
    pub fn is_bool(self : Object) callconv(.Inline) bool {
        if (self==True) return true;
        return self==False;
    }
    pub fn is_heap(self : Object) callconv(.Inline) bool {
        if (@bitCast(u64,self)<=NEGATIVE_INF) return false;
        return @bitCast(u64,self)<@bitCast(u64,False);
    }
    pub fn as_int(self : Object) callconv(.Inline) i64 {
        return @bitCast(i64,@bitCast(u64,self)-INT_ZERO);
    }
    pub fn as_double(self : Object) callconv(.Inline) f64 {
        return @bitCast(f64,self);
    }
    pub fn as_bool(self : Object) callconv(.Inline) bool {
        return @bitCast(u64,self)==@bitCast(u64,True);
    }
    pub fn as_char(self : Object) callconv(.Inline) u8 {
        return @intCast(u8,self.hash&0xff);
    }
    pub fn as_string(self : Object) callconv(.Inline) [] const u8 {
        //
        // symbol handling broken
        //
        _ = self;
        return "dummy string";
    }
    pub fn as_pointer(self : Object) callconv(.Inline) *Header {
        return @intToPtr(*Header,@bitCast(usize,@bitCast(i64,self)<<12>>12));
    }
    pub fn from(value: anytype) callconv(.Inline) Object {
        switch (@typeInfo(@TypeOf(value))) {
            .Int => {
                return @bitCast(Object,@bitCast(u64,value)+%INT_ZERO);
            },
            .ComptimeInt => {
                return @bitCast(Object,@bitCast(u64,@as(i64,value))+%INT_ZERO);
            },
            .Float => {
                return  @bitCast(Object,value);
            },
            .ComptimeFloat => {
                return  @bitCast(Object,@as(f64,value));
            },
            .Bool => {
                return if (value) True else False;
            },
            else => {
                return @bitCast(Object,@ptrToInt(value)+NEGATIVE_INF);
            },
        }
    }
    pub fn closure(self : Object) callconv(.Inline) Object {
        return @bitCast(Object,@bitCast(u64,self)+(1<<49));
    }
    pub fn immediate_class(self : Object) callconv(.Inline) u64 {
        if (@bitCast(u64,self)<=NEGATIVE_INF) return 8;
        return (@bitCast(u64,self)>>49)&7;
    }
    pub fn get_class(self : Object) callconv(.Inline) u64 {
        if (@bitCast(u64,self)<=NEGATIVE_INF) return 8;
        const immediate = (@bitCast(u64,self)>>49)&7;
        if (immediate>1) return immediate;
        return self.as_pointer().*.get_class();
    }
    pub fn println(self : Object, writer : @import("std").fs.File.Writer) !void {
        try self.print(writer);
        try writer.print("\n",.{});
    }
    pub fn print(self : Object, writer : @import("std").fs.File.Writer) !void {
        try switch (self.immediate_class()) {
            0 => writer.print("object",.{}),//,as_pointer(self)),
            1 => writer.print("closure",.{}),//,as_pointer(x));
            2 => writer.print("false",.{}),
            3 => writer.print("true",.{}),
            4 => writer.print("nil",.{}),
            5 => writer.print("{s}",.{self.as_string()}),
            6 => writer.print("${c}",.{self.as_char()}),
            7 => writer.print("{d}",.{self.as_int()}),
            8 => writer.print("{}",.{self.as_double()}),
            else => unreachable
        };
    }
};
test "printing" {
    const stdout = @import("std").io.getStdOut().writer();
    const symbol = @import("symbol.zig");
    try Object.from(42).println(stdout);
    try symbol.yourself.println(stdout);
}
pub const Tag = enum (u3) { Object, Closure, False, True, UndefinedObject, Symbol, Character, SmallInteger};
pub const Object = switch (native_endian) {
    .Big => 
        packed struct {
            signMantissa: u12,
            tag : Tag,
            highHash: u25,
            hash : i24,
            usingnamespace objectMethods;
    },
    .Little =>
        packed struct {
            hash : i24,
            highHash: u25,
            tag : Tag,
            signMantissa: u12,
            usingnamespace objectMethods;
    },
};

test "from conversion" {
    const expect = @import("std").testing.expect;
    try expect(@bitCast(f64,Object.from(3.14))==3.14);
    try expect(@bitCast(u64,Object.from(42))==INT_ZERO+%42);
}
test "as conversion" {
    const expect = @import("std").testing.expect;
    const x = Object.from(42);
    try expect(Object.from(&x).is_heap());
    try expect(Object.from(3.14).as_double()==3.14);
    try expect(Object.from(42).as_int()==42);
    try expect(Object.from(42).is_int());
    try expect(Object.from(true).as_bool()==true);
}
//pub fn from_object(x : anytype) callconv(.Inline) Object {
  //  return 42;
//}
var next_thread_number : u64 = 0;
const default_heap_size = 512;
threadlocal var default_heap : [default_heap_size]Object = undefined;
pub const threadT = packed struct {
    id : u64,
    heap : [*]Object,
    stack: [*]Object,
    start: [*]Object,
    end:   [*]Object,
    pub fn init() threadT {
        defer next_thread_number += 1;
        const end = @as([*]Object,&default_heap)+default_heap_size;
        return threadT {
            .id = next_thread_number,
            .stack = end,
            .end = end,
            .start = &default_heap,
            .heap = &default_heap,
        };
    }
};
pub threadlocal var thread : threadT = undefined;
pub const returnE = enum {
    Normal,
    NonLocal
};
const methodT = fn(self : Object, stack : [*]Object, heap : [*]Object) returnE;
test "thread 0 initialization" {
    thread = threadT.init();
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

// #define from_object(addr) ((((long)(void*)addr))+(0x7ff8l<<49))
// #define from_closure(addr) ((((long)(void*)addr))+(0x7ff9l<<49))
// #define from_char(c) ((objectT)(c))|(0x7ffel<<49)
