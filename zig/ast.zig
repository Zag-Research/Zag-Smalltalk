const a = 1;
pub const b = 2;
pub const c = 3;
pub const NEGATIVE_INF = 0xfff0000000000000;
pub const False = @bitCast(Object,@as(u64,0xfff4000000100002));
pub const True = @bitCast(Object,@as(u64,0xfff6000000100001));
pub const Nil = @bitCast(Object,@as(u64,0xfff8000000000000));
const INT_MINVAL = 0xfffe000000000000;
const INT_ZERO =  0xffff000000000000;
const INT_MAXVAL = 0xffffffffffffffff;
const native_endian = @import("builtin").target.cpu.arch.endian();
const Header = switch (native_endian) {
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

pub const Tag = enum (u3) { Object, Closure, False, True, UndefinedObject, Symbol, Character, SmalltInteger};
pub const Object = switch (native_endian) {
    .Big => 
        packed struct {
            signMantissa: u12,
            tag : Tag,
            highHash: u25,
            hash : i24,
    },
    .Little =>
        packed struct {
            hash : i24,
            highHash: u25,
            tag : Tag,
            signMantissa: u12,
    },
};

pub fn from(value: anytype) Object {
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
//            .Pointer => {
  //              return self.write(value);
    //        },
            else => {
                @compileError("Unable to convert '" ++ @typeName(@TypeOf(value)) ++ "'");
            },
    }
}
test "from conversion" {
    const expect = @import("std").testing.expect;
    try expect(@bitCast(f64,from(3.14))==3.14);
    try expect(@bitCast(u64,from(42))==INT_ZERO+%42);
}
pub fn is_int(x : Object) callconv(.Inline) bool {
    return @bitCast(u64,x)>=INT_MINVAL;
}
pub fn is_double(x : Object) callconv(.Inline) bool {
    return @bitCast(u64,x)>=NEGATIVE_INF;
}
pub fn is_bool(x : Object) callconv(.Inline) bool {
    if (x==True) return true;
    return x==False;
}
pub fn as_int(x : Object) callconv(.Inline) i64 {
    return @bitCast(i64,@bitCast(u64,x)-INT_ZERO);
}
pub fn as_double(x : Object) callconv(.Inline) f64 {
    return @bitCast(f64,x);
}
pub fn as_bool(x : Object) callconv(.Inline) bool {
    return @bitCast(u64,x)==@bitCast(u64,True);
}
test "as conversion" {
    const expect = @import("std").testing.expect;
    try expect(as_double(from(3.14))==3.14);
    try expect(as_int(from(42))==42);
    try expect(as_bool(from(true))==true);
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
// #define as_pointer(x) (((long)(x))<<12>>12)
// #define get_class(x) (((objectT)(x)>>49)>0x7ff8?((int)((objectT)(x)>>49)&7):((objectT)(x)>>49)<0x7ff0?8:get_class_7ff0(x))
// #define short_class(x) (((objectT)(x))>NEGATIVE_INF?((int)((objectT)(x)>>49)&7):8)
// static inline int get_class_7ff0(const objectT x) {
//   long ptr = as_pointer(x);
//   if (ptr==0) return 8;
//   ptr = *((long *)ptr);
//   if (ptr>0) return ptr&0xffff;
//   __builtin_trap();
// }
// static char * printString(objectT x) {
//   static char result[100];
//   switch (short_class(x)) {
//   case 0:
//     snprintf(result,sizeof(result),"object 0x%lx",as_pointer(x));
//     break;
//   case 1:
//     snprintf(result,sizeof(result),"closure 0x%lx",as_pointer(x));
//     break;
//   case 2:
//     snprintf(result,sizeof(result),"false");
//     break;
//   case 3:
//     snprintf(result,sizeof(result),"true");
//     break;
//   case 4:
//     snprintf(result,sizeof(result),"nil");
//     break;
//   case 5:
//     snprintf(result,sizeof(result),"#%s",symbol_table[x&0xfffff]);
//     break;
//   case 6:
//     snprintf(result,sizeof(result),"$%c",(char)(x&0xffffff));
//     break;
//   case 7:
//     snprintf(result,sizeof(result),"%ld",as_int(x));
//     break;
//   case 8:
//     snprintf(result,sizeof(result),"%g",as_double(x));
//     break;
//   }
//   return result;
// }
