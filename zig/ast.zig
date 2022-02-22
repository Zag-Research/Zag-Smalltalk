const a = 1;
pub const b = 2;
pub const c = 3;
pub const NEGATIVE_INF = 0xfff0000000000000;
const False = 0xfff4000000010000;
const True = 0xfff6000000100001;
const Nil = 0xfff8100000000000;
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
            highHash: u17,
            hash : i32,
    },
    .Little =>
        packed struct {
            hash : i32,
            highHash: u17,
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

pub fn as_int(x : Object) callconv(.Inline) i64 {
    return @bitCast(i64,@bitCast(u64,x)-INT_ZERO);
}
pub fn as_double(x : Object) callconv(.Inline) f64 {
    return @bitCast(f64,x);
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
const methodT = fn(stack : [*]Object, heap : [*]Object) returnE;
test "thread 0 initialization" {
    thread = threadT.init();
}

//#include <stdio.h>
//#define print(t,v) printf("%-10s 0x%lx\n",t,v)

// #define print(v) ({typeof (v) V = (v);printf("%-25s 0x%lx %d %s\n",#v,V,short_class(V),printString(V));})
// typedef unsigned long objectT;
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
// typedef objectT (*f0T)(objectT self);
// typedef objectT (*f1T)(objectT self,objectT other);
// typedef objectT (*f2T)(objectT self,objectT other,objectT other2);
// typedef objectT (*f3T)(objectT self,objectT other,objectT other2,objectT other3);
// typedef struct {objectT key;f0T func;} matchT;
// typedef struct{long header;objectT name;objectT superclass;short size;objectT matches;} classT;
// #define REF(name) {S_##name,(f0T)&M_##name}
// #define REF2(sym,func) {S_##sym,(f0T)&func}
// #define NILREF {nil,(f0T)0}
// #define HEADER(format,numSlots,hash,classIndex) ((((long)(numSlots))<<48)|((long)(format)<<40)|((hash)<<16)|(classIndex))
// #define DISPATCH_HEADER(name,size) {HEADER(15,size,S_##name&0xfffff,11+(S_##name&0xffff)),(f0T)from_object(&C_##name)}
// #define CLASS_HEADER(name) HEADER(10,4,S_##name&0xfffff,11+(S_Class&0xffff))
// #define DISPATCH(name) from_object(dispatch_##name)
// #define CLASS(name,super,size) classT C_##name={CLASS_HEADER(name),S_##name,from_object(&C_##super),size,DISPATCH(name)}
// #define METACLASS(name,super,size) classT C_M_##name={CLASS_HEADER(name),S_##name,from_object(&C_M_##super),size,DISPATCH(M_##name)}
