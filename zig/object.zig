pub const Start_of_Literals = 0xfff2000000000000;
pub const False = @bitCast(Object,@as(u64,0xfff4000000000000));
pub const True = @bitCast(Object,@as(u64,0xfff6000000000001));
pub const Nil = @bitCast(Object,@as(u64,0xfff8000000000000));
const u64_MINVAL = 0xfffe000000000000;
const u64_ZERO =  0xffff000000000000;
pub const Object_MINVAL = @bitCast(Object,@as(u64,u64_MINVAL));
pub const Object_INT_ZERO = @bitCast(Object,@as(u64,u64_ZERO));
pub const Object_MAXVAL = @bitCast(Object,@as(u64,0xffffffffffffffff));
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
        return @bitCast(u64,self)>=u64_MINVAL;
    }
    pub fn is_double(self : Object) callconv(.Inline) bool {
        return @bitCast(u64,self)>=Start_of_Literals;
    }
    pub fn is_bool(self : Object) callconv(.Inline) bool {
        if (self==True) return true;
        return self==False;
    }
    pub fn is_heap(self : Object) callconv(.Inline) bool {
        if (@bitCast(u64,self)<=Start_of_Literals) return false;
        return @bitCast(u64,self)<@bitCast(u64,False);
    }
    pub fn as_int(self : Object) callconv(.Inline) i64 {
        return @bitCast(i64,@bitCast(u64,self)-u64_ZERO);
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
                return @bitCast(Object,@bitCast(u64,value)+%u64_ZERO);
            },
            .ComptimeInt => {
                return @bitCast(Object,@bitCast(u64,@as(i64,value))+%u64_ZERO);
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
                return @bitCast(Object,@ptrToInt(value)+Start_of_Literals);
            },
        }
    }
    pub fn fullHash(self : Object) callconv(.Inline) u64 {
        return @bitCast(u64,self)%16777213;
    }
    pub fn immediate_class(self : Object) callconv(.Inline) u64 {
        if (@bitCast(u64,self)<=Start_of_Literals) return 8;
        return (@bitCast(u64,self)>>49)&7;
    }
    pub fn get_class(self : Object) callconv(.Inline) u64 {
        if (@bitCast(u64,self)<=Start_of_Literals) return 8;
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
pub const Tag = enum (u3) { Object = 1, False, True, UndefinedObject, Symbol, Character, SmallInteger};
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
    try expect(@bitCast(u64,Object.from(42))==u64_ZERO+%42);
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
const HeapObject = packed struct {
    header: Header,
    objects: [0]Object,
};

test "sizeof" {
    const expect = @import("std").testing.expect;
    try expect(@sizeOf(HeapObject) == 8);
}
