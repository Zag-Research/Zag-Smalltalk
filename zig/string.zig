const std = @import("std");
const heap = @import("heap.zig");
const class = @import("class.zig");
const Object = @import("object.zig").Object;
fn hash24(str: [] const u8) u24 {
    const phi = @import("utilities.zig").inversePhi(u32);
    var hash = phi*%(str.len+%1);
    for (str) |c|
        hash +%= phi*%c;
    return @truncate(u24,hash);
}
pub fn CompileTimeString(comptime str: [] const u8) type {
    const size = str.len;
    const hash = hash24(str);
    return struct {
        header: heap.Header,
        chars: [size]u8,
        const Self = @This();
        pub fn init() Self {
            var result : Self = .{
                .header = heap.header((size+7)/8,heap.Format.immutable.raw(u8,size),class.String_I,hash,15),
                .chars = [_]u8{0}**size,
            };
            for (str) |c,idx| {
                result.chars[idx]=c;
            }
            return result;
        }
        fn h(self: * const Self) []const u8 {
            return @ptrCast([*]const u8,self)[0..(size+15)/8*8];
        }
        fn o(self: * const Self) Object {
            return Object.from(@ptrCast(*const heap.Header,self));
        }
    };
}
const abcde = CompileTimeString("abcde").init();
test "compile time" {
    std.debug.print("abcde: {any}\n",.{abcde.h()});
    std.debug.print("abcde: {any}\n",.{abcde.o()});
}
