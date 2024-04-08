const std = @import("std");
const mem = std.mem;
const ClassIndex = enum { String };
pub fn StaticObject(comptime T: type) type {
    return extern struct{
        header: u64,
        obj_: T,
        const Self = @This();
        pub fn init(aStruct: T) Self {
            return .{
                .header = undefined,
                .obj_ = aStruct};
        }
        pub fn setHeader(self: *Self, _: ClassIndex, _: u24) void {
            self.header = @sizeOf(T);
        }
        pub fn object(self: *Self) *T {
            return @ptrCast(&self.obj_);
        }
    };
}
pub fn staticObject(comptime class: ClassIndex, comptime obj: anytype) *@TypeOf(obj) {
    const T = @TypeOf(obj);
    return result: {
        var result = StaticObject(T).init(obj);
        var hash: u64 = 0;
        for (result.obj_[0..@min(@sizeOf(T),6)]) |p|
            hash = hash*%3+p;
        result.setHeader(class,@truncate(hash));
        break :result result.object();
    };
}
pub fn compileTimeString(comptime str: []const u8) *[str.len]u8 {
    comptime var result = staticObject(.String,[_]u8{0} ** (str.len));
    for (str, result[0..str.len]) |c, *r| {
        r.* = c;
    }
    return result;
}
const abcde = compileTimeString("abcdefghijklm");
test "compile time" {
    try std.testing.expect(mem.eql(u8, abcde, "abcdefghijklm"));
    const ref = @as(*u64,@ptrFromInt(@intFromPtr(abcde)-8));
    try std.testing.expectEqual(ref.*, 13);
    // try std.testing.expect(mem.eql(u8, try strings[3].arrayAsSlice(u8), "False"));
}
