const std = @import("std");
pub const Encoding = enum {
    zag,
    zagAlt,
    nan,
    spur,
    taggedPtr,
    cachedPtr,
    ptr,
    taggedInt,
    onlyInt,
    onlyFloat,
    pub fn fromName(key: []const u8) !Encoding {
        inline for (@typeInfo(Encoding).@"enum".fields) |f| {
            if (std.mem.eql(u8, f.name, key)) {
                return @enumFromInt(f.value);
            }
        }
        return error.InvalidKey;
    }
    pub fn default() Encoding {
        return .zag;
    }
    pub fn module(self: Encoding) type {
        return switch (self) {
            .zag, => @import("zag.zig"),
            .zagAlt => @import("zagAlt.zig"),
            .nan => @import("nan.zig"),
            .spur => @import("spur.zig"),
            .taggedPtr => @import("taggedPtr.zig"),
            .cachedPtr, .ptr => @import("ptr.zig"),
            .taggedInt => @import("taggedInt.zig"),
            .onlyInt => @import("onlyInt.zig"),
            .onlyFloat => @import("onlyFloat.zig"),
        };
    }
};
test "fromName" {
    const match = Encoding.fromName;
    const expect = std.testing.expect;
    try expect(try match("zag") == .zag);
    try expect(try match("nan") == .nan);
    try expect(try match("spur") == .spur);
    try expect(try match("taggedPtr") == .taggedPtr);
    try expect(try match("taggedInt") == .taggedInt);
    try expect(try match("cachedPtr") == .cachedPtr);
    try expect(try match("ptr") == .ptr);
    try expect(match("fubar") == error.InvalidKey);
}
