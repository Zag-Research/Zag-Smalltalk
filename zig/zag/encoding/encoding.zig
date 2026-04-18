const std = @import("std");
pub const Encoding = enum {
    zag,
    zagSpur,
    zagMixed,
    nan,
    spur,
    taggedPtr,
    taggedHigh,
    cachedPtr,
    ptr,
    taggedInt,
    taggedSMI,
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
};
pub fn module(self: anytype) type {
    return switch (self) {
        .zag => @import("zag.zig"),
        .zagSpur => @import("zagSpur.zig"),
        .zagMixed => @import("zagOrig.zig"),
        .nan => @import("nan.zig"),
        .spur => @import("spur.zig"),
        .taggedPtr => @import("taggedPtr.zig"),
        .taggedHigh => @import("taggedHigh.zig"),
        .cachedPtr, .ptr => @import("ptr.zig"),
        .taggedInt, .taggedSMI => @import("taggedInt.zig"),
        .onlyInt => @import("onlyInt.zig"),
        .onlyFloat => @import("onlyFloat.zig"),
    };
}
pub fn tag(self: anytype) ?u64 {
    return switch (self) {
        .zag => 'Z' + ('a' << 8),
        .zagMixed => 'Z' + ('M' << 8),
        .nan => 'N' + ('a' << 8),
        else => null,
    };
}
test "fromName" {
    const match = Encoding.fromName;
    const expect = std.testing.expect;
    try expect(try match("zag") == .zag);
    try expect(try match("zagMixed") == .zagMixed);
    try expect(try match("zagSpur") == .zagSpur);
    try expect(try match("spur") == .spur);
    try expect(try match("nan") == .nan);
    try expect(try match("taggedPtr") == .taggedPtr);
    try expect(try match("taggedHigh") == .taggedHigh);
    try expect(try match("taggedInt") == .taggedInt);
    try expect(try match("cachedPtr") == .cachedPtr);
    try expect(try match("ptr") == .ptr);
    try expect(match("fubar") == error.InvalidKey);
}
