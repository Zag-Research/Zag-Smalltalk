const std = @import("std");
pub const Encoding = enum {
    zag,
    zagSpur,
    zagOrig,
    compact,
    compact6,
    compactI,
    compactI6,
    nan,
    spur,
    spurOpt,
    taggedLow,
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
        .zagOrig => @import("zagOrig.zig"),
        .compact, .compact6, .compactI, .compactI6 => @import("compact.zig"),
        .nan => @import("nan.zig"),
        .spur, .spurOpt => @import("spur.zig"),
        .taggedLow => @import("taggedLow.zig"),
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
        .zagOrig => 'Z' + ('O' << 8),
        .nan => 'N' + ('a' << 8),
        .compact, .compact6, .compactI, .compactI6 => 'Z' + ('C' << 8),
        else => null,
    };
}
test "fromName" {
    const match = Encoding.fromName;
    const expect = std.testing.expect;
    try expect(try match("zag") == .zag);
    try expect(try match("zagOrig") == .zagOrig);
    try expect(try match("zagSpur") == .zagSpur);
    try expect(try match("compact") == .compact);
    try expect(try match("compact6") == .compact6);
    try expect(try match("compactI") == .compactI);
    try expect(try match("compactI6") == .compactI6);
    try expect(try match("spur") == .spur);
    try expect(try match("spurOpt") == .spurOpt);
    try expect(try match("nan") == .nan);
    try expect(try match("taggedPtr") == .taggedPtr);
    try expect(try match("taggedHigh") == .taggedHigh);
    try expect(try match("taggedInt") == .taggedInt);
    try expect(try match("cachedPtr") == .cachedPtr);
    try expect(try match("ptr") == .ptr);
    try expect(match("fubar") == error.InvalidKey);
}
