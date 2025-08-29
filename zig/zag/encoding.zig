const std = @import("std");
pub const Encoding = enum {
    zag,
    nan,
    spur,
    taggedPtr,
    cachedPtr,
    ptr,
    pub fn fromName(key: []const u8) !Encoding {
        inline for (@typeInfo(Encoding).@"enum".fields) |f| {
            if (std.mem.eql(u8, f.name, key)) {
                return @enumFromInt(f.value);
            }
        }
        return error.InvalidKey;
    }
};
test "fromName" {
    const match = Encoding.fromName;
    const expect = std.testing.expect;
    try expect(try match("zag") == .zag);
    try expect(try match("nan") == .nan);
    try expect(try match("spur") == .spur);
    try expect(try match("taggedPtr") == .taggedPtr);
    try expect(try match("cachedPtr") == .cachedPtr);
    try expect(try match("ptr") == .ptr);
    try expect(match("fubar") == error.InvalidKey);
}
