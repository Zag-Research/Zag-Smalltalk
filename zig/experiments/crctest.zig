const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const crc = std.hash.Crc32;

test {
    try expectEqual(crc.hash("Hello world!"), 461707669); // check if it's the same crc as on Pharo
}
