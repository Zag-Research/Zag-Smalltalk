const std = @import("std");
const assert = std.debug.assert;
const A = enum(u8) { a1, a2, a3 };

test "A" {
    const x = A.a2;
    switch (x) {
        .a1....a2 => assert(true),
        else => assert(false),
    }
}
