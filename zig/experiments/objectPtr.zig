const std = @import("std");
const Object = ?*ObjectElement;
const Nil: Object = null;
const ObjectElement = struct {
    header: u64,
    const Internal = packed struct {
        tag: u8,
        hash: u56,
    };
    fn isNil(self: Object) bool {
        return self == null;
    }
    fn int(self: Object) ?u56 {
        const internal: Internal = @bitCast(self);
        if (internal.tag == 1) return internal.hash;
        return null;
    }
    fn makeInt(v: u56) Object {
        return @ptrFromInt(@as(u64,@bitCast(Internal{.tag = 1, .hash = v})));
    }
};
test "int is int" {
    const f2 = ObjectElement.makeInt(42);
    if (f2.int()) |int| {
        try std.testing.expectEqual(42,int);
    } else
        try std.testing.expect(false);
}
test "nil is nil" {
    try std.testing.expect(Nil.isNil());
}
