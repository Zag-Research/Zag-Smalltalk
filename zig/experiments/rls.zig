const std = @import("std");
const T = struct {
    something: u32,
    other: *u32,
    fn init(t: *T) void {
        t.something = 1234;
        t.other = &t.something; 
    }
};

var t1: T = undefined;
test "T" {
    var t2: T = undefined;
    t1.init();
    try std.testing.expectEqual(t1.other,&t1.something);
    t2.init();
    try std.testing.expectEqual(t2.other,&t2.something);
}
const RLS = struct {
    something: u32,
    other: *u32 = undefined,
    const Self = @This();
    fn init() RLS {
        var rls = Self{.something = 1234};
        rls.other = &rls.something;
        return rls;
    }
};

var rls1 = RLS.init();
test "RLS" {
    var rls2 = RLS.init();
    try std.testing.expectEqual(rls1.other,&rls1.something);
    try std.testing.expectEqual(rls2.other,&rls2.something);
}
