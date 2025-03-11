const std = @import("std");
const assert = std.debug.assert;
pub const blah = struct {
    pub fn foo() void {}
};
pub fn testHasFn(T: anytype) bool {
//    @compileLog(T);
    // switch (T) {
    //     type => 
            if (std.meta.hasFn(T,"foo")) {
                return true;
    //     },
    //     else => {},
    }
    return false;
}
test "the test" {
//    assert(testHasFn(@TypeOf(blah)));
    assert(!testHasFn(@TypeOf(testHasFn)));
}
