const std = @import("std");

const execute = struct {
    const ThreadedFn = struct {
        f: *const fn (
            process: *Process,
            context: *Context,
        ) void,
    };
};
const Process = struct {
    debugFn: ?execute.ThreadedFn,
};
const Context = struct {
    npc: execute.ThreadedFn,
};

test "die" {
    _ = Process{
        .debugFn = null,
    };
}
