const std = @import("std");

const execute = struct {
    const ThreadedFn = *const fn (
        process: *Process,
        context: *Context,
    ) void;
};
const Process = extern struct {
    debugFn: ?execute.ThreadedFn,
};
const Context = struct {
    npc: execute.ThreadedFn,
};

test "die" {
    _ = Process{
        .debugFn=null,
    };
}
