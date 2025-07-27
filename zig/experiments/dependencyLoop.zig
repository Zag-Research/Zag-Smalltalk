const execute = struct {
    const ThreadedFn = packed struct {
        f: Fn,
        const Fn = *const fn (
            process: *Process,
        ) void;
    };
};
const Process = struct {
    m: [@sizeOf(P)]u8,
    const P = extern struct {
        h: Fields,
        const Fields = extern struct {
            debugFn: execute.ThreadedFn,
        };
    };
};
fn f(_: *Process) void {}
test "die" {
    const tfn = execute.ThreadedFn{ .f = &f };
    var p: Process = undefined;
    tfn.f(&p);
}
