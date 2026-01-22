const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Code = zag.execute.Code;
const Object = zag.Object;

pub fn initJitTest(method: anytype, process: *align(Process.alignment) Process, title: []const u8) !void {
    const MethodType = @TypeOf(method.*);
    method.* = try MethodType.init();
    process.init();
    std.debug.print("\n--- {s} ---\n", .{title});
    method.dump();
}

pub fn runCompiled(
    method: anytype,
    compiled: anytype,
    process: *align(Process.alignment) Process,
    op_positions: []const usize,
    sp_opt: ?SP,
) i64 {
    const context = process.getContext();
    const sp = sp_opt orelse process.endOfStack();
    const code = compiled.code[0..];
    for (op_positions, 0..) |pos, i| {
        method.patchOp(code, i, pos);
    }

    const pc = PC.init(&code[0]).next();
    const extra = Extra.forMethod(compiled.asCompiledMethodPtr(), sp);
    const entry = method.getEntryFor(0);
    const result_sp = entry(pc, sp, process, context, extra);

    return result_sp.at(0).to(i64);
}

pub fn setLiteral(code: []Code, index: usize, value: Object) void {
    code[index] = Code.objectOf(value);
}

pub fn reportResult(got: i64, expected: i64) !void {
    std.debug.print("Got: {}, Expected: {}\n", .{ got, expected });
    if (got != expected) {
        std.debug.print("FAILED!\n", .{});
        return error.TestFailed;
    }
    std.debug.print("SUCCESS!\n", .{});
}

pub fn runTest(comptime T: type) !void {
    try T.init();
    defer T.deinit();
    try T.run();
}

fn isLabel(comptime field: anytype) bool {
    return switch (@typeInfo(@TypeOf(field))) {
        .pointer => |ptr| switch (@typeInfo(ptr.child)) {
            .array => field[0] == ':',
            else => false,
        },
        else => false,
    };
}

/// Extracts threaded function ops and their code positions from a tuple.
/// Labels (strings starting with ':') are skipped.
pub fn opsInfo(comptime tup: anytype) type {
    comptime var count: usize = 0;
    inline for (tup) |field| {
        if (!isLabel(field) and @TypeOf(field) == tf) count += 1;
    }

    return struct {
        pub const ops: [count]tf = blk: {
            var arr: [count]tf = undefined;
            var i: usize = 0;
            for (tup) |field| {
                if (!isLabel(field) and @TypeOf(field) == tf) {
                    arr[i] = field;
                    i += 1;
                }
            }
            break :blk arr;
        };
        pub const positions: [count]usize = blk: {
            var arr: [count]usize = undefined;
            var i: usize = 0;
            var pos: usize = 0;
            for (tup) |field| {
                if (!isLabel(field)) {
                    if (@TypeOf(field) == tf) {
                        arr[i] = pos;
                        i += 1;
                    }
                    pos += 1;
                }
            }
            break :blk arr;
        };
    };
}
