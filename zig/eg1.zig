const expect = @import("std").testing.expect;
const assert = @import("std").debug.assert;
const stdout = @import("std").io.getStdOut().writer();
const Symbol = struct {
    usingnamespace @import("symbol.zig");
    const add = @This().symbol_of(42,0);
};
const A = @import("ast.zig");
const from = A.from;
const Object = A.Object;
const returnE = A.returnE;
const NEGATIVE_INF = A.NEGATIVE_INF;

test "import whole namespace" {
    try stdout.print("Hello, 0x{x}!\n", .{NEGATIVE_INF});
    try stdout.print("Hello, {}!\n", .{Symbol.yourself});
    try stdout.print("Hello, {}!\n", .{Symbol.add});
    try stdout.print("Hello, {}!\n", .{from(3.14)});
    try stdout.print("Hello, {}!\n", .{from(42)});
    try stdout.print("Hello, {}!\n", .{from(-17)});
}

fn test1(stack : [*]Object, heap : [*]Object) returnE {
    assert(@ptrToInt(stack)>@ptrToInt(heap));
    return .Normal;
}

test "run test1" {
    A.thread = A.threadT.init();
    try expect(test1(A.thread.stack,A.thread.heap)==.Normal);
}
