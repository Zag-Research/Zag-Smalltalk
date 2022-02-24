const expect = @import("std").testing.expect;
const assert = @import("std").debug.assert;
const stdout = @import("std").io.getStdOut().writer();
const Symbol = struct {
    usingnamespace @import("symbol.zig");
    const add = @This().symbol_of(42,0);
    const add_ = @This().symbol_of(43,1);
};
const A = @import("ast.zig");
const from = A.from;
const Object = A.Object;
const returnE = A.returnE;
const Nil = A.Nil;
const NEGATIVE_INF = A.NEGATIVE_INF;

test "import whole namespace" {
    try stdout.print("-inf, 0x{x}!\n", .{NEGATIVE_INF});
    try stdout.print("yourself, {}!\n", .{Symbol.yourself});
    try stdout.print("add, {}!\n", .{Symbol.add});
    try stdout.print("add_, {}!\n", .{Symbol.add_});
    try stdout.print("3.14, {}!\n", .{from(3.14)});
    try stdout.print("1.0, {}!\n", .{from(1.0)});
    try stdout.print("2.0, {}!\n", .{from(2.0)});
    try stdout.print("42, {}!\n", .{from(42)});
    try stdout.print("-17, {}!\n", .{from(-17)});
    try stdout.print("false, {}!\n", .{from(false)});
    try stdout.print("true, {}!\n", .{from(true)});
    try stdout.print("Nil, {}!\n", .{Nil});
}

fn test1(stack : [*]Object, heap : [*]Object) returnE {
    assert(@ptrToInt(stack)>@ptrToInt(heap));
    return .Normal;
}

test "run test1" {
    A.thread = A.threadT.init();
    try expect(test1(A.thread.stack,A.thread.heap)==.Normal);
}

test "hashes" {
    const bigPrime : u64 = 16777213;//4294967291;
    const mod : u64 = 128;
    try stdout.print("42 {}\n",.{@bitCast(u64,from(42))%bigPrime%mod});
    try stdout.print("-17 {}\n",.{@bitCast(u64,from(-17))%bigPrime%mod});
    try stdout.print("3.14 {}\n",.{@bitCast(u64,from(3.14))%bigPrime%mod});
    try stdout.print("1.0 {}\n",.{@bitCast(u64,from(1.0))%bigPrime%mod});
    try stdout.print("2.0 {}\n",.{@bitCast(u64,from(2.0))%bigPrime%mod});
    try stdout.print("true {}\n",.{@bitCast(u64,from(true))%bigPrime%mod});
    try stdout.print("false {}\n",.{@bitCast(u64,from(false))%bigPrime%mod});
    try stdout.print("nil {}\n",.{@bitCast(u64,Nil)%bigPrime%mod});
    try expect(@bitCast(u64,Nil)%bigPrime%mod != @bitCast(u64,from(false))%bigPrime%mod);
}
