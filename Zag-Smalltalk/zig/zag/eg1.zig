const expect = @import("std").testing.expect;
const assert = @import("std").debug.assert;
const stdout = @import("std").io.getStdOut().writer();
const Symbol = struct {
    usingnamespace @import("symbol.zig");
    const add = @This().symbol_of(42, 0);
    const add_ = @This().symbol_of(43, 1);
};
const O = @import("object.zig");
const Nil = O.Nil;
const Object = O.Object;
const Dispatch = @import("dispatch.zig");
const returnE = Dispatch.returnE;
const Process = @import("process.zig");
const NEGATIVE_INF = @as(u64, 0xfff0000000000000);

test "printing objects" {
    const from = Object.from;
    try stdout.print("-inf, 0x{x} {}\n", .{ NEGATIVE_INF, @as(f64, @bitCast(NEGATIVE_INF)) });
    const x = O.Header{ .numSlots = 17, .format = 10, .hash = 0x123, .classIndex = 35 };
    try stdout.print("ptr, {} {}\n", .{ from(&x), from(&x).fullHash() });
    //    try stdout.print("ptr deref, {}\n", .{from(&x).as_pointer().*});
    try stdout.print("yourself, {} {}\n", .{ Symbol.yourself, Symbol.yourself.fullHash() });
    try stdout.print("add, {} {}\n", .{ Symbol.add, Symbol.add.fullHash() });
    try stdout.print("add_, {} {}\n", .{ Symbol.add_, Symbol.add_.fullHash() });
    try stdout.print("3.14, {} {}\n", .{ @as(f64, @bitCast(from(3.14))), from(3.14).fullHash() });
    try stdout.print("1.0, {} {}\n", .{ @as(f64, @bitCast(from(1.0))), from(1.0).fullHash() });
    try stdout.print("2.0, {} {}\n", .{ @as(f64, @bitCast(from(2.0))), from(2.0).fullHash() });
    try stdout.print("42, {} {}\n", .{ from(42), from(42).fullHash() });
    try stdout.print("-17, {} {}\n", .{ from(-17), from(-17).fullHash() });
    try stdout.print("false, {} {}\n", .{ from(false), from(false).fullHash() });
    try stdout.print("true, {} {}\n", .{ from(true), from(true).fullHash() });
    try stdout.print("Nil, {} {}\n", .{ Nil, Nil.fullHash() });
}

fn test1(stack: [*]Object, heap: [*]Object) returnE {
    assert(@intFromPtr(stack) > @intFromPtr(heap));
    return .Normal;
}

//test "run test1" {
//    var process = Process.Process.init();
//    try expect(test1(process.stack,process.heap)==.Normal);
//}

test "hashes" {
    const from = Object.from;
    const bigPrime: u64 = 16777213; //4294967291;
    const mod: u64 = 128;
    try stdout.print("42 {}\n", .{from(42).u() % bigPrime % mod});
    try stdout.print("-17 {}\n", .{from(-17).u() % bigPrime % mod});
    try stdout.print("3.14 {}\n", .{from(3.14).u() % bigPrime % mod});
    try stdout.print("1.0 {}\n", .{from(1.0).u() % bigPrime % mod});
    try stdout.print("2.0 {}\n", .{from(2.0).u() % bigPrime % mod});
    try stdout.print("true {}\n", .{from(true).u() % bigPrime % mod});
    try stdout.print("false {}\n", .{from(false).u() % bigPrime % mod});
    try stdout.print("nil {}\n", .{Nil.u() % bigPrime % mod});
    try expect(Nil.u() % bigPrime % mod != from(false).u() % bigPrime % mod);
}
