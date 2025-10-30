const std = @import("std");
const lastPredefinedSymbol = 53;
const staticSymbols = blk: {
    var symbolArray = [_]PointedObject{undefined} ** lastPredefinedSymbol;
    for (symbolArray[0..]) |*sym|
        initSymbol(sym);
    break :blk symbolArray;
};
fn initSymbol(sym: *PointedObject) void {
    sym.header = .{
        //    sym.header = HeapHeader{
        .classIndex = .Symbol,
        .hash = 42,
        .format = .notIndexable,
        .age = .static,
        .length = 1,
    };
}
test "make it run" {
    std.log.err("in test {}\n", .{&staticSymbols[0]});
}
const PointedObject = packed struct {
    header: HeapHeader,
    data: packed union {
        int: i64,
        unsigned: u64,
    },
};
const HeapHeader = packed struct(u64) {
    classIndex: ClassIndex = .none,
    hash: u24 = 0,
    format: Format = .free,
    immutable: bool = false,
    age: Age = .nursery,
    length: u11 = 0,
    forwarded: bool = false,
};
const Age = enum(u4) {
    nursery,
    static,
};
const Format = enum(u7) {
    notIndexable, // this is an Object with no indexable values
    free,
};
const ClassIndex = enum(u16) {
    none = 0,
    Symbol,
};
