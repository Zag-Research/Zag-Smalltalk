const std = @import("std");
const lastPredefinedSymbol = 53;
const staticSymbols = blk: {
    var symbolArray = [_]PointedObject{undefined} ** lastPredefinedSymbol;
    for (symbolArray[0..]) |*sym|
        initSymbol(sym);
    break :blk symbolArray;
};
fn initSymbol(sym: *PointedObject) void {
    sym.header = HeapHeader{ .classIndex = .Symbol, .hash = 42, .format = .notIndexable, .age = .static, .length = 1 };
}
inline fn symbol_of(index: u24, _: u4) Object {
    return Object{ .ref = @ptrCast(&staticSymbols[index - 1]) };
}
// inline
fn symbol0(index: u24) Object {
    return symbol_of(index, 0);
}
inline fn symbolX(index: u24) Object {
    return symbol_of(index, 0);
}
const symbols = struct {
    const value = symbol0(2);
};
test "make it run" {
    std.debug.print("All three of these should be the same:\n", .{});
    std.debug.print("the compile-time value:     {}\n", .{symbols.value.raw()});
    std.debug.print("the run-time value:         {}\n", .{symbol0(2).raw()});
    std.debug.print("not inlined run-time value: {}\n", .{symbolX(2).raw()});
}
const Object = packed struct {
    ref: *const u64,
    fn raw(self: Object) u64 {
        return @bitCast(self);
    }
};
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
