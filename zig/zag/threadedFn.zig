const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const zag = @import("zag.zig");
const is_test = zag.config.is_test;
const object = zag.object;
const Object = object.Object;
const execute = zag.execute;
const ThreadedFn = execute.ThreadedFn;
const primitives = zag.primitives;
const globalArena = zag.globalArena;
const symbol = zag.symbol;

// All the structs that we import will contain a sequence of
// structs that each represent one threaded function
// these are identified by having a structure like:
// pub const branch = struct {
//    pub const order = 0;
//    pub fn threadedFn(pc: PC, sp: SP, ...
// }
// the function named "threadedFn" is the word to use in the
// threaded portion of a CompiledMethod
// the declaration "order" is optional and only needed if
// the order in the codeAddress array needs to be changed.
// The default is 0, and any other value should be like 20250305
// to represent the date it was added.
// This has to match the <order:> pragma in the Smalltalk code
// so the codeAddress arrays remain consistent. All the structures
// with the same "order" will be sorted alphabetically.
// As well as these required structure, the struct will typically
// contain any support fn/const/var declarations as well as tests.
// Any other public declarations in the import will be ignored.

const structures = struct {
    pub usingnamespace @import("controlWords.zig");
    pub usingnamespace @import("primitives.zig");
    pub usingnamespace @import("context.zig").threadedFunctions;
    pub usingnamespace @import("process.zig").threadedFunctions;
    pub usingnamespace if (is_test) struct {
        // these are just for testing to  verify that we can filter them out
        // pub const T = u32; // don't know how to filter these out
        pub const ignoreCTInt = 42;
        pub const ignoreInt: usize = 42;
        pub fn ignore() void {}
    } else struct {};
};

fn enumLessThan(_: void, lhs: EnumSort, rhs: EnumSort) bool {
    switch (std.math.order(lhs.order, rhs.order)) {
        .eq => return std.mem.lessThan(u8, lhs.field.name, rhs.field.name),
        .lt => return true,
        else => return false,
    }
}
const EnumSort = struct {
    field: *const std.builtin.Type.Declaration,
    order: usize,
};
fn hasFn(comptime T: type, comptime name: []const u8) bool {
    switch (@typeInfo(T)) {
        .@"struct", .@"union", .@"enum", .@"opaque" => {},
        else => return false,
    }
    if (!@hasDecl(T, name))
        return false;

    return @typeInfo(@TypeOf(@field(T, name))) == .@"fn";
}
pub const Enum =
    blk: {
    const decls = @typeInfo(structures).@"struct".decls;
    var array: [decls.len]EnumSort = undefined;
    var n = 0;
    for (decls) |decl| {
        const ds = @field(structures, decl.name);
        switch (@typeInfo(@TypeOf(ds))) {
            .comptime_int, .int, .@"fn", .array => {},
            else => {
                if (@hasDecl(ds, "threadedFn")) {
                    if (@hasDecl(ds, "order")) {
                        array[n] = .{ .field = &decl, .order = @field(ds, "order") };
                    } else array[n] = .{ .field = &decl, .order = 0 };
                    n += 1;
                }
            },
        }
    }
    const enums = array[0..n];
    std.mem.sort(EnumSort, enums, {}, enumLessThan);
    var fields = @typeInfo(enum {}).@"enum".fields;
    for (enums, 0..) |d, i| {
        fields = fields ++ [_]std.builtin.Type.EnumField{.{
            .name = d.field.name,
            .value = i,
        }};
    }
    //@compileLog(fields);
    break :blk @Type(.{ .@"enum" = .{
        .tag_type = usize,
        .is_exhaustive = false,
        .fields = fields,
        .decls = &.{},
    } });
};

const functions =
    blk: {
    var array: [@typeInfo(Enum).@"enum".fields.len]ThreadedFn.Fn = undefined;
    for (@typeInfo(Enum).@"enum".fields) |d| {
        const ds = @field(structures, d.name);
        array[d.value] = &@field(ds, "threadedFn");
    }
    break :blk array;
};

pub fn initialize() void {}
pub fn threadedFn(key: Enum) ThreadedFn.Fn {
    return functions[@intFromEnum(key)];
}

comptime {
    assert(structures.branch.threadedFn == threadedFn(.branch));
}
test "function size" {
    expectEqual(43, functions.len) catch |err| {
        inline for (std.meta.fields(Enum)) |f| {
            std.debug.print("{s}\n", .{f.name});
        }
        return err;
    };
}
// test "test list" {
//     for (@import("builtin").test_functions) |f| {
//         std.debug.print("tests: {s}\n",.{f.name});
//     }
// }
