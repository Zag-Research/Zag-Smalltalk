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

const structures = .{
    @import("controlWords.zig"),
    @import("dispatch.zig").threadedFunctions,
    @import("primitives.zig").threadedFunctions,
    @import("context.zig").threadedFunctions,
    @import("process.zig").threadedFunctions,
    if (is_test) struct {
        // these are just for testing to  verify that we can filter them out
        // pub const T = u32; // don't know how to filter these out
        const ignoreCTInt = 42;
        const ignoreInt: usize = 42;
        fn ignore() void {}
    } else struct {},
} ++ @import("primitives.zig").primitiveThreadedFunctions;
//comptime {@compileLog(structures[6].asThunk);}
fn declsCount() usize {
    comptime var count = 0;
    for (structures) |structure| count += @typeInfo(structure).@"struct".decls.len;
    return count;
}
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
    threadedFn: ThreadedFn.Fn,
};
const addUnrecognized = true;
const showThreadedFns = false;
const enumAndFunctions =
    blk: {
        @setEvalBranchQuota(100000);
        var array: [declsCount()]EnumSort = undefined;
        var n = 0;
        for (structures) |structure| {
            if (showThreadedFns) @compileLog(structure);
            const decls = @typeInfo(structure).@"struct".decls;
            for (decls) |decl| {
                const ds = @field(structure, decl.name);
                switch (@typeInfo(@TypeOf(ds))) {
                    .comptime_int, .int, .@"fn", .array => {},
                    else => {
                        if (@hasDecl(ds, "threadedFn")) {
                            if (showThreadedFns and !(@hasDecl(ds,"forTests") and @field(ds,"forTests"))) @compileLog(decl.name);
                            if (@hasDecl(ds, "order")) {
                                array[n] = .{ .field = &decl, .order = @field(ds, "order"), .threadedFn = @field(ds, "threadedFn") };
                            } else array[n] = .{ .field = &decl, .order = 0, .threadedFn = @field(ds, "threadedFn") };
                            n += 1;
                        }
                    },
                }
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
        if (addUnrecognized) {
            fields = fields ++ [_]std.builtin.Type.EnumField{.{
                .name = "_end",
                .value = fields.len,
            }};
            fields = fields ++ [_]std.builtin.Type.EnumField{.{
                .name = "Unrecognized",
                .value = 9999,
            }};
        }
        const arraySize = enums.len + if (addUnrecognized) 1 else 0;
        var arrayFns: [arraySize]ThreadedFn.Fn = undefined;
        for (enums, 0..) |eb, index| {
            arrayFns[index] = eb.threadedFn;
        }
        if (addUnrecognized) {
            arrayFns[arraySize - 1] = &execute.Code.end;
        }

        break :blk .{ @Type(.{ .@"enum" = .{
            .tag_type = usize,
            .is_exhaustive = false,
            .fields = fields,
            .decls = &.{},
        } }), arrayFns };
    };

pub const Enum = enumAndFunctions[0];
const functions = enumAndFunctions[1];

// test "print threadedFns" {
//     for (@typeInfo(Enum).@"enum".fields) |field| {
//         std.debug.print("{s:<25}", .{field.name});
//     }
//     std.debug.print("\n", .{});
// }

pub fn initialize() void {}
pub fn threadedFn(key: Enum) ThreadedFn.Fn {
    return functions[@intFromEnum(key)];
}
pub fn find(f: ThreadedFn.Fn) Enum {
    for (&functions, 0..) |func, index| {
        if (func == f) return @enumFromInt(index);
    }
    return .Unrecognized;
}
comptime {
    assert(@import("controlWords.zig").branch.threadedFn == threadedFn(.branch));
}
test "number of threaded functions" {
    if (true) return error.SkipZigTest;
    expectEqual(43, functions.len) catch |err| {
        inline for (std.meta.fields(Enum), 0..) |f, i| {
            std.debug.print("{s:<25}", .{f.name});
            if (i < functions.len) {
                std.debug.print("{x:0>16}\n", .{@intFromPtr(functions[i])});
            } else std.debug.print("?\n", .{});
        }
        return err;
    };
}
// test "test list" {
//     for (@import("builtin").test_functions) |f| {
//         std.debug.print("tests: {s}\n",.{f.name});
//     }
// }
