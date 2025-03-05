const std = @import("std");
const assert = std.debug.assert;
const zag = @import("zag.zig");
const object = zag.object;
const Object = object.Object;
const execute = zag.execute;
const ThreadedFn = execute.ThreadedFn;
const primitives = zag.primitives;
const globalArena = zag.globalArena;
const symbol = zag.symbol;

pub const structures = struct {
    pub usingnamespace @import("controlWords.zig");
};
pub const branch = structures.branch.threadedFn;

fn Enum(funcName: [:0]const u8, M: type) type {
    var fields = @typeInfo(enum{}).@"enum".fields;
    comptime var n = 0;
    for (@typeInfo(M).@"struct".decls) |d| {
        const ds = @field(M,d.name);
        switch (@TypeOf(ds)) {
            type => {
                if (@hasDecl(ds,funcName)) {
                    fields = fields ++ [_]std.builtin.Type.EnumField{.{
                        .name = d.name,
                        .value = n,
                    }};
                    n += 1;
                }
            },
            else => {},
        }
    }
    return @Type(.{ .@"enum" = .{
        .tag_type = usize,
        .is_exhaustive = false,
        .fields = fields,
        .decls = &.{},
    } });
}

const ThreadedFnEnum = Enum("threadedFn",structures);

fn countStructs(funcName: [:0]const u8, M: type) comptime_int {
    comptime var n = 0;
    for (@typeInfo(M).@"struct".decls) |d| {
        const ds = @field(M,d.name);
        switch (@TypeOf(ds)) {
            type => {
                if (@hasDecl(ds,funcName)) {
                    n += 1;
                }
            },
            else => {},
        }
    }
    return n;
}

fn extractFunctions(comptime funcName: [:0]const u8, M: type) [countStructs(funcName,M)]ThreadedFn.Fn {
    return blk: {
        var array: [countStructs(funcName,M)]ThreadedFn.Fn = undefined;
        var n = 0;
        for (@typeInfo(M).@"struct".decls) |d| {
            const ds = @field(M,d.name);
            switch (@TypeOf(ds)) {
                type => {
                    if (@hasDecl(ds,funcName)) {
                        array[n] = &@field(ds,funcName);
                        n += 1;
                    }
                },
                else => {},
            }
        }
        break :blk array;
    };
}

pub const functions = extractFunctions("threadedFn",structures);
fn f(key: ThreadedFnEnum) ThreadedFn.Fn {
    return functions[@intFromEnum(key)];
}

comptime {
    assert(branch == f(.branch));
    assert(functions.len == @typeInfo(ThreadedFnEnum).@"enum".fields.len);
}
    
                                    
