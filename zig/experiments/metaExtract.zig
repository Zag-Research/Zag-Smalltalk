const builtin = @import("builtin");
const std = @import("std");

fn Extract(funcName: [:0]const u8, M: type) type {
    var fields = @typeInfo(struct {}).@"struct".fields;
    for (@typeInfo(M).@"struct".decls) |d| {
        const func = @field(@field(M, d.name), funcName);
        const F = @TypeOf(func);
        fields = fields ++ [_]std.builtin.Type.StructField{.{
            .name = d.name,
            .type = F,
            .default_value = func,
            .is_comptime = true,
            .alignment = @alignOf(F),
        }};
    }
    return @Type(.{ .@"struct" = .{
        .layout = .auto,
        .is_tuple = false,
        .fields = fields,
        .decls = &.{},
    } });
}

const combined = struct {
    pub const struct1 = struct {
        pub fn func() void {
            std.log.err("Hello, struct1\n", .{});
        }
    };
    pub const struct2 = struct {
        pub fn func() void {
            std.log.err("Goodbye, struct2\n", .{});
        }
    };
};

const Foo = Extract("func", combined){};

pub fn main() !void {
    Foo.struct1();
    Foo.struct2();
}
