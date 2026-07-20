const std = @import("std");
const EnumField = std.builtin.Type.EnumField;

pub fn DeriveEnum(
    comptime BaseEnum: type,
    comptime tag_type: type,
    offset: usize,
    comptime blocks: anytype,
) type {
    @setEvalBranchQuota(10000);
    const base_info = @typeInfo(BaseEnum).@"enum";
    const base_fields = base_info.fields;

    // 1. Calculate maximum possible fields assuming zero duplicates
    comptime var max_extra: usize = 0;
    comptime var max_value: usize = 0;
    for (base_fields) |f| {
        max_value = @max(max_value, f.value + offset);
    }
    inline for (blocks) |block| {
        max_extra += block.names.len;
        max_value = @max(max_value, block.base + block.names.len);
    }
    const max_fields_count = base_fields.len + max_extra;

    comptime var used = [_]bool{false} ** max_value;
    var out_fields: [max_fields_count]EnumField = undefined;
    var i: usize = 0;

    // 2. Populate the starting array with the base enum fields
    for (base_fields) |f| {
        out_fields[i] = EnumField{
            .name = f.name,
            .value = f.value + offset,
        };
        used[f.value + offset] = true;
        i += 1;
    }

    // 3. Process each configuration block
    inline for (blocks) |block| {
        var current_val = block.base;

        names: inline for (block.names) |name| {
            // Check if the name has already been defined (in BaseEnum or prior blocks)
            var name_exists = false;
            for (base_fields) |f| {
                if (std.mem.eql(u8, f.name, name)) {
                    name_exists = true;
                    continue :names;
                }
            }

            // Find the lowest available value >= current_val
            while (used[current_val]) {
                current_val += 1;
            }

            out_fields[i] = EnumField{
                .name = name,
                .value = current_val,
            };
            used[current_val] = true;
            i += 1;
            current_val += 1;
        }
    }

    // 4. Shrink to a precisely-sized array for final reification
    const final_count = i;
    var final_fields: [final_count]EnumField = undefined;
    for (0..final_count) |idx| {
        final_fields[idx] = out_fields[idx];
    }

    return @Type(.{
        .@"enum" = .{
            .tag_type = tag_type,
            .fields = &final_fields,
            .decls = &[_]std.builtin.Type.Declaration{},
            .is_exhaustive = base_info.tag_type == tag_type and base_info.is_exhaustive,
        },
    });
}
const Status = enum(u32) {
    ok = 200,
    bad_request = 400,
    unauthorized = 401,
};

pub fn main() !void {
    const ExtendedStatus = DeriveEnum(Status, u32, .{
        // Block 1: Low-range values
        .{
            .base = 0,
            .names = .{ "teapot", "continue_processing", "ok" },
        },
        // Block 2: High-range values (Deliberate collision test at 401)
        .{
            .base = 401,
            .names = .{ "payment_required", "forbidden", "teapot" },
        },
    });

    std.debug.print("--- Generalized Enum Layout ---\n", .{});
    std.debug.print("teapot:              {}\n", .{@intFromEnum(ExtendedStatus.teapot)});
    std.debug.print("continue_processing: {}\n", .{@intFromEnum(ExtendedStatus.continue_processing)});
    std.debug.print("ok:                  {}\n", .{@intFromEnum(ExtendedStatus.ok)});
    std.debug.print("bad_request:         {}\n", .{@intFromEnum(ExtendedStatus.bad_request)});
    std.debug.print("unauthorized:        {}\n", .{@intFromEnum(ExtendedStatus.unauthorized)});
    std.debug.print("payment_required:    {}\n", .{@intFromEnum(ExtendedStatus.payment_required)});
    std.debug.print("forbidden:           {}\n", .{@intFromEnum(ExtendedStatus.forbidden)});
}
// Note the block base can be calculated
// .{
//    .base = @typeInfo(Status).@"enum".fields[@typeInfo(Status).@"enum".fields.len - 1].value + 1,
//    .names = .{ "payment_required", "forbidden" },
// }
