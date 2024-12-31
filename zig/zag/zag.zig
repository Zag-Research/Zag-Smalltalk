const std = @import("std");
const object = @import("zobject.zig");
const execute = @import("execute.zig");
const Object = object.Object;
const ThreadedFn = execute.ThreadedFn;
const primitives = @import("primitives.zig");
const globalArena = @import("globalArena.zig");
// grep -r ': *PC,.*: *SP,.*:.*Process,.*:.*Context,.*: *MethodSignature)' .|grep -v 'not embedded'|sed -nE 's;./\(.*\)[.]zig:[ \t]*pub fn \([^(]*\)(.*;"\2",\&\1.\2,;p'|sed 's;\(&[^/.]*\).;\1.embedded.;'|sed 's;@"\([^"]*\)";\1;'|sed '/^"p[0-9]/s/embedded[.][^.]*/primitives/'|sort
const references = [_]ThreadedFn{
    &execute.embedded.branch, // T_Branch := 1.
    &execute.embedded.call, // T_Call := 2.
    &execute.embedded.callRecursive, // T_CallRecursive := 3
    &execute.embedded.drop, // T_Drop := 4
    &execute.embedded.dropNext, // T_DropNext := 5
    &execute.embedded.dup, // T_Dup := 6
    &execute.embedded.dynamicDispatch, // T_DynamicDispatch := 7
    &execute.embedded.fallback, // T_Fallback := 8
    &execute.embedded.ifFalse, // T_IfFalse := 9
    &execute.embedded.ifNil, // T_IfNil := 10
    &execute.embedded.ifNotNil, // T_IfNotNil := 11
    &execute.embedded.ifTrue, // T_IfTrue := 12
    &execute.embedded.over, // T_Over := 13
    &execute.embedded.popLocal, // T_PopLocal := 14
    &execute.embedded.popLocal0, // T_PopLocal0 := 15
    &execute.embedded.popLocalData, // T_PopLocalData := 16
    &execute.embedded.popLocalField, // T_PopLocalField := 17
    &execute.embedded.printStack, // T_PrintStack := 18
    &execute.embedded.pushContext, // T_PushContext := 19
    &execute.embedded.pushLiteral, // T_PushLiteral := 20
    &execute.embedded.pushLiteral0, // T_PushLiteral0 := 21
    &execute.embedded.pushLiteral1, // T_PushLiteral1 := 22
    &execute.embedded.pushLiteral2, // T_PushLiteral2 := 23
    &execute.embedded.pushLiteralFalse, // T_PushLiteralFalse := 24
    &execute.embedded.pushLiteralIndirect, // T_PushLiteralIndirect := 25
    &execute.embedded.pushLiteralNil, // T_PushLiteralNil := 26
    &execute.embedded.pushLiteralTrue, // T_PushLiteralTrue := 27
    &execute.embedded.pushLiteral_1, // T_PushLiteral_1 := 28
    &execute.embedded.pushLocal, // T_PushLocal := 29
    &execute.embedded.pushLocal0, // T_PushLocal0 := 30
    &execute.embedded.pushLocalData, // T_PushLocalData := 31
    &execute.embedded.pushLocalField, // T_PushLocalField := 32
    &execute.embedded.pushThisContext, // T_PushThisContext := 33
    &execute.embedded.replaceLiteral, // T_ReplaceLiteral := 34
    &execute.embedded.replaceLiteral0, // T_ReplaceLiteral0 := 35
    &execute.embedded.replaceLiteral1, // T_ReplaceLiteral1 := 36
    &execute.embedded.returnNoContext, // T_ReturnNoContext := 37
    &execute.embedded.returnNonLocal, // T_ReturnNonLocal := 38
    &execute.embedded.returnTop, // T_ReturnTop := 39
    &execute.embedded.returnWithContext, // T_ReturnWithContext := 40
    &execute.embedded.setupSend, // T_SetupSend := 41
    &execute.embedded.setupTailSend, // T_SetupTailSend := 42
    &execute.embedded.setupTailSend0, // T_SetupTailSend0 := 43
    &execute.embedded.storeLocal, // T_StoreLocal := 44
    &execute.embedded.swap, // T_Swap := 45
    &execute.embedded.verifyMethod, // T_VerifyMethod := 46
};
// &primitives.embedded.BlockClosure.closureData, // P_ClosureData := 47
// &primitives.embedded.BlockClosure.fullClosure, // P_FullClosure := 48
// &primitives.embedded.BlockClosure.generalClosure, // P_GeneralClosure := 49
// &primitives.embedded.BlockClosure.immutableClosure, // P_ImmutableClosure := 50
// &primitives.embedded.BlockClosure.pushNonlocalBlock_false, // P_PushNonlocalBlock_false := 51
// &primitives.embedded.BlockClosure.pushNonlocalBlock_minusOne, // P_PushNonlocalBlock_minusOne := 52
// &primitives.embedded.BlockClosure.pushNonlocalBlock_nil, // P_PushNonlocalBlock_nil := 53
// &primitives.embedded.BlockClosure.pushNonlocalBlock_one, // P_PushNonlocalBlock_one := 54
// &primitives.embedded.BlockClosure.pushNonlocalBlock_self, // P_PushNonlocalBlock_self := 55
// &primitives.embedded.BlockClosure.pushNonlocalBlock_true, // P_PushNonlocalBlock_true := 56
// &primitives.embedded.BlockClosure.pushNonlocalBlock_two, // P_PushNonlocalBlock_two := 57
// &primitives.embedded.BlockClosure.pushNonlocalBlock_zero, // P_PushNonlocalBlock_zero := 58
// &primitives.primitives.p201, // P_P201 := 59
// &primitives.primitives.p202, // P_P202 := 60
// &primitives.primitives.p203, // P_P203 := 61
// &primitives.primitives.p204, // P_P204 := 62
// &primitives.primitives.p205, // P_P205 := 63
// &primitives.embedded.BlockClosure.@"value:", // P_value_ := 64
// &execute.embedded.perform, // P_Perform := 65
// &execute.embedded.performWith, // P_PerformWith := 66

fn format_timestamp(seconds: u64, out_buffer: []u8) void {
    if (std.debug.runtime_safety) std.debug.assert(out_buffer.len >= 27);
    const epoch_seconds: std.time.epoch.EpochSeconds = .{ .secs = seconds };
    const epoch_day = epoch_seconds.getEpochDay();
    const day_seconds = epoch_seconds.getDaySeconds();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    // Format the timestamp as "YYYY-MM-DD, HH:MM:SS (UTC)"
    _ = std.fmt.bufPrint(out_buffer, "{d}-{d:0>2}-{d:0>2}, {d:0>2}:{d:0>2}:{d:0>2} (UTC) ", .{
        year_day.year,
        month_day.month.numeric(),
        month_day.day_index + 1,
        day_seconds.getHoursIntoDay(),
        day_seconds.getMinutesIntoHour(),
        day_seconds.getSecondsIntoMinute(),
    }) catch unreachable;
}
const unix_timestamp: u64 = @intCast(std.time.timestamp());
const timestamp: []const u8 = blk: {
    var buffer: [27]u8 = undefined; // Buffer size increased to 27 bytes
    format_timestamp(unix_timestamp, &buffer);
    break :blk &buffer;
};

const ZagImageHeader = struct {
    magic: u64,
    target: Object,
    selector: Object,
    classTable: Object,
    symTable: Object,
    codeAddresses: Object,
    const magicTag: u64 = 0x6567616D4967615A; // "ZagImage" in little-endian
};
var zagImageHeader: ZagImageHeader = undefined;
fn usage() void {
    std.debug.print(
        \\Usage: zig image-directory
        \\
    , .{});
}
fn extensionMatches(name: []const u8, ext: []const u8) bool {
    if (name.len <= ext.len) return false;
    for (name[name.len - ext.len ..], ext) |n, e| {
        if (n != e) return false;
    }
    return true;
}
fn parseAddress(name: []const u8) u64 {
    var index = name.len;
    var address: u64 = 0;
    var seenDot = false;
    var shift:u6 = 0;
    while (index > 0) {
        index -= 1;
        shift += 4;
        switch (name[index]) {
            '0'...'9' => |c| address += @as(u64,c - '0') << (shift - 4),
            'a'...'f' => |c| address += @as(u64,c - 'a' + 10) << (shift - 4),
            'A'...'F' => |c| address += @as(u64,c - 'A' + 10) << (shift - 4),
            '.' => {
                seenDot = true;
                address = 0;
                shift = 0;
            },
            else => if (seenDot) break,
        }
    }
    return address;
}
fn checkHeader(file: std.fs.File) !void {
    const stat = try file.stat();
    std.debug.print("header stat: {}\n", .{stat});
    if (stat.size < @sizeOf(ZagImageHeader) or (stat.size & 7) != 0)
        return error.WrongImageFileSize;
    _ = try file.read(@as([*]u8, @ptrCast(&zagImageHeader))[0..@sizeOf(ZagImageHeader)]);
    if (zagImageHeader.magic != ZagImageHeader.magicTag)
        return error.BadImageMagic;
}
fn processHeader(file: std.fs.File) !void {
    _ = .{file};
    @panic("not implemented");
}
fn runImage() !void {
    @panic("not implemented");
}
fn readHeap(file: std.fs.File, address: u64) !void {
    const stat = try file.stat();
    std.debug.print("heap stat: {}\naddress: 0x{x}\n", .{ stat, address });
    try globalArena.HeapAllocation.loadHeap(file, address);
}
fn readLargeHeapObject(file: std.fs.File, address: u64) !void {
    _ = .{ file, address };
    @panic("not implemented");
}
fn readProcess(file: std.fs.File, address: u64) !void {
    _ = .{ file, address };
    @panic("not implemented");
}
fn loadAndRun(directory: [*:0]const u8) !void {
    var dir = try std.fs.cwd().openDirZ(directory, .{});
    defer dir.close();
    {
        var headerFile: ?std.fs.File = null;
        defer if (headerFile) |file| file.close();
        var it = dir.iterate();
        while (try it.next()) |entry| {
            const name = entry.name;
            if (entry.kind == .file) {
                if (extensionMatches(name, ".header")) {
                    headerFile = try dir.openFile(name, .{});
                    try checkHeader(headerFile.?);
                } else {
                    const file = try dir.openFile(name, .{});
                    defer file.close();
                    const address = parseAddress(name);
                    if (extensionMatches(name, ".heap")) {
                        try readHeap(file, address);
                    } else if (extensionMatches(name, ".lho")) {
                        try readLargeHeapObject(file, address);
                    } else if (extensionMatches(name, ".process")) {
                        try readProcess(file, address);
                    } else {
                        std.debug.print("unknown file: {s}\n", .{name});
                        return error.UnknownFile;
                    }
                }
            } else {
                std.debug.print("unknown non-file: {s}\n", .{name});
                return error.UnknownNonFile;
            }
        }
        try processHeader(headerFile.?);
    }
    try runImage();
}
pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var argsIterator = try std.process.ArgIterator.initWithAllocator(allocator);
    defer argsIterator.deinit();

    _ = argsIterator.next(); // Skip executable

    if (argsIterator.next()) |arg| {
        if (std.mem.orderZ(u8, arg, "-h") == .eq or std.mem.orderZ(u8, arg, "--help") == .eq) {
            usage();
        } else {
            try loadAndRun(arg);
        }
        while (argsIterator.next()) |extra|
            std.debug.print(
                \\unused argument: {s}
                \\
            , .{extra});
    } else {
        usage();
    }
}
