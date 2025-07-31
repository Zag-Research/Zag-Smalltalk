const std = @import("std");
const zag = @import("zag.zig");
const assert = std.debug.assert;
const config = zag.config;
const InMemory = zag.inMemory;
const object = zag.object;
const Object = object.Object;
const execute = zag.execute;
const Context = zag.context;
const Process = zag.process;
const heap = zag.heap;
const globalArena = zag.globalArena;
const symbol = zag.symbol;
const utilities = zag.utilities;
const threadedFn = zag.threadedFn;
const llvm = zag.llvm;

const references = execute.ebmedded.references;
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
        \\Usage: zag image-directory
        \\
    , .{});
}
fn version() void {
    std.debug.print("{s}\n", .{config.git_version});
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
    var shift: u6 = 0;
    while (index > 0) {
        index -= 1;
        shift += 4;
        switch (name[index]) {
            '0'...'9' => |c| address += @as(u64, c - '0') << (shift - 4),
            'a'...'f' => |c| address += @as(u64, c - 'a' + 10) << (shift - 4),
            'A'...'F' => |c| address += @as(u64, c - 'A' + 10) << (shift - 4),
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
    //std.debug.print("header stat: {}\n", .{stat});
    if (stat.size < @sizeOf(ZagImageHeader) or (stat.size & 7) != 0)
        return error.WrongImageFileSize;
    _ = try file.read(@as([*]u8, @ptrCast(&zagImageHeader))[0..@sizeOf(ZagImageHeader)]);
    if (zagImageHeader.magic != ZagImageHeader.magicTag)
        return error.BadImageMagic;
}
fn loadSymbols() !void {
    var exportedSymbols = zagImageHeader.symTable;
    outer: while (!exportedSymbols.isNil()) {
        for (try exportedSymbols.arrayAsSlice(Object)) |obj| {
            // if (obj.isString()) {
            //     std.debug.print("LoadingSymbol: {}\n", .{symbol.intern(obj)});
            // } else {
            //     exportedSymbols = obj;
            //     continue :outer;
            // }
            _ = obj;
            continue :outer;
        }
        break;
    }
}
fn loadClassTable() !void {
    assert(zagImageHeader.classTable == object.Nil);
}
fn loadDispatchTable(file: std.fs.File) !void {
    execute.loadIntrinsicsDispatch();
    const stat = try file.stat();
    assert(stat.size == @sizeOf(ZagImageHeader)); // no dispatch to read
    //    _ = references;
}
fn loadCodeAddresses() !void {
    const threadedFunctions = zag.threadedFn.functions;
    const Element = struct {
        files: u64,
        ours: u64,
    };
    var map: [threadedFunctions.len]Element = undefined;
    const codeAddresses = try zagImageHeader.codeAddresses.arrayAsSlice(u64);
    if (threadedFunctions.len != codeAddresses.len)
        std.debug.print("my primitives length: {} file:{}\n", .{ threadedFunctions.len, codeAddresses.len });
    for (&map, &threadedFunctions, codeAddresses) |*element, o, f| {
        element.files = f;
        element.ours = @intFromPtr(o.f);
    }
}
fn processHeader(file: std.fs.File) !void {
    //std.debug.print("Zag header: {}\n",.{zagImageHeader});
    try loadSymbols();
    try loadCodeAddresses();
    try loadDispatchTable(file);
    try loadClassTable();
}
fn runImage() !void {
    _ = try execute.mainSendTo(zagImageHeader.selector, zagImageHeader.target);
}
fn readHeap(file: std.fs.File, address: u64) !void {
    //const stat = try file.stat();std.debug.print("heap stat: {}\naddress: 0x{x}\n", .{ stat, address });
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
        if (std.mem.orderZ(u8, arg, "-v") == .eq or std.mem.orderZ(u8, arg, "--version") == .eq) {
            version();
        } else if (std.mem.orderZ(u8, arg, "-h") == .eq or std.mem.orderZ(u8, arg, "--help") == .eq) {
            usage();
        } else {
            _ = @import("controlWords.zig").drop.threadedFn;
            // try loadAndRun(arg);
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
