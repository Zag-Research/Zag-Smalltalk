const std = @import("std");
const execute = @import("execute.zig");
const primitives = @import("primitives.zig");
const object = @import("zobject.zig");
const Object = object.Object;
const ThreadedFn = execute.ThreadedFn;
const Reference = struct {
    str: []const u8,
    addr: ThreadedFn,
};
pub fn compileReferences(comptime tup: anytype) [tup.len / 2]Reference {
    @setEvalBranchQuota(3000);
    comptime var result: [tup.len / 2]Reference = undefined;
    inline for (tup, 0..) |field, idx| {
        if (idx & 1 == 0) {
            result[idx / 2].str = comptime field[0..];
        } else result[idx / 2].addr = field;
    }
    const final = result;
    return final;
}
const references = compileReferences(.{
    // grep -r ': *PC,.*: *SP,.*:.*Process,.*:.*Context,.*: *MethodSignature)' .|grep -v 'not embedded'|sed -n 's;./\(.*\)[.]zig:[ 	]*pub fn \([^(]*\)(.*;"\2",\&\1.\2,;p'|sed 's;\(&[^/.]*\).;\1.embedded.;'|sed 's;@"\([^"]*\)";\1;'|sed '/^"p[0-9]/s/embedded[.][^.]*/primitives/'|sort
    "branch",                     &execute.embedded.branch,
    "call",                       &execute.embedded.call,
    "callRecursive",              &execute.embedded.callRecursive,
    "closureData",                &primitives.embedded.BlockClosure.closureData,
    "drop",                       &execute.embedded.drop,
    "dropNext",                   &execute.embedded.dropNext,
    "dup",                        &execute.embedded.dup,
    "dynamicDispatch",            &execute.embedded.dynamicDispatch,
    "fallback",                   &execute.embedded.fallback,
    "fullClosure",                &primitives.embedded.BlockClosure.fullClosure,
    "generalClosure",             &primitives.embedded.BlockClosure.generalClosure,
    "ifFalse",                    &execute.embedded.ifFalse,
    "ifNil",                      &execute.embedded.ifNil,
    "ifNotNil",                   &execute.embedded.ifNotNil,
    "ifTrue",                     &execute.embedded.ifTrue,
    "immutableClosure",           &primitives.embedded.BlockClosure.immutableClosure,
    "over",                       &execute.embedded.over,
    "p201",                       &primitives.primitives.p201,
    "p202",                       &primitives.primitives.p202,
    "p203",                       &primitives.primitives.p203,
    "p204",                       &primitives.primitives.p204,
    "p205",                       &primitives.primitives.p205,
    //"perform",&execute.embedded.perform,
    //"performWith",&execute.embedded.performWith,
    "popLocal",                   &execute.embedded.popLocal,
    "popLocal0",                  &execute.embedded.popLocal0,
    "popLocalData",               &execute.embedded.popLocalData,
    "popLocalField",              &execute.embedded.popLocalField,
    "printStack",                 &execute.embedded.printStack,
    "pushContext",                &execute.embedded.pushContext,
    "pushLiteral",                &execute.embedded.pushLiteral,
    "pushLiteral0",               &execute.embedded.pushLiteral0,
    "pushLiteral1",               &execute.embedded.pushLiteral1,
    "pushLiteral2",               &execute.embedded.pushLiteral2,
    "pushLiteralFalse",           &execute.embedded.pushLiteralFalse,
    "pushLiteralIndirect",        &execute.embedded.pushLiteralIndirect,
    "pushLiteralNil",             &execute.embedded.pushLiteralNil,
    "pushLiteralTrue",            &execute.embedded.pushLiteralTrue,
    "pushLiteral_1",              &execute.embedded.pushLiteral_1,
    "pushLocal",                  &execute.embedded.pushLocal,
    "pushLocal0",                 &execute.embedded.pushLocal0,
    "pushLocalData",              &execute.embedded.pushLocalData,
    "pushLocalField",             &execute.embedded.pushLocalField,
    "pushNonlocalBlock_false",    &primitives.embedded.BlockClosure.pushNonlocalBlock_false,
    "pushNonlocalBlock_minusOne", &primitives.embedded.BlockClosure.pushNonlocalBlock_minusOne,
    "pushNonlocalBlock_nil",      &primitives.embedded.BlockClosure.pushNonlocalBlock_nil,
    "pushNonlocalBlock_one",      &primitives.embedded.BlockClosure.pushNonlocalBlock_one,
    "pushNonlocalBlock_self",     &primitives.embedded.BlockClosure.pushNonlocalBlock_self,
    "pushNonlocalBlock_true",     &primitives.embedded.BlockClosure.pushNonlocalBlock_true,
    "pushNonlocalBlock_two",      &primitives.embedded.BlockClosure.pushNonlocalBlock_two,
    "pushNonlocalBlock_zero",     &primitives.embedded.BlockClosure.pushNonlocalBlock_zero,
    "pushThisContext",            &execute.embedded.pushThisContext,
    "replaceLiteral",             &execute.embedded.replaceLiteral,
    "replaceLiteral0",            &execute.embedded.replaceLiteral0,
    "replaceLiteral1",            &execute.embedded.replaceLiteral1,
    "returnNoContext",            &execute.embedded.returnNoContext,
    "returnNonLocal",             &execute.embedded.returnNonLocal,
    "returnTop",                  &execute.embedded.returnTop,
    "returnWithContext",          &execute.embedded.returnWithContext,
    "setupSend",                  &execute.embedded.setupSend,
    "setupTailSend",              &execute.embedded.setupTailSend,
    "setupTailSend0",             &execute.embedded.setupTailSend0,
    "storeLocal",                 &execute.embedded.storeLocal,
    "swap",                       &execute.embedded.swap,
//    "value:",                     &primitives.embedded.BlockClosure.@"value:",
    "verifyMethod",               &execute.embedded.verifyMethod,
});
var zfHashValue: u64 = 0;
const print = std.debug.print;
fn capitalize(c: u8) u8 {
    return c - 'a' + 'A';
}
fn hash(addr: ThreadedFn) void {
    zfHashValue = std.math.rotr(u64, zfHashValue, 1) +% @intFromPtr(addr);
}
fn format_timestamp(timestamp: u64, out_buffer: []u8) void {
    if (std.debug.runtime_safety) std.debug.assert(out_buffer.len >= 27);
    const epoch_seconds: std.time.epoch.EpochSeconds = .{ .secs = timestamp };
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
fn writeHeader() void {
    const unix_timestamp: u64 = @intCast(std.time.timestamp());
    const timestamp: []const u8 = blk: {
        var buffer: [27]u8 = undefined; // Buffer size increased to 27 bytes
        format_timestamp(unix_timestamp, &buffer);
        break :blk &buffer;
    };
    print(
        \\{c}I have shared variables generated by zag so that images can be generated that directly reference zig code.
            \\From zag on {s}{c}
            \\
            , .{ '"', timestamp, '"' });
    print(
        \\Class {c}
            \\	#name : 'ASZagConstants',
            \\	#superclass : 'SharedPool',
            \\	#classVars : [
            \\		'Addresses',
            \\		'PredefinedSymbols',
            \\		'Primitives'
            \\
            , .{ '{' });
    for (references[0..]) |ref| {
        if (ref.str[0]=='p' and ref.str[1] <= '9' and ref.str[1] >= '0') {}
        else
            print("\t\t'Zf{c}{s}',\n", .{ capitalize(ref.str[0]), ref.str[1..] });
    }
    print(
        \\	],
            \\	#category : 'ASTSmalltalk-Image',
            \\	#package : 'ASTSmalltalk',
            \\	#tag : 'Image'
            \\{c}
            \\
            \\{c} #category : 'mapping' {c}
            \\ASZagConstants class >> constantMap [
            \\
            \\	^ Dictionary new
            \\
            , .{ '}', '{', '}' });
    for (references[0..]) |ref| {
        if (ref.str[0]=='p' and ref.str[1] <= '9' and ref.str[1] >= '0') {}
        else
            print("\t\t  at: #{s} put: Zf{c}{s};\n", .{ ref.str, capitalize(ref.str[0]), ref.str[1..] });
    }
    print(
        \\		  yourself
            \\]
            \\
            \\{c} #category : 'class initialization' {c}
            \\ASZagConstants class >> initialize [
            \\
            \\	PredefinedSymbols := #(  ).
            \\	self initializeForImage.
            \\	Addresses := self constantMap
            \\]
            \\
            \\{c} #category : 'class initialization' {c}
            \\ASZagConstants class >> initializeForImage [
            \\	CodeTable := {c}
            \\
            , .{ '{', '}', '{', '}', '{' });
    var nPrimitives : usize = 0;
    for (references[0..]) |ref| {
        if (ref.str[0]=='p' and ref.str[1] <= '9' and ref.str[1] >= '0') {
            print("\t\t{}.\n",
                  .{ @intFromPtr(ref.addr) });
            nPrimitives = @max(nPrimitives,std.zig.number_literal.parseNumberLiteral(ref.str[1..]).int);
        } else
            print("\t\tZf{c}{s} := {}.\n",
                  .{ capitalize(ref.str[0]), ref.str[1..], @intFromPtr(ref.addr) });
    }
    print(
        \\	{c}.
            \\	Primitives := Array new: {};
            \\
            ,.{'}', nPrimitives});
    for (references[0..]) |ref| {
        if (ref.str[0]=='p' and ref.str[1] <= '9' and ref.str[1] >= '0') {
            print("\t\tat: {} put: {};\n",
                  .{ std.zig.number_literal.parseNumberLiteral(ref.str[1..]).int,
                    @intFromPtr(ref.addr) });
        }
    }
    print(
        \\		yourself
            \\]
            ,.{});
}

const ZagImageHeader = struct {
    magic: u64,
    target: Object,
    selector: Object,
    classTable: Object,
    symTable: Object,
    dispatchTable: Object,
    codeAddresses: Object,
    processTable: Object,
    const magic: u64 = 0x010203040567615A;
};
var zagImageHeader: ZagImageHeader = undefined;
fn usage() void {
            std.debug.print(
            \\Usage: zig -constants | image-directory
            \\
            ,.{});
}
fn extensionMatches(name: []const u8, ext: []const u8) bool {
    if (name.len<=ext.len) return false;
    for(name[name.len-ext.len..],ext) |n,e| {
        if (n!=e) return false;
    }
    return true;
}
fn readImage(file: std.fs.File) !void {
    if ((try file.stat()).size!=@sizeOf(ZagImageHeader))
        return error.WrongImageFileSize;
    _ = try file.read(@as([*]u8,@ptrCast(&zagImageHeader))[0..@sizeOf(ZagImageHeader)]);
    if (zagImageHeader.magic!=ZagImageHeader.magic)
        return error.BadImageMagic;
}
fn runImage() !void {
    @panic("not implemented");
}
fn readHeap(file: std.fs.File) !void {
    _ = file;
    @panic("not implemented");
}
fn readLargeHeapObject(file: std.fs.File) !void {
    _ = file;
    @panic("not implemented");
}
fn readProcess(file: std.fs.File) !void {
    _ = file;
    @panic("not implemented");
}
fn loadAndRun(directory: [*:0]const u8) !void {
    var dir = try std.fs.cwd().openDirZ(directory,.{});
    defer dir.close();
    var loadedImage = false;
    {
        var errors = false;
        var it = dir.iterate();
        while (try it.next()) |entry| {
            const name = entry.name;
            if (entry.kind == .file) {
                if (extensionMatches(name,".image")) {
                    const file = try dir.openFile(name,.{});
                    defer file.close();
                    try readImage(file);
                    loadedImage = true;
                } else if (extensionMatches(name,".heap")) {
                } else if (extensionMatches(name,".lho")) {
                } else if (extensionMatches(name,".process")) {
                } else {
                    std.debug.print("unknown file: {s}\n",.{name});
                    errors = true;
                }
            } else {
                std.debug.print("unknown non-file: {s}\n",.{name});
                errors = true;
            }
        }
        if (errors or !loadedImage)
            return error.ImageLoadProblem;
    }
    var it = dir.iterate();
    while (try it.next()) |entry| {
        const name = entry.name;
        if (entry.kind == .file) {
            if (extensionMatches(name,".image")) {
            } else if (extensionMatches(name,".heap")) {
                const file = try dir.openFile(name,.{});
                defer file.close();
                try readHeap(file);
            } else if (extensionMatches(name,".lho")) {
                const file = try dir.openFile(name,.{});
                defer file.close();
                try readLargeHeapObject(file);
            } else if (extensionMatches(name,".process")) {
                const file = try dir.openFile(name,.{});
                defer file.close();
                try readProcess(file);
            }
        }
    }
    try runImage();
}
pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var argsIterator = try std.process.ArgIterator.initWithAllocator(allocator);
    defer argsIterator.deinit();

    _ = argsIterator.next(); // Skip executable

    if (argsIterator.next()) |arg| {
        if (std.mem.orderZ(u8, arg, "-constants") == .eq) {
            writeHeader();
        } else if (std.mem.orderZ(u8, arg, "-h") == .eq or std.mem.orderZ(u8, arg, "--help") == .eq) {
            usage();
        } else {
            try loadAndRun(arg);
        }
        while (argsIterator.next()) |extra|
            std.debug.print(
                \\unused argument: {s}
                    \\
            ,.{extra});

    } else {
        usage();
    }
}
