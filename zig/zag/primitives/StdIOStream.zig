const std = @import("std");
const config = @import("../config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const execute = @import("../execute.zig");
const SendCache = execute.SendCache;
const Context = execute.Context;
const ContextPtr = *Context;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const Sym = @import("../symbol.zig").symbols;
const heap = @import("../heap.zig");
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;
const empty = &[0]Object{};
const symbols = @import("../symbol.zig").symbols;
const utf8Decode = std.unicode.utf8Decode;
const utf8ByteSequenceLength = std.unicode.utf8ByteSequenceLength;

pub fn init() void {}
pub const inlines = struct {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    fn readCharacter() Object {
        var chars: [4]u8 = undefined;
        if (stdin.read(chars[0..1]) < 1) return Nil;
        const n = utf8ByteSequenceLength(chars) catch @panic("invalid utf8");
        if (n > 1) {
            if (stdin.read(chars[1..n]) < n - 1) return Nil;
        }
        return Object.asCharacter(utf8Decode(char[0..n]) catch @panic("invalid utf8"));
    }
};

pub const embedded = struct {};

fn ignore() !void {
    _ = try stdin.readUntilDelimiter(&input, '\n');

    try stdout.print("The user entered: {s}\n", .{input});
}
pub const primitives = struct {
    pub fn p1(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP { // SmallInteger>>#+
        trace("\n+: {any}", .{context.stack(sp, process)});
        if (!Sym.@"+".withClass(.SmallInteger).selectorEquals(selector)) {
            const dPc = cache.current();
            return @call(tailCall, dPc.prim(), .{ dPc.next(), sp, process, context, selector, cache.next() });
        }
        trace("\np1: {any}", .{context.stack(sp, process)});
        const newSp = sp.dropPut(inlines.p1(sp.next, sp.top) catch
            return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache }));
        return @call(tailCall, context.npc, .{ context.tpc, newSp, process, context, undefined, undefined });
    }
};
const e = struct {
    usingnamespace execute.controlPrimitives;
    usingnamespace embedded;
};
pub fn main() void {
    var prog = compileMethod(Sym.value, 0, 0, .{
        &e.pushLiteral,    3,
        &e.pushLiteral,    4,
        &e.p1,             &e.pushLiteral,
        Object.from(-999), &e.returnNoContext,
    });
    _ = testExecute(prog.asCompiledMethodPtr());
}
