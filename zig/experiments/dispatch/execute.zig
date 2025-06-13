const std = @import("std");
const ee = std.testing.expectEqual;
const assert = std.debug.assert;
const print = std.debug.print;
const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const checkEqual = zag.utilities.checkEqual;
const object = zag.zobject;
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const indexSymbol0 = object.Object.indexSymbol0;
const indexSymbol1 = object.Object.indexSymbol1;
const heap = zag.heap;
const symbol = zag.symbol;
const Sym = symbol.symbols;

pub const SP = *Stack;
const Stack = struct {
    top: Object,
    next: Object,
    third: Object,
};
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = struct {
    signature: Signature,
    executeFn: ThreadedFn,
    code: [codeSize]Code, // will typically be a lot more then 1, as it will be the threaded version of the method
    const Self = @This();
    const codeSize = 10;
    pub fn init(name: Object, methodFn: ThreadedFn) Self {
        return Self{
            .signature = Signature.from(name, .none),
            .executeFn = methodFn,
            .code = undefined,
        };
    }
    fn fill(self: *Self, tup: anytype) void {
        var free = 0;
        inline for (tup, 0..) |field, index| {
            self.code[index] = field;
            free = index + 1;
        }
        for (self.code[free..]) |*c|
            c.* = Code.panic;
    }
    pub fn execute(self: *Self, sp: SP) callconv(stdCall) SP {
        const pc = PC.init(&self.code[0]);
        trace("\nexecute: {} {} {}", .{ pc, sp, self.signature });
        return pc.prim()(pc.next(), sp, self.signature);
    }
    pub fn forDispatch(self: *Self, class: ClassIndex) void {
        self.signature.setClass(class);
        addMethod(self);
    }
};
const Extra = Object;
pub const ThreadedFn = packed struct {
    f: Fn,
    pub const Fn = *const fn (programCounter: PC, stackPointer: SP, signature: Extra) callconv(stdCall) SP;
};
pub const Signature = zag.main.execute.Signature;
pub const PC = packed struct {
    code: *const Code,
    pub const baseType = Code;
    const Self = @This();
    pub inline fn init(code: *const Code) PC {
        return .{ .code = code };
    }
    pub inline fn method(self: PC) CompiledMethodPtr {
        return @constCast(self.code.method);
    }
    pub inline fn codeAddress(self: PC) *const Code {
        return self.code.codeAddress;
    }
    pub inline fn targetPC(self: PC) PC {
        return .{ .code = self.code.codeAddress };
    }
    pub inline fn prim(self: PC) ThreadedFn {
        return self.code.prim;
    }
    pub inline fn uint(self: PC) u64 {
        return self.code.uint;
    }
    pub inline fn int(self: PC) i64 {
        return self.code.int;
    }
    pub inline fn object(self: PC) Object {
        return self.code.object;
    }
    pub inline fn asCode(self: PC) Code {
        return self.code.*;
    }
    pub inline fn asCodePtr(self: PC) *const Code {
        return self.code;
    }
    pub inline fn next(self: PC) PC {
        return asPC(self.array() + 1);
    }
    pub inline fn prev(self: PC) PC {
        return asPC(self.array() - 1);
    }
    pub inline fn prim2(self: PC) ThreadedFn {
        return self.array()[1].prim;
    }
    pub inline fn next2(self: PC) PC {
        return asPC(self.array() + 2);
    }
    pub inline fn skip(self: PC, n: usize) PC {
        return asPC(self.array() + n);
    }
    pub inline fn returnOffset(self: PC) PC {
        return self.skip(config.picSize * 2);
    }
    pub inline fn offsetFor(self: PC, n: usize) PC {
        return @bitCast(@intFromPtr(@as([*]const Code, @ptrCast(self.code)) + n));
    }
    pub inline fn offsetBytes(self: PC, n: usize) PC {
        return @bitCast(@intFromPtr(self.code) + n);
    }
    pub inline fn back(self: PC, n: usize) PC {
        return @bitCast(@intFromPtr(@as([*]const Code, @ptrCast(self.code)) - n));
    }
    pub inline fn literalIndirect(self: PC) Object {
        const offset = self.code.uint;
        return @as(*const Object, @ptrCast(@as([*]const Code, @ptrCast(self.code)) + 1 + offset)).*;
    }
    inline fn asPC(ptr: [*]const Code) PC {
        return .{ .code = @ptrCast(ptr) };
    }
    inline fn array(self: PC) [*]const Code {
        return @ptrCast(self.code);
    }
};
pub const Code = union {
    prim: ThreadedFn,
    int: i64,
    uint: u64,
    object: Object,
    header: heap.HeapObject,
    method: *const CompiledMethod,
    codeAddress: *const Code,
    const refFlag = 1024;
    pub inline fn primOf(pp: ThreadedFn) Code {
        return Code{ .prim = pp };
    }
    inline fn intOf(i: i64) Code {
        return Code{ .int = i };
    }
    pub inline fn uintOf(u: u64) Code {
        return Code{ .uint = u };
    }
    pub inline fn asObject(self: Code) Object {
        @setRuntimeSafety(false);
        return self.object;
    }
    pub inline fn objectOf(o: Object) Code {
        return Code{ .object = o };
    }
    inline fn ref1Of(comptime u: u12) Code {
        return Code{ .object = indexSymbol1(u) };
    }
    inline fn headerOf(h: heap.HeapObject) Code {
        return Code{ .header = h };
    }
    pub inline fn codeRefOf(c: [*]const Code) Code {
        return Code{ .uint = @intFromPtr(@constCast(c)) };
    }
    pub fn end(_: PC, sp: SP, _: Extra) callconv(stdCall) SP { // not embedded
        return sp;
    }
    pub fn panic(_: PC, _: SP, _: Extra) callconv(stdCall) SP { // not embedded
        @panic("not implemented");
    }
    var endCode = [CompiledMethod.codeSize]Code{.{ .prim = .{ .f = end } }};
    pub const endThread = PC.init(@ptrCast(&endCode));
    pub fn format(
        self: *const Code,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const obj = self.asObject();
        if (!obj.isDouble()) {
            try writer.print("{}", .{obj});
        } else if (self.int >= -100 and self.int < 100) {
            try writer.print("({})", .{self.int});
        } else try writer.print("0x{x}", .{self.uint});
    }
};
