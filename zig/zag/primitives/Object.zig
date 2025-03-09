const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const Process = zag.Process;
const Context = zag.context;
const object = zag.object;
const Object = object.Object;
const empty = Object.empty;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const Sym = zag.symbol.zig.symbols;
const heap = zag.heap.zig;
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub fn init() void {}
pub const moduleName = "object";
pub const inlines = struct {
    pub inline fn p60(self: Object, other: Object) !Object { // basicAt:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p61(self: Object, other: Object) !Object { // basicAt:put:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p70(self: Object, other: Object) !Object { // basicNew
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p71(self: Object, other: Object) !Object { // basicNew:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p110(self: Object, other: Object) bool { // Identical - can't fail
        return self == other;
    }
    pub inline fn p145(self: Object, other: Object) !Object { // atAllPut:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p169(self: Object, other: Object) bool { // NotIdentical - can't fail
        return !self.equals(other);
    }
};
const fallback = execute.fallback;
const embedded = struct {
    pub fn @"basicAt:"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        sp[1] = inlines.p60(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, undefined, undefined });
    }
    pub fn @"basicAt:put:"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        sp[1] = inlines.p61(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:put:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, undefined, undefined });
    }
    pub fn p110(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP { // ProtoObject>>#==
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.dropPut(Object.from(inlines.p110(sp.next, sp.top))), process, context, undefined });
    }
    pub fn p169(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP { // ProtoObject>>#~~
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.dropPut(Object.from(inlines.p169(sp.next, sp.top))), process, context, undefined });
    }
    // pub inline fn p111(pc: PC, sp: SP, heap: Hp, rpc: PC, process: *Process, caller: Context) Object { // ProtoObject>>class
    pub fn p145(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP { // atAllPut:
        const newSp = sp.dropPut(inlines.p145(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"atAllPut:" }));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
};
pub const primitive60 = struct {
    pub const number = 60;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // basicAt:
        if (inlines.p60(sp.next, sp.top)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, context.npc, .{ context.tpc, newSp, process, context, undefined });
        } else |_| {}
        return @call(tailCall, extra.threadedFn(), .{ pc, sp, process, context, extra.encoded() });
    }
};
pub const primitive110 = struct {
    pub const number = 110;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // ==
        const newSp = sp.dropPut(Object.from(inlines.p110(sp.next, sp.top)));
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, extra });
    }
};
const primitives = struct {
    pub fn p61(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // basicAt:put:
        _ = (.{ pc, sp, process, context, extra });
        unreachable;
    }
    pub fn p83(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // perform: perform:with: perform:with:with: perform:with:with:with:
        _ = (.{ pc, sp, process, context, extra });
        unreachable;
    }
    pub fn p84(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // perform:withArguments:
        _ = (.{ pc, sp, process, context, extra });
        unreachable;
    }
    pub fn p100(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // perform:withArguments:inSuperclass:
        _ = (.{ pc, sp, process, context, extra });
        unreachable;
    }
    pub fn p145(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // atAllPut:
        _ = (.{ pc, sp, process, context, extra });
        inlines.p1(sp[0]) catch
            return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, undefined });
        return @call(tailCall, context.npc, .{ context.tpc, sp + 1, process, context, undefined });
    }
    pub fn p169(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP { // ProtoObject>>#~~
        _ = (.{ pc, sp, process, context, extra });
        sp[1] = Object.from(inlines.p169(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, undefined });
    }
    // pub inline fn p111(pc: PC, sp: SP, heap: Hp, rpc: PC, process: *Process, caller: Context) Object { // ProtoObject>>class
};
