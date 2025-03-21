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
const Result = execute.Result;
const Process = zag.Process;
const Context = zag.Context;
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
    pub inline fn @"basicAt:"(self: Object, other: Object) !Object {
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn @"basicAt:put:"(self: Object, other: Object) !Object {
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn basicNew(self: Object, other: Object) !Object {
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn @"basicNew:"(self: Object, other: Object) !Object {
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn @"=="(self: Object, other: Object) bool { // Identical - can't fail
        return self == other;
    }
    pub inline fn @"atAllPut:"(self: Object, other: Object) !Object {
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn @"~~"(self: Object, other: Object) bool { // NotIdentical - can't fail
        return !self.equals(other);
    }
};
pub const @"basicAt:" = struct {
    pub const number = 60;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (inlines.@"basicAt:"(sp.next, sp.top)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
        } else |_| {}
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
};
pub const @"==" = struct {
    pub const number = 110;
    pub fn primitive(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
        const newSp = sp.dropPut(Object.from(inlines.@"=="(sp.next, sp.top)));
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
    }
};
pub const @"~~" = struct {
    pub const number = 169;
    pub fn primitive(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
        const newSp = sp.dropPut(Object.from(inlines.@"~~"(sp.next, sp.top)));
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
    }
};
pub const @"perform:" = struct { // perform: perform:with: perform:with:with: perform:with:with:with:
    pub const number = 83;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        // the same primitive number is used for perform:, perform:with:, etc.
        // so we need to find out where the selector is
        const arity = extra.method.signature.numArgs() - 1;
        const selector = sp.at(arity);
        const numArgs = selector.numArgs();
        if (selector.isSymbol() and numArgs == arity) {
            //     const newPc = lookupAddress(selector);//, sp.next.get_class());
            unreachable;
        }
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
};
pub const @"perform:withArguments:" = struct {
    pub const number = 84;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        //     const context = tfAsContext(_context);
        //     const selector = sp.next;
        //     sp.next = sp.top;
        //     if (selector.numArgs() != 1) @panic("wrong number of args");
        //     const newPc = lookupAddress(selector);//, sp.third.get_class());
        //     context.setTPc(pc + 1);
        //     return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp + 1, process, context, undefined });
        _ = .{ pc, sp, process, context, extra, unreachable };
    }
};
pub const @"perform:withArguments:inSuperclass:" = struct {
    pub const number = 100;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ pc, sp, process, context, extra, unreachable };
    }
};

pub const @"basicAt:put:" = struct {
    pub const number = 61;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ pc, sp, process, context, extra, unreachable };
    }
};
pub const @"atAllPut:" = struct {
    pub const number = 145;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ pc, sp, process, context, extra, unreachable };
    }
};
pub const @"ProtoObject>>class" = struct {
    pub const number = 111;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ pc, sp, process, context, extra, unreachable };
    }
};
