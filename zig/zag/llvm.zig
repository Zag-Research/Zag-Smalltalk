const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const checkEqual = @import("utilities.zig").checkEqual;
const object = @import("zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const indexSymbol0 = object.Object.indexSymbol0;
const indexSymbol1 = object.Object.indexSymbol1;
pub const Context = @import("context.zig").Context;

//const class = @import("class.zig");
const symbol = @import("symbol.zig");
const Sym = symbol.symbols;
const phi32 = @import("utilities.zig").inversePhi(u32);

const execute = @import("execute.zig");

pub fn init() void {
    //install allMethods in dispatch
}
const allMethods = [_]*CompiledMethod{
    @"CM_defineLabel:",
};
pub fn @"defineLabel:"(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
    // sp.top is the label to define
    // sp.next is self - the generator object
    const result = Nil; // some api
    const newSp = sp.dropPut(result); // or sp.drop() if returning self
    return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
}
const @"CM_defineLabel:" = compiledZigMethod("defineLabel:", @"defineLabel:");

test "primitives" {}
