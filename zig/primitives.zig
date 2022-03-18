const MethodReturns = @import("dispatch.zig").MethodReturns;
const Normal = MethodReturns.Normal;
const PrimitiveFailed = MethodReturns.PrimitiveFailed;
const NonLocal = MethodReturns.NonLocal;
const ExceptionSignaled = MethodReturns.ExceptionSignaled;

const Thread = @import("thread.zig").Thread;
const object = @import("object.sig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
pub const PrimitiveErrors = error {Failure, WithArg};

pub inline fn @"prim_1_+"(thread: *Thread) PrimitiveErrors!MethodReturns {
    const stack = thread.stack();
    if (stack[0].is_int()) {
        const other = stack[0].to(i64);
        stack[1]+=@bitCast(Object,@bitCast(i64,stack[1])+other);
        thread.pop(1);
        return .Normal;
    }
    return .Failure;
}
pub inline fn @"prim_110_=="(thread: *Thread) PrimitiveErrors!MethodReturns { // ProtoObject>>==
    const stack = thread.stack();
    stack[1] = if (stack[0].equals(stack[1])) True else False;
    thread.pop(1);
    return .Normal;
}
pub inline fn @"prim_169_~~"(thread: *Thread) PrimitiveErrors!MethodReturns { // ProtoObject>>~~
    const stack = thread.stack();
    stack[1] = if (stack[0].equals(stack[1])) False else True;
    thread.pop(1);
    return .Normal;
}
