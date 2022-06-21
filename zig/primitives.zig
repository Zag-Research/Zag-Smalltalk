const _MR = @import("dispatch.zig").MethodReturns;
const Context = @import("dispatch.zig").Context;
const object = @import("object.sig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;

pub inline fn @"prim_1_+"(self: Object, other: Object, _: *Context) !_MR {
    if (other.is_int()) {
        const o = other.to(i64);
        const result = @bitCast(Object,@bitCast(i64,self)+o);
        if (result.is_int()) return _MR{.Normal=result};
    }
    return error.Failure;
}
pub inline fn @"prim_110_=="(self: Object, other: Object, _: *Context) !_MR { // ProtoObject>>==
    return _MR{.Normal=Object.from(self.equals(other))};
}
pub inline fn @"prim_169_~~"(self: Object, other: Object, _: *Context) !_MR { // ProtoObject>>~~
    return _MR{.Normal=Object.from(!self.equals(other))};
}
pub inline fn prim_111_class(self: Object, _: Object, _: *Context) !_MR { // ProtoObject>>class
    _ = self;
    return Nil;
}
