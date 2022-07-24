const Context = @import("execute.zig").Context;
const Code = @import("execute.zig").Code;
const tailCall = @import("execute.zig").tailCall;
const branch = @import("execute.zig").branch;
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;

pub fn p1(thread: *Thread, next: [*]const Code, heap: [*]Object, tos: [*]Object, caller: Context) Object {//SmallInteger>>#+
    const other = tos[0];
    if (other.is_int()) {
        const o = other.toUnchecked(i64);
        const result = @bitCast(Object,@bitCast(i64,tos[1])+o);
        if (result.is_int()) {
            tos[1]=result;
            return @call(tailCall,next[1].prim,.{thread,next+2,heap,tos+1,caller});
        }
    }
    return @call(tailCall,branch,.{thread,next,heap,tos,caller});
}
pub fn p110(thread: *Thread, next: [*]const Code, heap: [*]Object, tos: [*]Object, caller: Context) Object { // ProtoObject>>==
    const result = Object.from(tos[1].equals(tos[0]));
    tos[1]=result;
    return @call(tailCall,next[0].prim,.{thread,next+1,heap,tos+1,caller});
}
pub fn p169(thread: *Thread, next: [*]const Code, heap: [*]Object, tos: [*]Object, caller: Context) Object { // ProtoObject>>!!
    const result = Object.from(!tos[1].equals(tos[0]));
    tos[1]=result;
    return @call(tailCall,next[0].prim,.{thread,next+1,heap,tos+1,caller});
}
// pub inline fn p111(thread: *Thread, next: [*]const Code, heap: [*]Object, tos: [*]Object, caller: Context) Object { // ProtoObject>>class
