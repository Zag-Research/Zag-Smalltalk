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

pub fn p1(next: [*]const Code, tos: [*]Object, heap: [*]Object, thread: *Thread, caller: Context) Object {//SmallInteger>>#+
    const other = tos[0];
    if (other.is_int()) {
        const o = other.toUnchecked(i64);
        const result = @bitCast(Object,@bitCast(i64,tos[1])+o);
        if (result.is_int()) {
            tos[1]=result;
            return @call(tailCall,next[1].prim,.{next+2,tos+1,heap,thread,caller});
        }
    }
    return @call(tailCall,branch,.{next,tos,heap,thread,caller});
}
pub fn p110(next: [*]const Code, tos: [*]Object, heap: [*]Object, thread: *Thread, caller: Context) Object { // ProtoObject>>==
    const result = Object.from(tos[1].equals(tos[0]));
    tos[1]=result;
    return @call(tailCall,next[0].prim,.{next+1,tos+1,heap,thread,caller});
}
pub fn p169(next: [*]const Code, tos: [*]Object, heap: [*]Object, thread: *Thread, caller: Context) Object { // ProtoObject>>!!
    const result = Object.from(!tos[1].equals(tos[0]));
    tos[1]=result;
    return @call(tailCall,next[0].prim,.{next+1,tos+1,heap,thread,caller});
}
// pub inline fn p111(thread: *Thread, next: [*]const Code, heap: [*]Object, tos: [*]Object, caller: Context) Object { // ProtoObject>>class
