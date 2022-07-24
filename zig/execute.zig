const std = @import("std");
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const NilFlag = object.NilFlag;
const True = object.True;

const Context = [*]Object;
const Code = packed union {
    prim: PrimitivePtr,
    int: i64,
    object: Object,
    fn object(o: Object) Code {
        return Code{.object=o};
    }
    fn end() Code {
        return Code{.object=NilFlag};
    }
    fn int(i: i64) Code {
        return Code{.int=i};
    }
    fn prim(p: PrimitivePtr) Code {
        return Code{.prim=p};
    }
};
const PrimitivePtr = fn(next: [*]Code,heap: [*]Object, tos: [*]Object, caller: Context) Object;
fn countNonLabels(comptime tup: anytype) usize {
    var n = 0;
    inline for (tup) |field| {
        switch (@typeInfo(@TypeOf(field))) {
            .Pointer => {if (field[field.len-1]!=':') n = n + 1;},
            else => {n = n+1;},
        }
    }
    return n;
}
fn compileTuple(name: Object, comptime tup: anytype) [countNonLabels(tup)+2]Code {
    var result: [countNonLabels(tup)+2]Code = undefined;
    result[0] = Code.object(name);
    comptime var n = 1;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {result[n]=Code.object(field);n=n+1;},
            comptime_int => {result[n]=Code.int(field);n = n+1;},
            PrimitivePtr => {result[n]=Code.prim(field);n=n+1;},
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => {
                        if (field[field.len-1]==':') {
                            found = true;
                        } else {
                            comptime var p = 0;
                            inline for (tup) |t| {
                                switch (@typeInfo(@TypeOf(t))) {
                                    .Pointer => {
                                        if (t[t.len-1]==':') {
                                            if (comptime std.mem.startsWith(u8,t,field)) {
                                                result[n]=Code.int(p+1-n);
                                                n=n+1;
                                                found = true;
                                            }
                                        } else p=p+1;
                                    },
                                    else => {p=p+1;},
                                }
                            }
                            if (!found) @compileError("missing label: "++field);
                        }
                    },
                    else => {},
                }
                if (!found) @compileError("don't know how to handle \""++@typeName(@TypeOf(field))++"\"");
            },
        }
    }
    result[n]=Code.end();
    return result;
}
const stdout = std.io.getStdOut().writer();
fn return_tos(next: [*]Code, heap: [*]Object, tos: [*]Object, caller: Context) Object {
    _ = next;
    _ = heap;
    _ = caller;
    return tos[0];
}
test "compiling tuple" {
    const expectEqual = std.testing.expectEqual;
    var t = compileTuple(Nil,.{"abc:", return_tos, "def", True, 42, "def:", "abc"});
    try expectEqual(t.len,7);
    try expectEqual(t[0].object,Nil);
    try expectEqual(t[2].int,3);
    try expectEqual(t[3].object,True);
    try expectEqual(t[4].int,42);
    try expectEqual(t[5].int,-4);
    try expectEqual(t[t.len-1].object,NilFlag);
}
