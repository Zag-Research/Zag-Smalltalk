const std = @import("std");
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const dispatch = @import("dispatch.zig");
const methodT = dispatch.methodT;
const heap = @import("heap.zig");
const treap = @import("treap.zig");
pub const ClassIndex = u16; // only allows 65535 classes and this size is baked into a few places, but Pharo has less than 18000 (including metaclasses), so shouldn't be a problem
pub const Object_I: ClassIndex = 1;
pub const False_I: ClassIndex = 2;
pub const True_I: ClassIndex = 3;
pub const UndefinedObject_I: ClassIndex = 4;
pub const Symbol_I: ClassIndex = 5;
pub const Character_I: ClassIndex = 6;
pub const SmallInteger_I: ClassIndex = 7;
pub const Float_I: ClassIndex = 8;
pub const Array_I: ClassIndex = 9;
pub const String_I: ClassIndex = 10;
pub const Class_I: ClassIndex = 11;
pub const Metaclass_I: ClassIndex = 12;
pub const Behavior_I: ClassIndex = 13;
pub const BlockClosure_I: ClassIndex = 14;
pub const Method_I: ClassIndex = 15;
pub const MethodDictionary_I: ClassIndex = 16;
pub const System_I: ClassIndex = 17;
pub const Return_I: ClassIndex = 18;
pub const Send_I: ClassIndex = 19;
pub const Literal_I: ClassIndex = 20;
pub const Load_I: ClassIndex = 21;
pub const Store_I: ClassIndex = 22;
pub const SymbolTable_I: ClassIndex = 23;
pub const Dispatch_I: ClassIndex = 24;
var number_of_classes: usize = 24;
pub const ReservedSpaceForClasses = 5000;
var classes = [_]object.Object{Nil} ** ReservedSpaceForClasses;
const Behavior_S = packed struct {
    superclass: Object,
    methodDict: Object,
    format: Object,
};
const ClassDescription_S = packed struct {
    super: Behavior_S,
    organization: Object,
};
pub const Metaclass_S = packed struct{
    const ClassIndex = Metaclass_I;
    super: ClassDescription_S,
    soleInstance: Object,
};
pub const Class_S = packed struct{
    const ClassIndex = Class_I;
    super: ClassDescription_S,
    name: Object,
    instVarNames: Object,
    classVariables: Object,
    pub fn addMethod(self: *Class_S, name: Object, method: methodT) void {
        _ = self;
        _ = name;
        _ = method;
        unreachable;
    }
};
pub fn getClass(name: Object) Object {
    _ = name;
    unreachable;
}
pub fn init() !void {
    const classesList = [_]ClassIndex {
        "Object",
        "False",
        "True",
        "UndefinedObject",
        "Symbol",
        "Character",
        "SmallInteger",
        "Float",
        "Array",
        "String",
        "Class",
        "Metaclass",
        "Behavior",
        "BlockClosure",
        "Method",
        "MethodDictionary",
        "System",
        "Return",
        "Send",
        "Literal",
        "Load",
        "Store",
        "SymbolTable",
        "Dispatch",
    };
    _ = classesList;
}
