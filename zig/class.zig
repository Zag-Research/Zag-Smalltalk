const heap = @import("heap");
const object = @import("object.zig");
pub const ClassIndex = u16; // only allows 65535 classes and this size is baked into a few places, but Pharo has less than 18000 (including metaclasses), so shouldn't be a problem
pub const Object: ClassIndex = 1;
pub const False: ClassIndex = 2;
pub const True: ClassIndex = 3;
pub const UndefinedObject: ClassIndex = 4;
pub const Symbol: ClassIndex = 5;
pub const Character: ClassIndex = 6;
pub const SmallInteger: ClassIndex = 7;
pub const Float: ClassIndex = 8;
pub const Array: ClassIndex = 9;
pub const String: ClassIndex = 10;
pub const Class: ClassIndex = 11;
pub const Metaclass: ClassIndex = 12;
pub const Behavior: ClassIndex = 13;
pub const BlockClosure: ClassIndex = 14;
pub const Method: ClassIndex = 15;
pub const MethodDictionary: ClassIndex = 16;
pub const System: ClassIndex = 17;
pub const Return: ClassIndex = 18;
pub const Send: ClassIndex = 19;
pub const Literal: ClassIndex = 20;
pub const Load: ClassIndex = 21;
pub const Store: ClassIndex = 22;
pub const SymbolTable: ClassIndex = 23;
pub const Dispatch: ClassIndex = 24;
var number_of_classes: usize = 24;
var classes = [_]object.Object{object.Nil} ** 5000;
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
