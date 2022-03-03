const Heap = @import("heap");
pub const ClassIndex = u16; // only allows 65535 classes and this size is baked into a few places, but Pharo only has 20000, so shouldn't be a problem
pub const classObject: ClassIndex = 1;
pub const classFalse: ClassIndex = 2;
pub const classTrue: ClassIndex = 3;
pub const classUndefinedObject: ClassIndex = 4;
pub const classSymbol: ClassIndex = 5;
pub const classCharacter: ClassIndex = 6;
pub const classSmallInteger: ClassIndex = 7;
pub const classFloat: ClassIndex = 8;
pub const classArray: ClassIndex = 9;
pub const classString: ClassIndex = 10;
pub const classClass: ClassIndex = 11;
pub const classMetaclass: ClassIndex = 12;
pub const classBehavior: ClassIndex = 13;
pub const classBlockClosure: ClassIndex = 14;
pub const classMethod: ClassIndex = 15;
pub const classMethodDictionary: ClassIndex = 16;
pub const classSystem: ClassIndex = 17;
pub const classReturn: ClassIndex = 18;
pub const classSend: ClassIndex = 19;
pub const classLiteral: ClassIndex = 20;
pub const classLoad: ClassIndex = 21;
pub const classStore: ClassIndex = 22;
pub var number_of_classes: usize = 1;
pub fn init() !void {
    const classes = [_]ClassIndex {
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
    };
    _ = classes;
}
