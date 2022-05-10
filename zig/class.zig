const std = @import("std");
const builtin = @import("builtin");
const thread = @import("thread.zig");
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const symbol = @import("symbol.zig");
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
pub const ClassTable_I: ClassIndex = 25;
pub const Magnitude_I: ClassIndex = 26;
pub const Number_I: ClassIndex = 27;

pub const ReservedNumberOfClasses = if (builtin.is_test) 100 else 500;
var classes = [_]object.Object{Nil} ** ReservedNumberOfClasses;
var classTable : Class_Table = undefined;
const objectTreap = treap.Treap(object.Object);
const Class_Table = struct {
    theObject: object.Object,
    const Self = @This();
    fn init(arena: *heap.Arena, initialClassTableSize:usize) !Self {
        var theHeapObject = try arena.allocObject(ClassTable_I,
                                                  heap.Format.none,0,initialClassTableSize*2);
        _ = objectTreap.init(theHeapObject.arrayAsSlice(u8),object.compareObject,Nil);
        return Class_Table {
            .theObject = theHeapObject.asObject(),
        };
    }
    fn deinit(s: *Self) void {
        s.*=undefined;
    }
    fn nextFree(s: *Self) ClassIndex {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject);
        return @truncate(ClassIndex,trp.nextFree() catch @panic("class treap full"));
    }
    fn lookup(s: *Self,sym: object.Object) ClassIndex {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject);
        return @truncate(ClassIndex,trp.lookup(sym));
    }
    fn intern(s: *Self, sym: object.Object) ClassIndex {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject);
        //const arena = thr.getArena().getGlobal();
        while (true) {
            const lu = s.lookup(sym);
            if (lu>0) return lu;
            const result = @truncate(ClassIndex,trp.insert(sym) catch @panic("class treap full"));
            if (result>0) return result;
            unreachable; // out of space
        }
        unreachable;
    }
    fn lookupLiteral(s: *Self, string: []const u8) ClassIndex {
        return s.lookup(symbol.lookupLiteral(string));
    }
    fn loadInitialClassNames(s: *Self, arena: *heap.Arena) void {
        var names = std.mem.tokenize(
            u8,
\\ Object False True
\\ UndefinedObject Symbol Character SmallInteger
\\ Float Array String Class Metaclass
\\ Behavior BlockClosure Method MethodDictionary System
\\ Return Send Literal Load Store
\\ SymbolTable Dispatch ClassTable Magnitude Number
                ," \n");
        while(names.next()) |name| {
            _ = s.intern(symbol.internLiteral(arena,name));
        }
    }
};
const Behavior_S = packed struct {
    header: u64,
    superclass: Object,
    methodDict: Object,
    format: Object,
};
const ClassDescription_S = packed struct {
    super: Behavior_S,
    index: ClassIndex,
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
    subclasses: Object,
};
pub fn getClass(name: Object) Object {
    _ = name;
    unreachable;
}
pub fn addClass(_: *thread.Thread, className: Object,superclass: ClassIndex) void {
    const metaclass = classTable.nextFree();
    const class = classTable.intern(className);
    _ = superclass;
    _ = metaclass;
    _ = class;
    // allocate nextFree location in treap (not in key structure) for metaclass
    // intern class name, add class object at that location with metaclass as ClassIndex
    // set up superclass on both sides
    @panic("unfinished code");
}
pub fn init(thr: *thread.Thread) !void {
    var arena = thr.getArena().getGlobal();
    classTable = try Class_Table.init(arena,ReservedNumberOfClasses);
    classTable.loadInitialClassNames(arena);
    addClass(thr,symbol.Object,0);
}
test "classes match initialized class table" {
    const expectEqual = std.testing.expectEqual;
    var thr = try thread.Thread.initForTest();
    try symbol.init(&thr,500);
    try init(&thr);
    var class = classTable;
    try expectEqual(Object_I,class.lookupLiteral("Object"));
    try expectEqual(False_I,class.lookupLiteral("False"));
    try expectEqual(True_I,class.lookupLiteral("True"));
    try expectEqual(UndefinedObject_I,class.lookupLiteral("UndefinedObject"));
    try expectEqual(SmallInteger_I,class.lookupLiteral("SmallInteger"));
    try expectEqual(Class_I,class.lookupLiteral("Class"));
    try expectEqual(Character_I,class.lookupLiteral("Character"));
    try expectEqual(Float_I,class.lookupLiteral("Float"));
    try expectEqual(Array_I,class.lookupLiteral("Array"));
    try expectEqual(String_I,class.lookupLiteral("String"));
    try expectEqual(Class_I,class.lookupLiteral("Class"));
    try expectEqual(Metaclass_I,class.lookupLiteral("Metaclass"));
    try expectEqual(Behavior_I,class.lookupLiteral("Behavior"));
    try expectEqual(BlockClosure_I,class.lookupLiteral("BlockClosure"));
    try expectEqual(Method_I,class.lookupLiteral("Method"));
    try expectEqual(System_I,class.lookupLiteral("System"));
    try expectEqual(Return_I,class.lookupLiteral("Return"));
    try expectEqual(Send_I,class.lookupLiteral("Send"));
    try expectEqual(Literal_I,class.lookupLiteral("Literal"));
    try expectEqual(Load_I,class.lookupLiteral("Load"));
    try expectEqual(Store_I,class.lookupLiteral("Store"));
    try expectEqual(SymbolTable_I,class.lookupLiteral("SymbolTable"));
    try expectEqual(Dispatch_I,class.lookupLiteral("Dispatch"));
    try expectEqual(ClassTable_I,class.lookupLiteral("ClassTable"));
    try expectEqual(Magnitude_I,class.lookupLiteral("Magnitude"));
    try expectEqual(Number_I,class.lookupLiteral("Number"));
}
