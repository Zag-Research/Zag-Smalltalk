const std = @import("std");
const builtin = @import("builtin");
const process = @import("process.zig");
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const symbol = @import("symbol.zig");
const symbols = symbol.symbols;
const globalAllocator = symbol.globalAllocator;
const heap = @import("heap.zig");
const Treap = @import("utilities.zig").Treap;
const assert = std.debug.assert;
fn consistent(l: object.Level2, i: ClassIndex) void {
    assert(@intFromEnum(l) == i);
}
test "consistency" {
    assert(object.ClassIndex == ClassIndex);
    consistent(.Object, Object_I);
    consistent(.SmallInteger, SmallInteger_I);
}
pub const ClassIndex = u16; // only allows 65535 classes and this size is baked into a few places, but Pharo has fewer than 18000 (including metaclasses), so shouldn't be a problem
pub const Object_I = object.Object_I;
pub const SmallInteger_I = object.SmallInteger_I;
pub const Float_I = object.Float_I;
pub const False_I = object.False_I;
pub const True_I = object.True_I;
pub const UndefinedObject_I = object.UndefinedObject_I;
pub const Symbol_I = object.Symbol_I;
pub const Character_I = object.Character_I;
pub const BlockClosure_I = object.BlockClosure_I;
pub const Array_I = object.Array_I;
pub const String_I = object.String_I;
pub const SymbolTable_I = object.SymbolTable_I;
pub const Method_I = object.Method_I;
pub const CompiledMethod_I = object.CompiledMethod_I;
pub const Dispatch_I = object.Dispatch_I;
pub const ClosureData_I = object.ClosureData_I;
pub const Context_I = object.Context_I;
const c3o = 18;
pub const Class_I = c3o + 0;
pub const Metaclass_I = c3o + 1;
pub const Behavior_I = c3o + 2;
pub const MethodDictionary_I = c3o + 3;
pub const System_I = c3o + 4;
pub const Return_I = c3o + 5;
pub const Send_I = c3o + 6;
pub const Literal_I = c3o + 7;
pub const Load_I = c3o + 8;
pub const Store_I = c3o + 9;
pub const ClassTable_I = c3o + 10;
pub const Magnitude_I = c3o + 11;
pub const Number_I = c3o + 12;
pub const ClassDescription_I = c3o + 13;
pub const Boolean_I = c3o + 14;
pub const CatchingContext_I = c3o + 15;
pub const EnsuringContext_I = c3o + 16;
pub const ByteArray_I = c3o + 17;
const initialClassStrings = heap.compileStrings(.{ // must be in same order as above
    "Object",           "SmallInteger", "Float",           "False",           "True",
    "UndefinedObject",  "Symbol",       "Character",       "BlockClosure",    "Array",
    "String",           "SymbolTable",  "Method",          "CompiledMethod",  "Dispatch",
    "ClosureData",      "Context",      "Class",           "Metaclass",       "Behavior",
    "MethodDictionary", "System",       "Return",          "Send",            "Literal",
    "Load",             "Store",        "ClassTable",      "Magnitude",       "Number",
    "ClassDescription", "Boolean",      "CatchingContext", "EnsuringContext", "ByteArray",
});
pub const ReservedNumberOfClasses = if (builtin.is_test) 60 else 500;
var classTable: ClassTable = undefined;
pub fn init() !void {
    classTable = try setUpClassTable(&symbol.symbolTable);
}
const objectTreap = Treap(u32, ClassIndex, u0);
const ClassTable = struct {
    theObject: Object,
    treap: objectTreap,
    symbolTable: *symbol.SymbolTable,
    classes: [ReservedNumberOfClasses]Object,
    const Self = @This();
    fn compareU32(l: u32, r: u32) std.math.Order {
        return std.math.order(l, r);
    }
    fn init(st: *symbol.SymbolTable) Self {
        return ClassTable{
            .theObject = Nil,
            .treap = objectTreap.initEmpty(compareU32, 0),
            .symbolTable = st,
            .classes = [_]Object{Nil} ** ReservedNumberOfClasses,
        };
    }
    inline fn theTreap(self: *Self, adding: usize) *objectTreap {
        if (self.treap.hasRoom(adding))
            return &self.treap;
        return self.allocTreap(adding);
    }
    fn allocTreap(self: *Self, _: usize) *objectTreap {
        {
            // ToDo: add locking
            const size = self.theObject.growSize(objectTreap.elementSize) catch ReservedNumberOfClasses * objectTreap.elementSize;
            var context = @import("context.zig").Context.init();
            var newHeapObject = self.symbolTable.arena.allocArray(ClassTable_I, size, u8, &context);
            const memory = newHeapObject.arrayAsSlice(u8);
            const newTreap = self.treap.resize(memory);
            self.treap = newTreap;
            if (Nil.equals(self.theObject)) {
                self.theObject = newHeapObject;
                for (initialClassStrings) |name| {
                    _ = self.intern(self.symbolTable.intern(name.asObject()));
                }
            } else {
                self.theObject = newHeapObject;
            }
        }
        return &self.treap;
    }
    fn deinit(self: *Self) void {
        self.* = undefined;
    }
    fn nextFree(self: *Self) ClassIndex {
        return self.theTreap(1).nextFree() catch unreachable;
    }
    fn lookup(self: *Self, sym: Object) ClassIndex {
        return self.theTreap(0).lookup(sym.hash32());
    }
    fn intern(self: *Self, sym: Object) ClassIndex {
        var trp = self.theTreap(1);
        while (true) {
            const lu = self.lookup(sym);
            if (lu > 0) return lu;
            const result = trp.insert(sym.hash32()) catch unreachable;
            if (result > 0) return result;
            unreachable; // out of space
        }
        unreachable;
    }
    pub inline fn getClassIndex(self: *Self, className: Object) ClassIndex {
        return self.intern(className);
    }
    pub fn getClass(self: *Self, className: Object) Object {
        return self.classes[self.getClassIndex(className)];
    }
    pub fn subClass(self: *Self, superclass: ?*Metaclass_S, className: Object, context: @import("context.zig").ContextPtr) *Metaclass_S {
        const class_I = self.getClassIndex(className);
        var class: *Class_S = undefined;
        var metaclass: *Metaclass_S = undefined;
        if (self.classes[class_I].isNil()) {
            const metaclass_I = self.nextFree();
            metaclass = self.symbolTable.arena.allocStruct(Metaclass_I, Metaclass_S, 8, Object, context);
            self.classes[metaclass_I] = Object.from(metaclass);
            class = self.symbolTable.arena.allocStruct(Metaclass_I, Class_S, 8, Object, context);
            const class_O = Object.from(class);
            metaclass.super.index = Object.from(class_I);
            metaclass.soleInstance = class_O;
            class.super.index = Object.from(class_I);
            class.name = className;
            self.classes[class_I] = class_O;
        } else {
            class = self.classes[class_I].to(*Class_S);
            metaclass = self.classes[class.super.super.header.classIndex].to(*Metaclass_S);
        }
        if (superclass) |sc| {
            metaclass.super.super.superclass = Object.from(sc);
            class.super.super.superclass = sc.soleInstance;
        } else {
            metaclass.super.super.superclass = Nil;
            class.super.super.superclass = Nil;
        }
        return metaclass;
    }
};
const Behavior_S = extern struct {
    header: heap.HeapObject,
    superclass: Object,
    methodDict: Object,
    format: Object,
};
const ClassDescription_S = extern struct {
    super: Behavior_S,
    index: Object,
    organization: Object,
    pub const includesHeader = true;
};
pub const Metaclass_S = extern struct {
    pub const ClassIndex = Metaclass_I;
    pub const includesHeader = true;
    const size = @sizeOf(Metaclass_S) - @sizeOf(Object);
    super: ClassDescription_S,
    soleInstance: Object,
};
pub const Class_S = extern struct {
    pub const includesHeader = true;
    const size = @sizeOf(Class_S) - @sizeOf(Object);
    super: ClassDescription_S,
    name: Object,
    instVarNames: Object,
    classVariables: Object,
    subclasses: Object,
};

fn setUpClassTable(st: *symbol.SymbolTable) ClassTable {
    var ct = ClassTable.init(st);
    var context = @import("context.zig").Context.init();
    var obj = ct.subClass(null, symbols.Object, &context);
    const behavior = ct.subClass(obj, symbols.Behavior, &context);
    const classDescription = ct.subClass(behavior, symbols.ClassDescription, &context);
    _ = ct.subClass(classDescription, symbols.Class, &context);
    _ = ct.subClass(classDescription, symbols.Metaclass, &context);
    // repeat to set metaclass superclass properly
    obj.super.super.superclass = behavior.soleInstance;
    return ct;
}
test "classes match initialized class table" {
    var thr = process.Process.new();
    thr.init();
    var st = symbol.symbolTable;
    var ct = setUpClassTable(&st);
    for (initialClassStrings, 0..) |string, idx| {
        try std.testing.expectEqual(idx + 1, ct.lookup(st.lookup(string.asObject())));
    }
}
