const std = @import("std");
const builtin = @import("builtin");
const thread = @import("thread.zig");
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const u64_MINVAL = object.u64_MINVAL;
const symbol = @import("symbol.zig");
const symbols = symbol.symbols;
const arenas = @import("arenas.zig");
const dispatch = @import("dispatch.zig");
const methodT = dispatch.methodT;
const heap = @import("heap.zig");
const Treap = @import("utilities.zig").Treap;
pub const ClassIndex = u16; // only allows 65535 classes and this size is baked into a few places, but Pharo has fewer than 18000 (including metaclasses), so shouldn't be a problem
pub const Object_I: ClassIndex = 1;
pub const SmallInteger_I: ClassIndex = 2;
pub const Float_I: ClassIndex = 3;
const c2o: ClassIndex = 4;
pub const False_I = c2o+0;
pub const True_I = c2o + 1;
pub const UndefinedObject_I = c2o + 2;
pub const Symbol_I = c2o + 3;
pub const Character_I = c2o + 4;
const c3o = c2o + 5;
pub const Array_I = c3o + 0;
pub const String_I = c3o + 1;
pub const Class_I = c3o + 2;
pub const Metaclass_I = c3o + 3;
pub const Behavior_I = c3o + 4;
pub const BlockClosure_I = c3o + 5;
pub const Method_I = c3o + 6;
pub const MethodDictionary_I = c3o + 7;
pub const System_I = c3o + 8;
pub const Return_I = c3o + 9;
pub const Send_I = c3o + 10;
pub const Literal_I = c3o + 11;
pub const Load_I = c3o + 12;
pub const Store_I = c3o + 13;
pub const SymbolTable_I = c3o + 14;
pub const Dispatch_I = c3o + 15;
pub const ClassTable_I = c3o + 16;
pub const Magnitude_I = c3o + 17;
pub const Number_I = c3o + 18;
pub const ClassDescription_I = c3o + 19;
pub const Boolean_I = c3o + 20;
pub const Context_I = c3o + 21;
pub const CatchingContext_I = c3o + 22;
pub const EnsuringContext_I = c3o + 23;
pub const CompiledMethod_I = c3o + 24;
pub const ByteArray_I = c3o + 25;
const initialClassStrings = heap.compileStrings(.{ // must be in same order as above
    "Object", "SmallInteger", "Float", "False", "True",
    "UndefinedObject", "Symbol", "Character", "Array", "String",
    "Class", "Metaclass", "Behavior", "BlockClosure", "Method",
    "MethodDictionary", "System", "Return", "Send", "Literal", "Load",
    "Store", "SymbolTable", "Dispatch", "ClassTable", "Magnitude",
    "Number", "ClassDescription", "Boolean", "Context",
    "CatchingContext", "EnsuringContext", "CompiledMethod", "ByteArray",
});
pub const ReservedNumberOfClasses = if (builtin.is_test) 60 else 500;
var classTable : ClassTable = undefined;
pub fn init() !void {
    classTable = setUpClassTable();
}
const objectTreap = Treap(u32,ClassIndex,u0);
const ClassTable = struct {
    theObject: Object,
    treap: objectTreap,
    symbolTable: *symbol.SymbolTable,
    classes: [ReservedNumberOfClasses]Object,
    const Self = @This();
    fn compareU32(l: u32, r: u32) std.math.Order {
        return std.math.order(l,r);
    }
    fn init(st: *symbol.SymbolTable) !Self {
        return ClassTable {
            .theObject = Nil,
            .treap = objectTreap.initEmpty(compareU32,0),
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
            const size = self.theObject.growSize(objectTreap.elementSize)
                catch ReservedNumberOfClasses*objectTreap.elementSize;
            var newHeapObject = self.symbolTable.arena.allocArray(ClassTable_I,size,u8);
            var memory = newHeapObject.arrayAsSlice(u8);
            var newTreap = self.treap.resize(memory);
            self.treap = newTreap;
            if (Nil.equals(self.theObject)) {
                self.theObject = newHeapObject;
                for(initialClassStrings) |name| {
                    _ = self.intern(self.symbolTable.intern(name.asObject()));
                }
            } else {
                self.theObject = newHeapObject;
            }
        }
        return &self.treap;
    }
    fn deinit(self: *Self) void {
        self.*=undefined;
    }
    fn nextFree(self: *Self) ClassIndex {
        return self.theTreap(1).nextFree() catch unreachable;
    }
    fn lookup(self: *Self,sym: Object) ClassIndex {
        return self.theTreap(0).lookup(sym.hash32());
    }
    fn intern(self: *Self, sym: Object) ClassIndex {
        var trp = self.theTreap(1);
        while (true) {
            const lu = self.lookup(sym);
            if (lu>0) return lu;
            const result = trp.insert(sym.hash32()) catch unreachable;
            if (result>0) return result;
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
    pub fn subClass(self: *Self, superclass: ?*Metaclass_S, className: Object) *Metaclass_S {
        const class_I = self.getClassIndex(className);
        var class: *Class_S = undefined;
        var metaclass: *Metaclass_S = undefined;
        if (self.classes[class_I].isNil()) {
            const metaclass_I = self.nextFree();
            metaclass = self.symbolTable.arena.allocStruct(Metaclass_I, Metaclass_S, 8, Object);
            self.classes[metaclass_I] = Object.from(metaclass);
            class = self.symbolTable.arena.allocStruct(Metaclass_I, Class_S, 8, Object);
            const class_O = Object.from(class);
            metaclass.super.index=Object.from(class_I);
            metaclass.soleInstance=class_O;
            class.super.index=Object.from(class_I);
            class.name=className;
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
    header: heap.Header,
    superclass: Object,
    methodDict: Object,
    format: Object,
};
const ClassDescription_S = extern struct {
    super: Behavior_S,
    index: Object,
    organization: Object,
};
pub const Metaclass_S = extern struct{
    pub const ClassIndex = Metaclass_I;
    pub const includesHeader = true;
    const size = @sizeOf(Metaclass_S)-@sizeOf(Object);
    super: ClassDescription_S,
    soleInstance: Object,
};
pub const Class_S = extern struct{
    pub const includesHeader = true;
    const size =  @sizeOf(Class_S)-@sizeOf(Object);
    super: ClassDescription_S,
    name: Object,
    instVarNames: Object,
    classVariables: Object,
    subclasses: Object,
};

fn setUpClassTable(st: *symbol.SymbolTable) !ClassTable {
    var ct = try ClassTable.init(st);
    var obj = ct.subClass(null,symbols.Object);
    const behavior = ct.subClass(obj,symbols.Behavior);
    const classDescription = ct.subClass(behavior,symbols.ClassDescription);
    _ = ct.subClass(classDescription,symbols.Class);
    _ = ct.subClass(classDescription,symbols.Metaclass);
    // repeat to set metaclass superclass properly
    obj.super.super.superclass = behavior.soleInstance;
    return ct;
}
test "classes match initialized class table" {
//    var thr = thread.Thread.new();
    //    thr.init();
    var ga = arenas.GlobalArena.init();
    defer ga.deinit();
    var st = symbol.SymbolTable.init(&ga);
    var ct = try setUpClassTable(&st);
    for(initialClassStrings) |string,idx| {
        try std.testing.expectEqual(idx+1,ct.lookup(st.lookup(string.asObject())));
    }
}
