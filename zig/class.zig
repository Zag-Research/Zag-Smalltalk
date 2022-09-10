const std = @import("std");
const builtin = @import("builtin");
const thread = @import("thread.zig");
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const u64_MINVAL = object.u64_MINVAL;
const symbol = @import("symbol.zig");
const symbols = symbol.symbols;
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
pub const CodeReference_I = c3o + 22;
pub const ReservedNumberOfClasses = if (builtin.is_test) 100 else 500;
var classes = [_]object.Object{Nil} ** ReservedNumberOfClasses;
var classTable : Class_Table = undefined;
const objectTreap = Treap(u32,ClassIndex,u0);
const Class_Table = struct {
    theObject: object.Object,
    const Self = @This();
    fn compareU32(l: u32, r: u32) std.math.Order {
        return std.math.order(l,r);
    }
    fn init(arena: *heap.Arena, initialClassTableSize:usize) !Self {
        var theHeapObject = try arena.allocObject(ClassTable_I,
                                                  heap.Format.none,0,initialClassTableSize*2);
        _ = objectTreap.init(theHeapObject.arrayAsSlice(u8),compareU32,0);
        return Class_Table {
            .theObject = theHeapObject.asObject(),
        };
    }
    fn deinit(s: *Self) void {
        s.*=undefined;
    }
    fn nextFree(s: *Self) ClassIndex {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),compareU32,0);
        return @truncate(ClassIndex,trp.nextFree() catch @panic("class treap full"));
    }
    fn lookup(s: *Self,sym: object.Object) ClassIndex {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),compareU32,0);
        return @truncate(ClassIndex,trp.lookup(sym.hash));
    }
    fn intern(s: *Self, sym: object.Object) ClassIndex {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),compareU32,0);
        //const arena = thr.getArena().getGlobal();
        while (true) {
            const lu = s.lookup(sym);
            if (lu>0) return lu;
            const result = @truncate(ClassIndex,trp.insert(sym.hash) catch @panic("class treap full"));
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
\\ Object SmallInteger Float
\\ False True
\\ UndefinedObject Symbol Character
\\ Array String Class Metaclass
\\ Behavior BlockClosure Method MethodDictionary System
\\ Return Send Literal Load Store
\\ SymbolTable Dispatch ClassTable Magnitude Number ClassDescription
\\ Boolean Context CodeReference
                ," \n");
        while(names.next()) |name| {
            _ = s.intern(symbol.internLiteral(arena,name));
        }
    }
};
const Behavior_S = packed struct {
    header: heap.Header,
    superclass: Object,
    methodDict: Object,
    format: Object,
};
const ClassDescription_S = packed struct {
    super: Behavior_S,
    index: Object,
    organization: Object,
};
pub const Metaclass_S = packed struct{
    pub const ClassIndex = Metaclass_I;
    pub const includesHeader = true;
    const size = @sizeOf(Metaclass_S)-@sizeOf(Object);
    super: ClassDescription_S,
    soleInstance: Object,
};
pub const Class_S = packed struct{
    pub const includesHeader = true;
    const size =  @sizeOf(Class_S)-@sizeOf(Object);
    super: ClassDescription_S,
    name: Object,
    instVarNames: Object,
    classVariables: Object,
    subclasses: Object,
};

pub inline fn getClassIndex(className: Object) ClassIndex {
   return classTable.intern(className);
}
pub fn getClass(className: Object) Object {
    return classes[getClassIndex(className)];
}
pub fn subClass(thr: *thread.Thread,superclassName: Object, className: Object) !void {
//    const stdout = std.io.getStdOut().writer();
    const class_I = getClassIndex(className);
    var class: *Class_S = undefined;
    var metaclass: *Metaclass_S = undefined;
    if (classes[class_I].isNil()) {
        const arena = thr.getArena().getGlobal();
        const metaclass_I = classTable.nextFree();
        metaclass = arena.allocStruct(Metaclass_I, Metaclass_S.size, Metaclass_S, Nil) catch @panic("No space");
        classes[metaclass_I] = Object.from(metaclass);
        class = arena.allocStruct(metaclass_I, Class_S.size, Class_S, Nil) catch @panic("No space");
        const class_O = Object.from(class);
        metaclass.super.index=Object.from(class_I);
        metaclass.soleInstance=class_O;
        class.super.index=Object.from(class_I);
        class.name=className;
        classes[class_I] = class_O;
//        try stdout.print("\nnew ", .{});
    } else {
        class = classes[class_I].to(*Class_S);
//        try stdout.print("\nexisting ", .{});
        metaclass = classes[class.super.super.header.classIndex].to(*Metaclass_S);
    }
//    try stdout.print("subClass {} {} 0x{x:0>16}\n", .{className,class_I,classes[class_I].u()});
    var superclass_I = classTable.lookup(superclassName);
    if (superclass_I==0)
        superclass_I = classTable.lookup(symbols.Class);
    // *****************************
    // * This is not correct
    // *****************************
//    try stdout.print("superclass:{}\n", .{superclass_I});
    _ = @ptrCast(heap.HeapPtr,@alignCast(8,&class.super.super.header)).setHash(superclass_I);
    _ = @ptrCast(heap.HeapPtr,@alignCast(8,&metaclass.super.super.header)).setHash(superclass_I);
//    try stdout.print(" class:{}\n", .{class});
//    try stdout.print(" metaclass:{}\n", .{metaclass});
}
pub fn init_class(t: *thread.Thread, className: Object,  instanceMethods: []const dispatch.SymbolMethod, classMethods: []const dispatch.SymbolMethod) !Object {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("before addClass\n",.{});
    try @import("dispatch.zig").addClass(t,className,instanceMethods,classMethods);
    try stdout.print("before getClass\n",.{});
    return getClass(className);
}
pub fn init(thr: *thread.Thread) !void {
    var arena = thr.getArena().getGlobal();
    classTable = try Class_Table.init(arena,ReservedNumberOfClasses);
    classTable.loadInitialClassNames(arena);
    try subClass(thr,Nil,symbols.Object);
    try subClass(thr,symbols.Object,symbols.Behavior);
    try subClass(thr,symbols.Behavior,symbols.ClassDescription);
    try subClass(thr,symbols.ClassDescription,symbols.Class);
    try subClass(thr,symbols.ClassDescription,symbols.Metaclass);
    // repeat to set metaclass superclass properly
    try subClass(thr,Nil,symbols.Object);
}
test "classes match initialized class table" {
    const expectEqual = std.testing.expectEqual;
    var thr = try thread.Thread.initForTest();
    _ = try symbol.init(&thr,500,"");
    try init(&thr);
    var class = classTable;
    try expectEqual(Object_I,class.lookupLiteral("Object"));
    try expectEqual(False_I,class.lookupLiteral("False"));
    try expectEqual(True_I,class.lookupLiteral("True"));
    try expectEqual(Boolean_I,class.lookupLiteral("Boolean"));
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
