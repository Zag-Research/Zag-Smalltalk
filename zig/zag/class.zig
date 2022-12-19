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
    fn init(initialClassTableSize:usize) !Self {
        var theHeapObject = arenas.globalArena.allocObject(ClassTable_I,initialClassTableSize*2);
        _ = objectTreap.init(theHeapObject.arrayAsSlice(u8),compareU32,0);
        return Class_Table {
            .theObject = theHeapObject,
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
        return @truncate(ClassIndex,trp.lookup(sym.hash32()));
    }
    fn intern(s: *Self, sym: object.Object) ClassIndex {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),compareU32,0);
        //const arena = thr.getArena().getGlobal();
        while (true) {
            const lu = s.lookup(sym);
            if (lu>0) return lu;
            const result = @truncate(ClassIndex,trp.insert(sym.hash32()) catch @panic("class treap full"));
            if (result>0) return result;
            unreachable; // out of space
        }
        unreachable;
    }
    fn loadInitialClassNames(s: *Self) void {
        for(initialClassStrings) |name| {
            _ = s.intern(symbol.intern(name.asObject()));
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
pub fn subClass(superclassName: Object, className: Object) !void {
//    const stdout = std.io.getStdOut().writer();
    const class_I = getClassIndex(className);
    var class: *Class_S = undefined;
    var metaclass: *Metaclass_S = undefined;
    if (classes[class_I].isNil()) {
        const metaclass_I = classTable.nextFree();
        metaclass = arenas.globalArena.allocStruct(Metaclass_I, Metaclass_S, 8, Nil);
        classes[metaclass_I] = Object.from(metaclass);
        class = arenas.globalArena.allocStruct(metaclass_I, Class_S, 8, Nil);
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
pub fn init() !void {
    classTable = try Class_Table.init(ReservedNumberOfClasses);
    classTable.loadInitialClassNames();
    try subClass(Nil,symbols.Object);
    try subClass(symbols.Object,symbols.Behavior);
    try subClass(symbols.Behavior,symbols.ClassDescription);
    try subClass(symbols.ClassDescription,symbols.Class);
    try subClass(symbols.ClassDescription,symbols.Metaclass);
    // repeat to set metaclass superclass properly
    try subClass(Nil,symbols.Object);
}
test "classes match initialized class table" {
    var thr = thread.Thread.new();
    thr.init();
    _ = try symbol.init(500,symbol.noStrings);
    try init();
    for(initialClassStrings) |string,idx|
        try std.testing.expectEqual(idx+1,classTable.lookup(symbol.lookup(&thr,string.asObject())));
}
