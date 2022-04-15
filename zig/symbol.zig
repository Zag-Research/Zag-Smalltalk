const std = @import("std");
const object = @import("object.zig");
const Nil = object.Nil;
const heap = @import("heap.zig");
const treap = @import("treap.zig");
const thread = @import("thread.zig");
pub inline fn symbol_of(index: u64, arity: u64) object.Object {
    return symbol0(index|(arity<<24));
}
pub inline fn symbol0(index: u64) object.Object {
    return @bitCast(object.Object,index|(0x7ffd<<49));
}
pub const valueWithArguments_ = symbol_of(1,1);
pub const cull_ = symbol_of(2,1);
pub const cull_cull_ = symbol_of(3,2);
pub const cull_cull_cull_ = symbol_of(4,3);
pub const cull_cull_cull_cull_ = symbol_of(5,4);
pub const value = symbol0(6);
pub const value_ = symbol_of(7,1);
pub const value_value_ = symbol_of(8,2);
pub const value_value_value_ = symbol_of(9,3);
pub const value_value_value_value_ = symbol_of(10,4);
pub const self = symbol0(11);
pub const Object = symbol0(12);
pub const BlockClosure = symbol0(13);
pub const False = symbol0(14);
pub const True = symbol0(15);
pub const UndefinedObject = symbol0(16);
pub const SmallInteger = symbol0(17);
pub const Symbol = symbol0(18);
pub const Character = symbol0(19);
pub const Float = symbol0(20);
pub const Array = symbol0(21);
pub const String = symbol0(22);
pub const Class = symbol0(23);
pub const Metaclass = symbol0(24);
pub const Behavior = symbol0(25);
pub const Magnitude = symbol0(26);
pub const Number = symbol0(27);
pub const Method = symbol0(28);
pub const System = symbol0(29);
pub const Return = symbol0(30);
pub const Send = symbol0(31);
pub const Literal = symbol0(32);
pub const Load = symbol0(33);
pub const Store = symbol0(34);
pub const SymbolTable = symbol0(35);
pub const Dispatch = symbol0(36);
pub const yourself = symbol0(37);
pub const @"==" = symbol_of(38,1);
pub const @"~~" = symbol_of(39,1);
pub const @"~=" = symbol_of(40,1);
pub const @"=" = symbol_of(41,1);
pub const @"+" = symbol_of(42,1);
pub const @"-" = symbol_of(43,1);
pub const @"*" = symbol_of(44,1);
pub const size = symbol0(45);
pub const negated = symbol0(46);

var symbolTable : *Symbol_Table = undefined;

pub fn init(thr: thread.Thread, initialSymbolTableSize:usize) void {
    var arena = thr.getArena().getGlobal();
    symbolTable = try Symbol_Table.init(arena,initialSymbolTableSize);
    symbolTable.loadInitialSymbols(&arena);
}
pub fn deinit(thr: thread.Thread) void {
    defer symbol.deinit();
}
const objectTreap = treap.Treap(object.Object);
fn numArgs(obj: object.Object) u32 {
    const string = obj.arrayAsSlice(u8);
    if (string.len==0) return 0;
    const first = string[0];
    if (first<'A' or (first>'Z' and first<'a') or first>'z') return 1;
    var count : u32 = 0;
    for (string) |char| {
        if (char==':') count +=1;
    }
    return count;
}
const Symbol_Table = struct {
    theObject: object.Object,
    const Self = @This();
    fn init(arena: *heap.Arena, initialSymbolTableSize:usize) !Self {
        var theHeapObject = try arena.allocObject(@truncate(u16,SymbolTable.fullHash()),
                                                  heap.Format.none,0,initialSymbolTableSize*2);
        _ = objectTreap.init(theHeapObject.arrayAsSlice(u8),object.compareObject,Nil);
        return Symbol_Table {
            .theObject = theHeapObject.asObject(),
        };
    }
    fn deinit(s: *Self) void {
        s.*=undefined;
    }
    fn lookupLiteral(s: *Self, string: []const u8) object.Object {
        var buffer: [200]u8 align(8)= undefined;
        var tempArena = heap.tempArena(&buffer);
        var str = tempArena.allocString(string) catch unreachable;
        return s.lookup(str.asObject());
    }
    fn lookup(s: *Self,string: object.Object) object.Object {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject);
        return lookupDirect(&trp,string);
    }
    fn lookupDirect(trp: *objectTreap, string: object.Object) object.Object {
        const index = trp.lookup(string);
        if (index>0) {
            const nArgs = numArgs(string);
            return symbol_of(index,nArgs);
        }
        return Nil;
    }
    fn internLiteral(s: *Self,arena: *heap.Arena, string: []const u8) object.Object {
        var buffer: [200]u8 align(8)= undefined;
        var tempArena = heap.tempArena(&buffer);
        const str = tempArena.allocString(string) catch unreachable;
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject);
        return internDirect(arena.etGlobal(),&trp,str.asObject());
    }
    fn intern(s: *Self,thr: thread.Thread,string: object.Object) object.Object {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject);
        const arena = thr.getArena().getGlobal();
        while (true) {
            const lu = lookupDirect(&trp,string);
            if (!lu.is_nil()) return lu;
            const result = s.internDirect(arena,&trp,string);
            if (!result.is_nil()) return result;
            unreachable; // out of space
        }
        unreachable;
    }
    fn internDirect(arena: *heap.Arena, trp: *objectTreap, string: object.Object) object.Object {
        const result = lookupDirect(trp,string);
        if (!result.is_nil()) return result;
        const str = string.promoteTo(arena) catch return Nil;
        const index = trp.insert(str) catch unreachable;
        const nArgs = numArgs(string);
        return symbol_of(index,nArgs);
    }
    fn loadInitialSymbols(s: *Self, arena: *heap.Arena) void {
        var symbols = std.mem.tokenize(
            u8,
\\ valueWithArguments: cull: cull:cull: cull:cull:cull: cull:cull:cull:cull: 
\\ value value: value:value: value:value:value: value:value:value:value:
\\ self
\\ Object BlockClosure False True
\\ UndefinedObject SmallInteger Symbol Character
\\ Float Array String Class Metaclass
\\ Behavior Magnitude Number Method System
\\ Return Send Literal Load Store
\\ SymbolTable Dispatch
\\ yourself == ~~ ~= = + - * size
                ," \n");
        while(symbols.next()) |symbol| {
            _ = s.internLiteral(arena,symbol);
        }
    }
};

test "symbols match initialized symbol table" {
    const expectEqual = std.testing.expectEqual;
    var buffer: [6000]u8 align(8)= undefined;
    var arena = heap.tempArena(&buffer);
    var symbol = try Symbol_Table.init(&arena);
    symbol.loadInitialSymbols(&arena);
    defer symbol.deinit();
    try expectEqual(valueWithArguments_,symbol.lookupLiteral("valueWithArguments:"));
    try expectEqual(cull_,symbol.lookupLiteral("cull:"));
    try expectEqual(cull_cull_,symbol.lookupLiteral("cull:cull:"));
    try expectEqual(cull_cull_cull_,symbol.lookupLiteral("cull:cull:cull:"));
    try expectEqual(cull_cull_cull_cull_,symbol.lookupLiteral("cull:cull:cull:cull:"));
    try expectEqual(value,symbol.lookupLiteral("value"));
    try expectEqual(value_,symbol.lookupLiteral("value:"));
    try expectEqual(value_value_,symbol.lookupLiteral("value:value:"));
    try expectEqual(value_value_value_,symbol.lookupLiteral("value:value:value:"));
    try expectEqual(value_value_value_value_,symbol.lookupLiteral("value:value:value:value:"));
    try expectEqual(self,symbol.lookupLiteral("self"));
    try expectEqual(Object,symbol.lookupLiteral("Object"));
    try expectEqual(BlockClosure,symbol.lookupLiteral("BlockClosure"));
    try expectEqual(False,symbol.lookupLiteral("False"));
    try expectEqual(True,symbol.lookupLiteral("True"));
    try expectEqual(UndefinedObject,symbol.lookupLiteral("UndefinedObject"));
    try expectEqual(SmallInteger,symbol.lookupLiteral("SmallInteger"));
    try expectEqual(Symbol,symbol.lookupLiteral("Symbol"));
    try expectEqual(Character,symbol.lookupLiteral("Character"));
    try expectEqual(Float,symbol.lookupLiteral("Float"));
    try expectEqual(Array,symbol.lookupLiteral("Array"));
    try expectEqual(String,symbol.lookupLiteral("String"));
    try expectEqual(Class,symbol.lookupLiteral("Class"));
    try expectEqual(Metaclass,symbol.lookupLiteral("Metaclass"));
    try expectEqual(Behavior,symbol.lookupLiteral("Behavior"));
    try expectEqual(Method,symbol.lookupLiteral("Method"));
    try expectEqual(Magnitude,symbol.lookupLiteral("Magnitude"));
    try expectEqual(Number,symbol.lookupLiteral("Number"));
    try expectEqual(System,symbol.lookupLiteral("System"));
    try expectEqual(Return,symbol.lookupLiteral("Return"));
    try expectEqual(Send,symbol.lookupLiteral("Send"));
    try expectEqual(Literal,symbol.lookupLiteral("Literal"));
    try expectEqual(Load,symbol.lookupLiteral("Load"));
    try expectEqual(Store,symbol.lookupLiteral("Store"));
    try expectEqual(SymbolTable,symbol.lookupLiteral("SymbolTable"));
    try expectEqual(Dispatch,symbol.lookupLiteral("Dispatch"));
    try expectEqual(yourself,symbol.lookupLiteral("yourself"));
    try expectEqual(@"==",symbol.lookupLiteral("=="));
    try expectEqual(@"~~",symbol.lookupLiteral("~~"));
    try expectEqual(@"~=",symbol.lookupLiteral("~="));
    try expectEqual(@"=",symbol.lookupLiteral("="));
    try expectEqual(@"+",symbol.lookupLiteral("+"));
    try expectEqual(@"-",symbol.lookupLiteral("-"));
    try expectEqual(@"*",symbol.lookupLiteral("*"));
    try expectEqual(size,symbol.lookupLiteral("size"));
}
