const std = @import("std");
const builtin = @import("builtin");
const object = @import("object.zig");
const Nil = object.Nil;
const class = @import("class.zig");
const heap = @import("heap.zig");
const treap = @import("treap.zig");
const thread = @import("thread.zig");
inline fn symbol_of(index: u64, arity: u64) object.Object {
    return symbol0(index|(arity<<24));
}
pub inline fn symbol0(index: u64) object.Object {
    return @bitCast(object.Object,index|((@as(u64,0xfff700)+class.Symbol_I)<<40));
}
pub inline fn symbol1(index: u64) object.Object {
    return symbol_of(index,1);
}
pub inline fn symbol2(index: u64) object.Object {
    return symbol_of(index,2);
}
pub inline fn symbol3(index: u64) object.Object {
    return symbol_of(index,3);
}
pub inline fn symbol4(index: u64) object.Object {
    return symbol_of(index,4);
}
pub const symbols = struct {
    pub const yourself = symbol0(1);
    pub const @"doesNotUnderstand:" = symbol1(2);
    pub const @"=" = symbol1(3);
    pub const @"+" = symbol1(4);
    pub const @"-" = symbol1(5);
    pub const @"*" = symbol1(6);
    pub const size = symbol0(7);
    pub const @"at:" = symbol0(8);
    pub const @"at:put:" = symbol0(9);
    pub const @"~=" = symbol1(10);
    pub const @"==" = symbol1(11);
    pub const @"~~" = symbol1(12);
    pub const value = symbol0(13);
    pub const @"value:" = symbol1(14);
    pub const @"value:value:" = symbol2(15);
    pub const negated = symbol0(16);
    pub const new = symbol0(17);
    pub const @"new:" = symbol0(18);
    pub const @"cull:" = symbol1(19);
    pub const @"value:value:value:" = symbol3(20);
    pub const @"value:value:value:value:" = symbol4(21);
    pub const @"valueWithArguments:" = symbol1(22);
    pub const @"cull:cull:" = symbol2(23);
    pub const @"cull:cull:cull:" = symbol3(24);
    pub const @"cull:cull:cull:cull:" = symbol4(25);
    pub const self = symbol0(26);
    pub const Object = symbol0(27);
    pub const BlockClosure = symbol0(28);
    pub const False = symbol0(29);
    pub const True = symbol0(30);
    pub const UndefinedObject = symbol0(31);
    pub const SmallInteger = symbol0(32);
    pub const Symbol = symbol0(33);
    pub const Character = symbol0(34);
    pub const Float = symbol0(35);
    pub const Array = symbol0(36);
    pub const String = symbol0(37);
    pub const Class = symbol0(38);
    pub const Metaclass = symbol0(39);
    pub const Behavior = symbol0(40);
    pub const Magnitude = symbol0(41);
    pub const Number = symbol0(42);
    pub const Method = symbol0(43);
    pub const System = symbol0(44);
    pub const Return = symbol0(45);
    pub const Send = symbol0(46);
    pub const Literal = symbol0(47);
    pub const Load = symbol0(48);
    pub const Store = symbol0(49);
    pub const SymbolTable = symbol0(50);
    pub const Dispatch = symbol0(51);
    pub const ClassTable = symbol0(52);
    pub const ClassDescription = symbol0(53);
};
var symbolTable : Symbol_Table = undefined;

pub fn init(thr: *thread.Thread, initialSymbolTableSize:usize,str:[]const u8) !void {
    var arena = thr.getArena().getGlobal();
    symbolTable = try Symbol_Table.init(arena,initialSymbolTableSize);
    symbolTable.loadInitialSymbols(arena);
    symbolTable.loadSymbols(arena,str);
}
pub fn deinit(thr: *thread.Thread) void {
    _ = thr;
    symbolTable.deinit();
}
pub fn loadSymbols(thr: *thread.Thread,str:[]const u8) void {
    var arena = thr.getArena().getGlobal();
    symbolTable.loadSymbols(arena,str);
}
pub fn lookupLiteral(string: []const u8) object.Object {
    return symbolTable.lookupLiteral(string);
}
pub fn internLiteral(arena: *heap.Arena, string: []const u8) object.Object {
    const result = symbolTable.internLiteral(arena, string);
    if (!result.is_nil()) return result;
    unreachable; // out of space
}
pub fn intern(thr: *thread.Thread,string: object.Object) object.Object {
    return symbolTable.intern(thr,string);
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
        var theHeapObject = try arena.allocObject(class.SymbolTable_I,
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
        return internDirect(arena.getGlobal(),&trp,str.asObject());
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
        s.loadSymbols(
            arena,
\\  yourself doesNotUnderstand: = + - * size at: at:put: ~= == ~~ value
\\ value: value:value: negated new new: cull: value:value:value:
\\ value:value:value:value: valueWithArguments: cull:cull:
\\ cull:cull:cull: cull:cull:cull:cull: self Object BlockClosure False
\\ True UndefinedObject SmallInteger Symbol Character Float Array String
\\ Class Metaclass Behavior Magnitude Number Method System Return Send
\\ Literal Load Store SymbolTable Dispatch ClassTable ClassDescription
        );
    }
    fn loadSymbols(s: *Self, arena: *heap.Arena,str:[]const u8) void {
        var symbolsToDefine = std.mem.tokenize(u8,str," \n");
        while(symbolsToDefine.next()) |symbol| {
            _ = s.internLiteral(arena,symbol);
        }
    }
};

test "symbols match initialized symbol table" {
    const expectEqual = std.testing.expectEqual;
    var buffer: [6000]u8 align(8)= undefined;
    var arena = heap.tempArena(&buffer);
    var symbol = try Symbol_Table.init(&arena,250);
    symbol.loadInitialSymbols(&arena);
    defer symbol.deinit();
    try expectEqual(symbols.@"valueWithArguments:",symbol.lookupLiteral("valueWithArguments:"));
    try expectEqual(symbols.@"cull:",symbol.lookupLiteral("cull:"));
    try expectEqual(symbols.@"cull:cull:",symbol.lookupLiteral("cull:cull:"));
    try expectEqual(symbols.@"cull:cull:cull:",symbol.lookupLiteral("cull:cull:cull:"));
    try expectEqual(symbols.@"cull:cull:cull:cull:",symbol.lookupLiteral("cull:cull:cull:cull:"));
    try expectEqual(symbols.value,symbol.lookupLiteral("value"));
    try expectEqual(symbols.@"value:",symbol.lookupLiteral("value:"));
    try expectEqual(symbols.@"value:value:",symbol.lookupLiteral("value:value:"));
    try expectEqual(symbols.@"value:value:value:",symbol.lookupLiteral("value:value:value:"));
    try expectEqual(symbols.@"value:value:value:value:",symbol.lookupLiteral("value:value:value:value:"));
    try expectEqual(symbols.self,symbol.lookupLiteral("self"));
    try expectEqual(symbols.Object,symbol.lookupLiteral("Object"));
    try expectEqual(symbols.BlockClosure,symbol.lookupLiteral("BlockClosure"));
    try expectEqual(symbols.False,symbol.lookupLiteral("False"));
    try expectEqual(symbols.True,symbol.lookupLiteral("True"));
    try expectEqual(symbols.UndefinedObject,symbol.lookupLiteral("UndefinedObject"));
    try expectEqual(symbols.SmallInteger,symbol.lookupLiteral("SmallInteger"));
    try expectEqual(symbols.Symbol,symbol.lookupLiteral("Symbol"));
    try expectEqual(symbols.Character,symbol.lookupLiteral("Character"));
    try expectEqual(symbols.Float,symbol.lookupLiteral("Float"));
    try expectEqual(symbols.Array,symbol.lookupLiteral("Array"));
    try expectEqual(symbols.String,symbol.lookupLiteral("String"));
    try expectEqual(symbols.Class,symbol.lookupLiteral("Class"));
    try expectEqual(symbols.Metaclass,symbol.lookupLiteral("Metaclass"));
    try expectEqual(symbols.Behavior,symbol.lookupLiteral("Behavior"));
    try expectEqual(symbols.Method,symbol.lookupLiteral("Method"));
    try expectEqual(symbols.Magnitude,symbol.lookupLiteral("Magnitude"));
    try expectEqual(symbols.Number,symbol.lookupLiteral("Number"));
    try expectEqual(symbols.System,symbol.lookupLiteral("System"));
    try expectEqual(symbols.Return,symbol.lookupLiteral("Return"));
    try expectEqual(symbols.Send,symbol.lookupLiteral("Send"));
    try expectEqual(symbols.Literal,symbol.lookupLiteral("Literal"));
    try expectEqual(symbols.Load,symbol.lookupLiteral("Load"));
    try expectEqual(symbols.Store,symbol.lookupLiteral("Store"));
    try expectEqual(symbols.SymbolTable,symbol.lookupLiteral("SymbolTable"));
    try expectEqual(symbols.Dispatch,symbol.lookupLiteral("Dispatch"));
    try expectEqual(symbols.yourself,symbol.lookupLiteral("yourself"));
    try expectEqual(symbols.@"==",symbol.lookupLiteral("=="));
    try expectEqual(symbols.@"~~",symbol.lookupLiteral("~~"));
    try expectEqual(symbols.@"~=",symbol.lookupLiteral("~="));
    try expectEqual(symbols.@"=",symbol.lookupLiteral("="));
    try expectEqual(symbols.@"+",symbol.lookupLiteral("+"));
    try expectEqual(symbols.@"-",symbol.lookupLiteral("-"));
    try expectEqual(symbols.@"*",symbol.lookupLiteral("*"));
    try expectEqual(symbols.size,symbol.lookupLiteral("size"));
    try expectEqual(symbols.ClassTable,symbol.lookupLiteral("ClassTable"));
}
