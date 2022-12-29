const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const object = @import("object.zig");
const Nil = object.Nil;
const class = @import("class.zig");
const heap = @import("heap.zig");
const Treap = @import("utilities.zig").Treap;
const thread = @import("thread.zig");
const arenas = @import("arenas.zig");
const GlobalArena = arenas.GlobalArena;
inline fn symbol_of(index: u32, arity: u8) object.Object {
    return symbol0(index|(@as(u32,arity)<<24));
}
pub inline fn symbol0(index: u32) object.Object {
    return @bitCast(object.Object,index|((@as(u64,0xfff20000)+class.Symbol_I)<<32));
}
pub inline fn symbol1(index: u24) object.Object {
    return symbol_of(index,1);
}
pub inline fn symbol2(index: u24) object.Object {
    return symbol_of(index,2);
}
pub inline fn symbol3(index: u24) object.Object {
    return symbol_of(index,3);
}
pub inline fn symbol4(index: u24) object.Object {
    return symbol_of(index,4);
}
pub fn uniqueSymbol(uniqueNumber:u24) object.Object {
    return symbol0(uniqueNumber|@as(u32,0xff000000));
}
pub const symbols = struct {
    pub const yourself = symbol0(1);
    pub const @"doesNotUnderstand:" = symbol1(2);
    pub const @"=" = symbol1(3);
    pub const @"+" = symbol1(4);
    pub const @"-" = symbol1(5);
    pub const @"*" = symbol1(6);
    pub const size = symbol0(7);
    pub const @"at:" = symbol1(8);
    pub const @"at:put:" = symbol2(9);
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
    pub const name = symbol0(27);
    pub const @"<" = symbol1(28);
    pub const @"<=" = symbol1(29);
    pub const @">=" = symbol1(30);
    pub const @">" = symbol1(31);
    pub const class = symbol0(32);
    pub const Class = symbol0(33);
    pub const Behavior = symbol0(34);
    pub const ClassDescription = symbol0(35);
    pub const Metaclass = symbol0(36);
    // define any new symbols here
    pub const Object = symbol0(37); // always have this the last initial symbol so the tests verify all the counts are correct
};
pub const predefinedSymbols = 37;
const initialSymbolStrings = heap.compileStrings(.{ // must be in exactly same order as above
    "yourself", "doesNotUnderstand:", "=", "+", "-", "*", "size",
    "at:", "at:put:", "~=", "==", "~~", "value", "value:",
    "value:value:", "negated", "new", "new:", "cull:",
    "value:value:value:", "value:value:value:value:",
    "valueWithArguments:", "cull:cull:", "cull:cull:cull:",
    "cull:cull:cull:cull:", "self", "name", "<", "<=", ">=", ">",
    "class", "Class", "Behavior", "ClassDescription", "Metaclass",
    // add any new values here
    "Object"
});
var symbolTable = SymbolTable.init(&arenas.globalArena);

pub fn asString(string: object.Object) object.Object {
    return symbolTable.asString(string);
}
pub fn loadSymbols(_: *thread.Thread,str:[]const heap.HeapConstPtr) void {
    symbolTable.loadSymbols(str);
}
pub inline fn lookup(_: *thread.Thread,string: object.Object) object.Object {
    return symbolTable.lookup(string);
}
pub inline fn intern(string: object.Object) object.Object {
    return symbolTable.intern(string);
}
const objectTreap = Treap(object.Object,u32,u0);
fn numArgs(obj: object.Object) u8 {
    const string = obj.arrayAsSlice(u8);
    if (string.len==0) return 0;
    const first = string[0];
    if (first<'A' or (first>'Z' and first<'a') or first>'z') return 1;
    var count : u8 = 0;
    for (string) |char| {
        if (char==':') count +=1;
    }
    return count;
}
pub const SymbolTable = struct {
    theObject: object.Object,
    treap: objectTreap,
    arena: *GlobalArena,
    const Self = @This();
    const initialSymbolTableSize = 50;
    pub fn init(ga: *GlobalArena) Self {
        return SymbolTable {
            .theObject = Nil,
            .treap = objectTreap.initEmpty(object.compareObject,Nil),
            .arena = ga,
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
                catch initialSymbolTableSize*objectTreap.elementSize;
            var newHeapObject = self.arena.allocArray(class.SymbolTable_I,size,u8);
            var memory = newHeapObject.arrayAsSlice(u8);
            var newTreap = self.treap.resize(memory);
            self.treap = newTreap;
            self.theObject = newHeapObject;
        }
        self.loadSymbols(initialSymbolStrings[0..initialSymbolStrings.len]);
        return &self.treap;
    }
    pub fn deinit(self: *Self) void {
        self.*=undefined;
    }
    fn asString(self: *Self,string: object.Object) object.Object {
        return self.theTreap(0).getKey(@truncate(u24,string.hash32()));
    }
    pub fn lookup(self: *Self,string: object.Object) object.Object {
        return lookupDirect(self.theTreap(0),string);
    }
    fn lookupDirect(trp: *objectTreap, string: object.Object) object.Object {
        const index = trp.lookup(string);
        if (index>0) {
            const nArgs = numArgs(string);
            return symbol_of(index,nArgs);
        }
        return Nil;
    }
    pub fn intern(self: *Self,string: object.Object) object.Object {
        var trp = self.theTreap(1);
        while (true) {
            const lu = lookupDirect(trp,string);
            if (!lu.isNil()) return lu;
            const result = internDirect(trp,string);
            if (!result.isNil()) return result;
            unreachable; // out of space
        }
        unreachable;
    }
    fn internDirect(trp: *objectTreap, string: object.Object) object.Object {
        const result = lookupDirect(trp,string);
        if (!result.isNil()) return result;
        const str = string.promoteTo() catch return Nil;
        const index = trp.insert(str) catch unreachable;
        const nArgs = numArgs(string);
        return symbol_of(index,nArgs);
    }
    fn loadSymbols(self: *Self, strings: [] const heap.HeapConstPtr) void {
        var trp = self.theTreap(strings.len);
        for (strings) |string|
            _ = internDirect(trp,string.asObject());
    }
    fn verify(self: *Self, symbol: object.Object) !void {
        std.debug.print("\nverify 0x{x:0>16} {} {}",.{symbol.u(),symbol.hash24(),initialSymbolStrings[symbol.hash24()-1].asObject()});
        try std.testing.expectEqual(symbol,self.lookup(initialSymbolStrings[symbol.hash24()-1].asObject()));
    }
};
pub const noStrings = &[0]heap.HeapConstPtr{};
test "symbols match initialized symbol table" {
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    var symbol = SymbolTable.init(&arenas.globalArena);
    defer symbol.deinit();
    symbol.loadSymbols(initialSymbolStrings[0..initialSymbolStrings.len-1]);
    var trp = symbol.theTreap(0);
    try expectEqual(symbols.Object,SymbolTable.internDirect(trp,initialSymbolStrings[initialSymbolStrings.len-1].asObject()));
    for(initialSymbolStrings) |string,idx|
        try expectEqual(idx+1,symbol.lookup(string.asObject()).hash24());
    // test a few at random to verify arity
    try symbol.verify(symbols.@"cull:");
    try symbol.verify(symbols.@"cull:cull:");
    try symbol.verify(symbols.@"cull:cull:cull:");
    try symbol.verify(symbols.@"cull:cull:cull:cull:");
    try symbol.verify(symbols.value);
    try symbol.verify(symbols.@"+");
    try symbol.verify(symbols.size);
    try symbol.verify(symbols.Object);
    try expect(mem.eql(u8,"valueWithArguments:"[0..],symbol.asString(symbols.@"valueWithArguments:").arrayAsSlice(u8)));
}
