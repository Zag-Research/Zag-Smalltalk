const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const builtin = @import("builtin");
const object = @import("zobject.zig");
const indexSymbol = object.Object.indexSymbol0;
const Nil = object.Nil;
const heap = @import("heap.zig");
const Treap = @import("utilities.zig").Treap;
const inversePhi32 = @import("utilities.zig").inversePhi(u32);
const undoPhi32 = @import("utilities.zig").undoPhi(u32);
pub var globalAllocator = std.heap.page_allocator; //@import("globalArena.zig").allocator();
pub inline fn fromHash32(hash: u32) object.Object {
    return object.Object.makeImmediate(.Symbol, hash);
}
inline fn symbol_of(index: u32, arity: u4) object.Object {
    return fromHash32((index << 5 | @as(u32, arity) << 1 | 1) *% inversePhi32);
}
pub inline fn symbol0(index: u32) object.Object {
    return symbol_of(index, 0);
}
pub inline fn symbol1(index: u32) object.Object {
    return symbol_of(index, 1);
}
pub inline fn symbol2(index: u32) object.Object {
    return symbol_of(index, 2);
}
pub inline fn symbol3(index: u32) object.Object {
    return symbol_of(index, 3);
}
pub inline fn symbol4(index: u32) object.Object {
    return symbol_of(index, 4);
}
pub const symbols = struct {
    pub const @"=" = symbol1(1);
    pub const value = symbol0(2);
    pub const @"value:" = symbol1(3);
    pub const @"cull:" = symbol1(4);
    pub const yourself = symbol0(5);
    pub const @"doesNotUnderstand:" = symbol1(6);
    pub const @"+" = symbol1(7);
    pub const @"-" = symbol1(8);
    pub const @"*" = symbol1(9);
    pub const size = symbol0(10);
    pub const @"at:" = symbol1(11);
    pub const @"at:put:" = symbol2(12);
    pub const @"~=" = symbol1(13);
    pub const @"==" = symbol1(14);
    pub const @"~~" = symbol1(15);
    pub const @"value:value:" = symbol2(16);
    pub const negated = symbol0(17);
    pub const new = symbol0(18);
    pub const @"new:" = symbol0(19);
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
    pub const SmallInteger = symbol0(37);
    pub const noFallback = symbol0(38);
    pub const @"ifTrue:" = symbol1(39);
    pub const @"ifTrue:ifFalse" = symbol2(40);
    pub const @"ifFalse:" = symbol1(41);
    pub const @"ifFalse:ifTrue:" = symbol2(42);
    pub const @"ifNil:" = symbol1(43);
    pub const @"ifNil:ifNotNil" = symbol2(44);
    pub const @"ifNotNil:" = symbol1(45);
    pub const @"ifNotNil:ifNil:" = symbol2(46);
    pub const @"perform:" = symbol1(47);
    pub const @"perform:with:" = symbol2(48);
    pub const @"perform:with:with:" = symbol3(49);
    pub const @"perform:with:with:with:" = symbol4(50);
    pub const @"perform:withArguments:" = symbol2(51);
    pub const @"perform:withArguments:inSuperclass:" = symbol3(52);
    pub const fibonacci = symbol3(53);
    // define any new symbols here
    pub const Object = symbol0(53); // always have this the last initial symbol so the tests verify all the counts are correct
    pub const i_0 = indexSymbol(0);
    pub const i_1 = indexSymbol(1);
    pub const i_2 = indexSymbol(2);
    pub const i_3 = indexSymbol(3);
    pub const i_4 = indexSymbol(4);
    pub const i_5 = indexSymbol(5);
    pub const i_6 = indexSymbol(6);
    pub const i_7 = indexSymbol(7);
    pub const i_8 = indexSymbol(8);
    pub const i_9 = indexSymbol(9);
};
pub const predefinedSymbols = 47;
const initialSymbolStrings = heap.compileStrings(.{ // must be in exactly same order as above
    "=",                        "value",                               "value:",     "cull:",           "yourself",             "doesNotUnderstand:", "+",            "-",             "*",                  "size",
    "at:",                      "at:put:",                             "~=",         "==",              "~~",                   "value:value:",       "negated",      "new",           "new:",               "value:value:value:",
    "value:value:value:value:", "valueWithArguments:",                 "cull:cull:", "cull:cull:cull:", "cull:cull:cull:cull:", "self",               "name",         "<",             "<=",                 ">=",
    ">",                        "class",                               "Class",      "Behavior",        "ClassDescription",     "Metaclass",          "SmallInteger", "noFallback",    "ifTrue:",            "ifTrue:ifFalse",
    "ifFalse:",                 "ifFalse:ifTrue:",                     "ifNil:",     "ifNil:ifNotNil",  "ifNotNil:",            "ifNotNil:ifNil:",    "perform:",     "perform:with:", "perform:with:with:", "perform:with:with:with:",
    "perform:withArguments:",   "perform:withArguments:inSuperclass:", "fibonacci",
    // add any new values here
     "Object",
});
pub var symbolTable = SymbolTable.init(&globalAllocator);
pub fn asString(string: object.Object) object.Object {
    return symbolTable.asString(string);
}
pub fn loadSymbols(strs: []const heap.HeapConstPtr) void {
    symbolTable.loadSymbols(strs);
}
pub inline fn lookup(string: object.Object) object.Object {
    return symbolTable.lookup(string);
}
pub inline fn intern(string: object.Object) object.Object {
    return symbolTable.intern(string);
}
const ObjectTreap = Treap(object.Object, u32, u0);
fn numArgs(obj: object.Object) u4 {
    const string = obj.arrayAsSlice(u8);
    if (string.len == 0) return 0;
    const first = string[0];
    if (first < 'A' or (first > 'Z' and first < 'a') or first > 'z') return 1;
    var count: u4 = 0;
    for (string) |char| {
        if (char == ':') count += 1;
    }
    return count;
}
pub const SymbolTable = struct {
    mem: []ObjectTreap.Element,
    treap: ObjectTreap,
    allocator: *Allocator,
    const Self = @This();
    const initialSymbolTableSize = 50;
    pub fn init(allocator: *Allocator) Self {
        return SymbolTable{
            .mem = &[0]ObjectTreap.Element{},
            .treap = ObjectTreap.initEmpty(object.compareObject, Nil),
            .allocator = allocator,
        };
    }
    inline fn theTreap(self: *Self, adding: usize) *ObjectTreap {
        if (self.treap.hasRoom(adding))
            return &self.treap;
        return self.allocTreap(adding);
    }
    fn allocTreap(self: *Self, _: usize) *ObjectTreap {
        {
            // ToDo: add locking
            const size = heap.growSize(self.mem, ObjectTreap.Element) catch initialSymbolTableSize * ObjectTreap.elementSize;
            const memory = self.allocator.alloc(ObjectTreap.Element, size) catch @panic("can't alloc");
            self.treap.resize(memory);
            self.allocator.free(self.mem);
            self.mem = memory;
        }
        self.loadSymbols(initialSymbolStrings[0..initialSymbolStrings.len]);
        return &self.treap;
    }
    fn deinit(self: *Self) void {
        self.allocator.free(self.mem);
        self.* = undefined;
    }
    fn unPhi(obj: object.Object) u32 {
        return @as(u32, @truncate(obj.hash32() >> 8)) *% undoPhi32;
    }
    fn asString(self: *Self, string: object.Object) object.Object {
        return self.theTreap(0).getKey(unPhi(string));
    }
    pub fn lookup(self: *Self, string: object.Object) object.Object {
        return lookupDirect(self.theTreap(0), string);
    }
    fn lookupDirect(trp: *ObjectTreap, string: object.Object) object.Object {
        const index = trp.lookup(string);
        if (index > 0) {
            const nArgs = numArgs(string);
            return symbol_of(index, nArgs);
        }
        return Nil;
    }
    fn intern(self: *Self, string: object.Object) object.Object {
        const trp = self.theTreap(1);
        while (true) {
            const lu = lookupDirect(trp, string);
            if (!lu.isNil()) return lu;
            const result = internDirect(trp, string);
            if (!result.isNil()) return result;
            unreachable; // out of space
        }
        unreachable;
    }
    fn internDirect(trp: *ObjectTreap, string: object.Object) object.Object {
        const result = lookupDirect(trp, string);
        if (!result.isNil()) return result;
        const str = string.promoteTo() catch return Nil;
        const index = trp.insert(str) catch unreachable;
        const nArgs = numArgs(string);
        return symbol_of(index, nArgs);
    }
    fn loadSymbols(self: *Self, strings: []const heap.HeapObjectConstPtr) void {
        const trp = self.theTreap(strings.len);
        for (strings) |string|
            _ = internDirect(trp, string.asObject());
    }
    fn verify(self: *Self, symbol: object.Object) !void {
        try std.testing.expectEqual(symbol, self.lookup(initialSymbolStrings[unPhi(symbol) - 1].asObject()));
    }
};
pub const noStrings = &[0]heap.HeapConstPtr{};
// test "symbols match initialized symbol table" {
//     const expectEqual = std.testing.expectEqual;
//     const expect = std.testing.expect;
//     var symbol = SymbolTable.init(&globalAllocator);
//     defer symbol.deinit();
//     symbol.loadSymbols(initialSymbolStrings[0 .. initialSymbolStrings.len - 1]);
//     const trp = symbol.theTreap(0);
//     try expectEqual(symbols.Object, SymbolTable.internDirect(trp, initialSymbolStrings[initialSymbolStrings.len - 1].asObject()));
//     for (initialSymbolStrings, 0..) |string, idx|
//         try expectEqual(symbol_of(@intCast(idx+1),0).hash32(), symbol.lookup(string.asObject()).hash32());
//     // test a few at random to verify arity
//     try symbol.verify(symbols.@"cull:");
//     try symbol.verify(symbols.@"cull:cull:");
//     try symbol.verify(symbols.@"cull:cull:cull:");
//     try symbol.verify(symbols.@"cull:cull:cull:cull:");
//     try symbol.verify(symbols.value);
//     try symbol.verify(symbols.@"+");
//     try symbol.verify(symbols.size);
//     try symbol.verify(symbols.Object);
//     try expect(mem.eql(u8, "valueWithArguments:"[0..], symbol.asString(symbols.@"valueWithArguments:").arrayAsSlice(u8)));
// }
test "force second allocation of symbol treap" {
    const moreSymbolStrings = heap.compileStrings(.{
        "xxx00", "xxx01", "xxx02", "xxx03", "xxx04", "xxx05", "xxx06", "xxx07", "xxx08", "xxx09",
        "xxx10", "xxx11", "xxx12", "xxx13", "xxx14", "xxx15", "xxx16", "xxx17", "xxx18", "xxx19",
        "xxx20", "xxx21", "xxx22", "xxx23", "xxx24", "xxx25", "xxx26", "xxx27", "xxx28", "xxx29",
        "xxx30", "xxx31", "xxx32", "xxx33", "xxx34", "xxx35", "xxx36", "xxx37", "xxx38", "xxx39",
    });
    //    const expectEqual = std.testing.expectEqual;
    //    const expect = std.testing.expect;
    var symbol = SymbolTable.init(&globalAllocator);
    defer symbol.deinit();
    symbol.loadSymbols(initialSymbolStrings[0 .. initialSymbolStrings.len - 1]);
    symbol.loadSymbols(moreSymbolStrings[0 .. moreSymbolStrings.len - 1]);
    //_ = symbol.allocator.allocArray(49,480,u8);
}
