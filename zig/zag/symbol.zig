const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const builtin = @import("builtin");
const zag = @import("zag.zig");
const config = zag.config;
const utilities = zag.utilities;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const Treap = zag.utilities.Treap;
const inversePhi24 = zag.utilities.inversePhi(u24);
const undoPhi24 = zag.utilities.undoPhi(u24);
pub var globalAllocator = std.heap.page_allocator; //@import("globalArena.zig").allocator();

pub const symbols = if (Object.inMemorySymbols) SymbolsEnum else SymbolsStruct;
pub inline fn symbolIndex(obj: object.Object) u24 {
    return obj.hash24() *% undoPhi24;
}
pub inline fn symbolArity(obj: object.Object) u4 {
    return @truncate(obj.hash32() >> 24);
}
inline fn hash_of(index: u24, arity: u4) u32 {
    return @as(u24, index *% inversePhi24) | (@as(u32, arity) << 24);
}

const SymbolsEnum = enum(u16) {
    @"=" = 0x200 + 1,
    value = 2,
    @"value:" = 0x100 + 3,
    @"cull:" = 0x100 + 4,
    yourself = 5,
    @"doesNotUnderstand:" = 0x100 + 6,
    @"at:",
    @"new:",
    @"valueWithArguments:",
    @"ifTrue:",
    @"ifFalse:",
    @"ifNil:",
    @"ifNotNil:",
    @"perform:",
    @"+" = 0x200 + 15,
    @"-",
    @"*",
    @"~=",
    @"==",
    @"~~",
    @"<",
    @"<=",
    @",>=",
    @">",
    @"at:put:",
    @"value:value:",
    @"cull:cull:",
    @"ifTrue:ifFalse",
    @"ifFalse:ifTrue:",
    @"ifNil:ifNotNil",
    @"ifNotNil:ifNil:",
    @"perform:with:",
    @"perform:withArguments:",
    @"value:value:value:" = 0x300 + 34,
    @"cull:cull:cull:",
    @"perform:with:with:",
    @"perform:withArguments:inSuperclass:",
    @"value:value:value:value:" = 0x400 + 38,
    @"cull:cull:cull:cull:",
    @"perform:with:with:with:",
    size = 41,
    negated,
    new,
    self,
    name,
    class,
    Class,
    Behavior,
    ClassDescription,
    Metaclass,
    SmallInteger,
    noFallback,
    Object,
    _,
    const staticSymbols = blk: {
        var symbolArray = [_]object.inMemory.PointedObject{undefined} ** lastPredefinedSymbol;
        const arities = [_]u4{
            1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 2, 1, 1, 1, 2, 0, 0, 0, 3, 4,
            1, 2, 3, 4, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2,
            1, 2, 1, 2, 1, 2, 3, 4, 2, 3, 0,
        };
        for (symbolArray[0..], arities, 1..) |*sym, arity, i|
            initSymbol(sym, i, arity);
        break :blk symbolArray;
    };
    fn initSymbol(sym: *object.inMemory.PointedObject, symbolNumber: u24, arity: u4) void {
        const hash = hash_of(symbolNumber, arity);
        sym.header = HeapHeader{ .classIndex = .Symbol, .hash = @truncate(hash), .format = .notIndexable, .age = .static, .length = 1 };
        sym.data.unsigned = (hash << 8) + 1;
    }
    pub inline fn numArgs(self: SymbolsEnum) u4 {
        return @truncate(@intFromEnum(self) >> 8);
    }
    pub inline fn symbolHash(self: SymbolsEnum) ?u56 {
        return @as(u24,@as(u8,@truncate(@intFromEnum(self)))) *% inversePhi24;
    }
    pub inline fn immediate_class(_: SymbolsEnum) object.ClassIndex {
        return .Symbol;
    }
    pub fn asObject(self: SymbolsEnum) Object {
        const O = packed struct { sym: *const object.inMemory.PointedObject};
        return @bitCast(O{ .sym = &staticSymbols[@as(u8,@truncate(@intFromEnum(self))) - 1]});
    }
    inline fn symbol_of(index: u24, _: u4) Object {
        return @as(SymbolsEnum,@enumFromInt(index)).asObject();
    }

};
const SymbolsStruct = struct {
    pub const @"=" = symbol1(1);
    pub const value = symbol0(2);
    pub const @"value:" = symbol1(3);
    pub const @"cull:" = symbol1(4);
    pub const yourself = symbol0(5);
    pub const @"doesNotUnderstand:" = symbol1(6);
    pub const @"at:" = symbol1(7);
    pub const @"new:" = symbol1(8);
    pub const @"valueWithArguments:" = symbol1(9);
    pub const @"ifTrue:" = symbol1(10);
    pub const @"ifFalse:" = symbol1(11);
    pub const @"ifNil:" = symbol1(12);
    pub const @"ifNotNil:" = symbol1(13);
    pub const @"perform:" = symbol1(14);
    pub const @"+" = symbol1(15);
    pub const @"-" = symbol1(16);
    pub const @"*" = symbol1(17);
    pub const @"~=" = symbol1(18);
    pub const @"==" = symbol1(19);
    pub const @"~~" = symbol1(20);
    pub const @"<" = symbol1(21);
    pub const @"<=" = symbol1(22);
    pub const @",>=" = symbol1(23);
    pub const @">" = symbol1(24);
    pub const @"at:put:" = symbol2(25);
    pub const @"value:value:" = symbol2(26);
    pub const @"cull:cull:" = symbol2(27);
    pub const @"ifTrue:ifFalse" = symbol2(28);
    pub const @"ifFalse:ifTrue:" = symbol2(29);
    pub const @"ifNil:ifNotNil" = symbol2(30);
    pub const @"ifNotNil:ifNil:" = symbol2(31);
    pub const @"perform:with:" = symbol2(32);
    pub const @"perform:withArguments:" = symbol2(33);
    pub const @"value:value:value:" = symbol3(34);
    pub const @"cull:cull:cull:" = symbol3(35);
    pub const @"perform:with:with:" = symbol3(36);
    pub const @"perform:withArguments:inSuperclass:" = symbol3(37);
    pub const @"value:value:value:value:" = symbol4(38);
    pub const @"cull:cull:cull:cull:" = symbol4(39);
    pub const @"perform:with:with:with:" = symbol4(40);
    pub const size = symbol0(41);
    pub const negated = symbol0(42);
    pub const new = symbol0(43);
    pub const self = symbol0(44);
    pub const name = symbol0(45);
    pub const class = symbol0(46);
    pub const Class = symbol0(47);
    pub const Behavior = symbol0(48);
    pub const ClassDescription = symbol0(49);
    pub const Metaclass = symbol0(40);
    pub const SmallInteger = symbol0(51);
    pub const noFallback = symbol0(52);
    pub const Object = symbol0(lastPredefinedSymbol); // always have this the last initial symbol so the tests verify all the counts are correct
    inline fn fromHash32(hash: u32) object.Object {
        return object.Object.makeImmediate(.Symbol, hash);
    }
    inline fn symbol_of(index: u24, arity: u4) object.Object {
        return fromHash32(hash_of(index, arity));
    }
    inline fn symbol0(index: u24) object.Object {
        return symbol_of(index, 0);
    }
    inline fn symbol1(index: u24) object.Object {
        return symbol_of(index, 1);
    }
    inline fn symbol2(index: u24) object.Object {
        return symbol_of(index, 2);
    }
    inline fn symbol3(index: u24) object.Object {
        return symbol_of(index, 3);
    }
    inline fn symbol4(index: u24) object.Object {
        return symbol_of(index, 4);
    }
};
const lastPredefinedSymbol = 53;
comptime {
    std.debug.assert(initialSymbolStrings.len == lastPredefinedSymbol);
}
const initialSymbolStrings = heap.compileStrings(.{ // must be in exactly same order as above
    "=", "value", "value:", "cull:", "yourself", "doesNotUnderstand:",
    "at:", "new:", "valueWithArguments:", "ifTrue:", "ifFalse:",
    "ifNil:", "ifNotNil:", "perform:", "+", "-", "*", "~=", "==",
    "~~", "<", "<=", ",>=", ">", "at:put:", "value:value:",
    "cull:cull:", "ifTrue:ifFalse", "ifFalse:ifTrue:",
    "ifNil:ifNotNil", "ifNotNil:ifNil:", "perform:with:",
    "perform:withArguments:", "value:value:value:", "cull:cull:cull:",
    "perform:with:with:", "perform:withArguments:inSuperclass:",
    "value:value:value:value:", "cull:cull:cull:cull:",
    "perform:with:with:with:", "size", "negated", "new", "self",
    "name", "class", "Class", "Behavior", "ClassDescription",
    "Metaclass", "SmallInteger", "noFallback",
    // add any new values here
    "Object",
});
var symbolTable = SymbolTable.init(&globalAllocator);
pub fn asString(string: Object) Object {
    return symbolTable.asString(string);
}
pub fn loadSymbols(strs: []const heap.HeapConstPtr) void {
    symbolTable.loadSymbols(strs);
}
pub inline fn lookup(string: Object) Object {
    return symbolTable.lookup(string);
}
pub inline fn intern(string: Object) Object {
    return symbolTable.intern(string);
}
const ObjectTreap = Treap(Object, u32, u0);
fn numArgs(obj: Object) u4 {
    const string = obj.arrayAsSlice(u8) catch return 0;
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
            .treap = ObjectTreap.initEmpty(object.compareObject, Nil()),
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
    fn asString(self: *Self, string: Object) Object {
        return self.theTreap(0).getKey(symbolIndex(string));
    }
    pub fn lookup(self: *Self, string: Object) Object {
        return lookupDirect(self.theTreap(0), string);
    }
    fn lookupDirect(trp: *ObjectTreap, string: Object) Object {
        const index = trp.lookup(string);
        if (index > 0) {
            const nArgs = numArgs(string);
            return symbols.symbol_of(@truncate(index), nArgs);
        }
        return Nil();
    }
    fn intern(self: *Self, string: Object) Object {
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
    fn internDirect(trp: *ObjectTreap, string: Object) Object {
        const result = lookupDirect(trp, string);
        if (!result.isNil()) return result;
        const str = string.promoteTo() catch return Nil();
        const index = trp.insert(str) catch unreachable;
        const nArgs = numArgs(string);
        return symbols.symbol_of(@truncate(index), nArgs);
    }
    fn loadSymbols(self: *Self, strings: []const heap.HeapObjectConstPtr) void {
        const trp = self.theTreap(strings.len);
        for (strings) |string|
            _ = internDirect(trp, string.asObject());
    }
    fn verify(self: *Self, symbol: Object) !void {
        const string = initialSymbolStrings[symbolIndex(symbol) - 1].asObject();
        const other = self.lookup(string);
        try std.testing.expectEqual(symbol, other);
    }
};
pub const noStrings = &[0]heap.HeapConstPtr{};
test "symbols match initialized symbol table" {
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    var symbol = SymbolTable.init(&globalAllocator);
    defer symbol.deinit();
    symbol.loadSymbols(initialSymbolStrings[0 .. initialSymbolStrings.len - 1]);
    // for (&staticSymbols, 0..) |ss, i| {
    //     std.debug.print("\nss[{}] {x} {x}", .{ i, ss.header.hash, ss.data.unsigned });
    // }
    try expectEqual(1, symbolIndex(symbols.@"=".asObject()));
    try expectEqual(1, symbolArity(symbols.@"=".asObject()));
    try expectEqual(2, symbolIndex(symbols.value.asObject()));
    try expectEqual(0, symbolArity(symbols.value.asObject()));
    try expectEqual(lastPredefinedSymbol, symbolIndex(symbols.Object.asObject()));
    try expectEqual(0, symbolArity(symbols.Object.asObject()));
    switch (config.objectEncoding) {
        .zag => {
            try expectEqual(3246132625, symbols.Object.testU());
            try expectEqual(8885783185, symbols.@"value:value:".testU());
        },
        else => {},
    }
    // test a few at random to verify arity
    try symbol.verify(symbols.@"=".asObject());
    try symbol.verify(symbols.@"cull:".asObject());
    try symbol.verify(symbols.@"cull:cull:".asObject());
    try symbol.verify(symbols.@"cull:cull:cull:".asObject());
    try symbol.verify(symbols.@"cull:cull:cull:cull:".asObject());
    try symbol.verify(symbols.value.asObject());
    try symbol.verify(symbols.@"perform:".asObject());
    try symbol.verify(symbols.@"at:put:".asObject());
    try symbol.verify(symbols.@"<=".asObject());
    try symbol.verify(symbols.@"+".asObject());
    try symbol.verify(symbols.size.asObject());
    try symbol.verify(symbols.Object.asObject());
    try expect(mem.eql(u8, "valueWithArguments:"[0..], try symbol.asString(symbols.@"valueWithArguments:".asObject()).arrayAsSlice(u8)));
}
// these selectors will have special handling in a dispatch table
// if anding a selector with QuickSelectorsMask == QuickSelectorsMatch
// then, with 98% probability, the selector is one of these 4
// only useful for `perform:` and famiy and adding a CompiledMethod to a dispatch table
// pretty low-frequency paths, so probably not worth it
pub const QuickSelectors = [_]Object{ symbols.@"=", symbols.value, symbols.@"value:", symbols.@"cull:" };
pub const QuickSelectorsMask = 0x19046000;
pub const QuickSelectorsMatch = 0x18046000;
test "find key value for quick selectors" {
    if (config.objectEncoding != .zag) return error.SkipZigTest;
    const printing = false;
    var mask: u64 = 0;
    var match: u64 = 0;
    outer: for (8..32) |bit| {
        const bitmask = @as(u64, 1) << @truncate(bit);
        const bitmatch = QuickSelectors[0].testU() & bitmask;
        for (QuickSelectors) |obj| {
            if ((obj.testU() & bitmask) != bitmatch) continue :outer;
        }
        mask = mask | bitmask;
        match = match | bitmatch;
        if (printing)
            std.debug.print("mask  = {b:0>64}\nmatch = {b:0>64}\n", .{ mask, match });
    }
    if (printing)
        std.debug.print("=     - {b:0>64}\nvalue - {b:0>64}\nvalue:- {b:0>64}\ncull: - {b:0>64}\n", .{ symbols.@"=".rawU(), symbols.value.rawU(), symbols.@"value:".rawU(), symbols.@"cull:".rawU() });
    if (printing)
        std.debug.print("mask  = 0x{x:0>8} match = 0x{x:0>8}\n", .{ mask, match });
    try std.testing.expectEqual(mask, QuickSelectorsMask);
    try std.testing.expectEqual(match, QuickSelectorsMatch);
}
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
