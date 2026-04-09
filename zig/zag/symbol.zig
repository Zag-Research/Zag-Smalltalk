const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const builtin = @import("builtin");
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const utilities = zag.utilities;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const PointedObject = zag.InMemory.PointedObject;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const Treap = zag.utilities.Treap;
const hash = zag.utilities.PhiHash.hash24;
const unhash = zag.utilities.PhiHash.unhash24;
const Signature = zag.execute.Signature;
pub var globalAllocator = std.heap.page_allocator; //@import("globalArena.zig").allocator();

pub const Symbols = SymbolsEnum;
pub inline fn symbolIndex(obj: object.Object) u24 {
    if (obj.symbolHash()) |hsh| return unhash(hsh);
    unreachable;
}
pub const signature = SymbolsEnum.signature;
pub fn fromHash(aHash: u24) Object {
    const index = unhash(aHash);
    return @as(SymbolsEnum, @enumFromInt(index)).asObject();
}
const SymbolsEnum = enum(u32) {
    value = 1,
    @"=" = 0x1000000 + 2,
    @"value:",
    @"cull:",
    @"doesNotUnderstand:",
    @"at:",
    @"new:",
    @"valueWithArguments:",
    @"ifTrue:",
    @"ifFalse:",
    @"ifNil:",
    @"ifNotNil:",
    @"perform:",
    @"+",
    @"-",
    @"*",
    @"~=",
    @"==",
    @"~~",
    @"<",
    @"<=",
    @",>=",
    @">" = 0x1000000 + 23,
    @"at:put:" = 0x2000000 + 24,
    @"value:value:",
    @"cull:cull:",
    @"ifTrue:ifFalse",
    @"ifFalse:ifTrue:",
    @"ifNil:ifNotNil",
    @"ifNotNil:ifNil:",
    @"perform:with:",
    @"perform:withArguments:" = 0x2000000 + 32,
    @"value:value:value:" = 0x3000000 + 33,
    @"cull:cull:cull:",
    @"perform:with:with:",
    @"perform:withArguments:inSuperclass:" = 0x3000000 + 36,
    @"value:value:value:value:" = 0x4000000 + 37,
    @"cull:cull:cull:cull:",
    @"perform:with:with:with:",
    yourself = 40,
    size,
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
    fibonacci,
    Object,
    _,
    const lastPredefinedSymbol = std.meta.fields(SymbolsEnum).len;
    comptime {
        std.debug.assert(initialSymbolStrings.len == lastPredefinedSymbol);
    }
    const staticSymbols = blk: {
        var symbolArray = [_]PointedObject{undefined} ** lastPredefinedSymbol;
        for (symbolArray[0..], std.meta.fields(SymbolsEnum)) |*sym, symbol|
            initSymbol(sym, @enumFromInt(symbol.value));
        break :blk symbolArray;
    };
    fn initSymbol(sym: *PointedObject, symbol: SymbolsEnum) void {
        const s_hash = symbol.symbolHash().?;
        sym.header = HeapHeader{ .classIndex = .Symbol, .hash = s_hash, .objectFormat = .notIndexable, .age = .static, .length = 1 };
        sym.data.unsigned = @as(u64, s_hash) << 8 | symbol.numArgs();
    }
    pub inline fn numArgs(self: SymbolsEnum) u4 {
        return @intCast(@intFromEnum(self) >> 24);
    }
    pub inline fn symbolHash(self: SymbolsEnum) ?u24 {
        return hash(@truncate(@intFromEnum(self)));
    }
    pub inline fn get_class(_: SymbolsEnum) object.ClassIndex {
        return .Symbol;
    }
    pub fn asObject(self: SymbolsEnum) Object {
        const index: u24 = @truncate(@intFromEnum(self));
        if (config.immediateSymbols) {
            return Object.makeSymbol(.Symbol, hash(index), @intCast(@intFromEnum(self) >> 24));
        }
        return Object.fromAddress(&staticSymbols[index - 1]);
    }
    fn signature(sym: SymbolsEnum, primitive: u8) Signature {
        const int = @intFromEnum(sym);
        return Signature.fromHashPrimitive(hash(@truncate(int)), @intCast(int >> 24), primitive);
    }
    inline fn symbol_of(index: u24, nArgs: u4) Object {
        if (config.immediateSymbols) {
            return Object.makeSymbol(.Symbol, hash(index), nArgs);
        }
        return Object.fromAddress(&staticSymbols[index - 1]);
    }
};
const initialSymbolStrings = heap.compileStrings(blk: {
    var array: [std.meta.fields(SymbolsEnum).len][]const u8 = undefined;
    for (0.., std.meta.fields(SymbolsEnum)) |i, sym| {
        array[i] = sym.name;
    }
    break :blk array;
});
var symbolTable = SymbolTable.init(&globalAllocator);
pub fn asString(obj: Object) Object {
    return symbolTable.asString(symbolIndex(obj));
}
pub fn asStringFromHash(h: u24) Object {
    return symbolTable.asString(unhash(h));
}
pub fn loadSymbols(strs: []const heap.HeapObjectConstPtr) void {
    symbolTable.loadSymbols(strs);
}
pub inline fn lookup(string: Object) ?Object {
    return symbolTable.lookup(string);
}
pub inline fn intern(string: Object) Object {
    return symbolTable.intern(string);
}
const SymbolTreap = if (config.immediateSymbols) Treap(Object, u32, u0) else Treap(Object, u32, *PointedObject);
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
    mem: []SymbolTreap.Element,
    treap: SymbolTreap,
    allocator: *Allocator,
    const Self = @This();
    const initialSymbolTableSize = 50;
    pub fn init(allocator: *Allocator) Self {
        return SymbolTable{
            .mem = &[0]SymbolTreap.Element{},
            .treap = SymbolTreap.initEmpty(object.compareObject, Object.ZERO),
            .allocator = allocator,
        };
    }
    inline fn theTreap(self: *Self, adding: usize) *SymbolTreap {
        if (self.treap.hasRoom(adding))
            return &self.treap;
        return self.allocTreap(adding);
    }
    fn allocTreap(self: *Self, _: usize) *SymbolTreap {
        {
            // ToDo: add locking
            const size = heap.growSize(self.mem, SymbolTreap.Element) catch initialSymbolTableSize * SymbolTreap.elementSize;
            const memory = self.allocator.alloc(SymbolTreap.Element, size) catch @panic("can't alloc");
            self.treap.resize(memory);
            self.allocator.free(self.mem);
            self.mem = memory;
        }
        self.loadSymbols(&initialSymbolStrings);
        return &self.treap;
    }
    fn deinit(self: *Self) void {
        self.allocator.free(self.mem);
        self.* = undefined;
    }
    fn asString(self: *Self, index: u32) Object {
        return self.theTreap(0).getKey(index);
    }
    pub fn lookup(self: *Self, string: Object) ?Object {
        return lookupDirect(self.theTreap(0), string);
    }
    fn lookupDirect(trp: *SymbolTreap, string: Object) ?Object {
        const index = trp.lookup(string);
        if (index > 0) {
            if (config.immediateSymbols) {
                const nArgs = numArgs(string);
                return SymbolsEnum.symbol_of(@truncate(index), nArgs);
            }
            return @bitCast(@intFromPtr(trp.getValue(index)));
        }
        return null;
    }
    fn intern(self: *Self, string: Object) Object {
        const trp = self.theTreap(1);
        while (true) {
            if (lookupDirect(trp, string)) |result| return result;
            if (internDirect(self, trp, string)) |result| return result;
            @panic("unreachable"); // out of space
        }
        @panic("unreachable");
    }
    fn internDirect(self: *Self, trp: *SymbolTreap, string: Object) ?Object {
        if (lookupDirect(trp, string)) |result| return result;
        const str = string.promoteToUnmovable() catch @panic("immovable");
        const index = trp.insert(str) catch @panic("unreachable");
        if (config.immediateSymbols)
            return SymbolsEnum.symbol_of(@truncate(index), numArgs(string));
        const obj: *PointedObject = @ptrCast(self.allocator.alloc(PointedObject, 1) catch @panic("can't alloc"));
        obj.setHeader(.Symbol, string.hash24());
        obj.data.object = string;
        trp.setValue(index, obj);
        return @bitCast(@intFromPtr(obj));
    }
    fn loadSymbols(self: *Self, strings: []const heap.HeapObjectConstPtr) void {
        const trp = self.theTreap(strings.len);
        for (strings) |string|
            _ = internDirect(self, trp, string.asObject());
    }
    fn verify(self: *Self, symbol: Object) !void {
        const string = initialSymbolStrings[symbolIndex(symbol) - 1].asObject();
        const other = self.lookup(string);
        try std.testing.expectEqual(symbol, other);
    }
};
pub const noStrings = &[0]heap.HeapObjectConstPtr{};
test "symbols match initialized symbol table" {
    const expectEqual = std.testing.expectEqual;
    var symbol = SymbolTable.init(&globalAllocator);
    defer symbol.deinit();
    symbol.loadSymbols(&initialSymbolStrings);
    const debugging = false;
    if (debugging) {
        for (&Symbols.staticSymbols, 0..) |ss, i|
            trace("ss[{}] {x} {x}", .{ i, ss.header.hash, ss.data.unsigned });
    }
    try expectEqual(1, symbolIndex(Symbols.value.asObject()));
    try expectEqual(0, Symbols.value.asObject().numArgs());
    try expectEqual(2, symbolIndex(Symbols.@"=".asObject()));
    try expectEqual(1, Symbols.@"=".asObject().numArgs());
    try expectEqual(0, Symbols.Object.asObject().numArgs());
    try expectEqual(2, Symbols.@"at:put:".asObject().numArgs());
    try expectEqual(1, Symbols.@"<=".asObject().numArgs());
    try expectEqual(Symbols.lastPredefinedSymbol, symbolIndex(Symbols.Object.asObject()));
    try expectEqual(0, Symbols.Object.asObject().numArgs());
    switch (config.objectEncoding) {
        .zag => {
            try expectEqual(0x5FB38659, Symbols.Object.asObject().testU());
            try expectEqual(0x2736AD159, Symbols.@"value:value:".asObject().testU());
        },
        .nan => {
            try expectEqual(0x0, Symbols.Object.asObject().testU());
            try expectEqual(0x0, Symbols.@"value:value:".asObject().testU());
        },
        else => {},
    }
    // test a few at random to verify arity
    try symbol.verify(Symbols.@"=".asObject());
    try symbol.verify(Symbols.@"cull:".asObject());
    try symbol.verify(Symbols.@"cull:cull:".asObject());
    try symbol.verify(Symbols.@"cull:cull:cull:".asObject());
    try symbol.verify(Symbols.@"cull:cull:cull:cull:".asObject());
    try symbol.verify(Symbols.value.asObject());
    try symbol.verify(Symbols.@"perform:".asObject());
    try symbol.verify(Symbols.@"at:put:".asObject());
    try symbol.verify(Symbols.@"<=".asObject());
    try symbol.verify(Symbols.@"+".asObject());
    try symbol.verify(Symbols.size.asObject());
    try symbol.verify(Symbols.Object.asObject());
    //    try std.testing.expect(mem.eql(u8, "valueWithArguments:"[0..], try symbol.asString(Symbols.@"valueWithArguments:".asObject()).arrayAsSlice(u8)));
}
// these selectors will have special handling in a dispatch table
// if anding a selector with QuickSelectorsMask == QuickSelectorsMatch
// then, with 98% probability, the selector is one of these 4
// only useful for `perform:` and famiy and adding a CompiledMethod to a dispatch table
// pretty low-frequency paths, so probably not worth it
pub const QuickSelectors = [_]Object{ Symbols.@"=".asObject(), Symbols.value.asObject(), Symbols.@"value:".asObject(), Symbols.@"cull:".asObject() };
pub const QuickSelectorsMask = 0x19046000;
pub const QuickSelectorsMatch = 0x18046000;
pub fn findQuickSelector(obj: Object) ?u2 {
    if (config.immediateSymbols and (obj.testU() & QuickSelectorsMask) == QuickSelectorsMatch) {
        for (QuickSelectors,0..) |qs,i| {
            if (obj.equals(qs)) return @truncate(i);
        }
    }
    return null;
}
test "find key value for quick selectors" {
    if (!config.immediateSymbols) return error.SkipZigTest;
    var mask: u64 = 0;
    var match: u64 = 0;
    outer: for (8..32) |bit| {
        const bitmask = @as(u64, 1) << @intCast(bit);
        const bitmatch = QuickSelectors[0].testU() & bitmask;
        for (QuickSelectors) |obj| {
            if ((obj.testU() & bitmask) != bitmatch) continue :outer;
        }
        mask = mask | bitmask;
        match = match | bitmatch;
    }
    try std.testing.expectEqual(mask, QuickSelectorsMask);
    try std.testing.expectEqual(match, QuickSelectorsMatch);
    for (QuickSelectors) |obj| {
        try std.testing.expect(findQuickSelector(obj)!=null);
    }
}
test "force second allocation of symbol treap" {
    if (true) return error.SkipZigTest;
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
    symbol.loadSymbols(&initialSymbolStrings);
    symbol.loadSymbols(&moreSymbolStrings);
    try std.testing.expect(lookup(initialSymbolStrings[1]) != null);
    try std.testing.expect(lookup(moreSymbolStrings[1]) != null);
    //_ = symbol.allocator.allocArray(49,480,u8);
}
