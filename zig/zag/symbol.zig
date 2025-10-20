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

pub const symbols = SymbolsEnum;
pub inline fn symbolIndex(obj: object.Object) u24 {
    return unhash(obj.hash24());
}
pub inline fn symbolArity(obj: object.Object) u4 {
    return @truncate(obj.hash32() >> 24);
}
inline fn hash_of(index: u24, arity: u4) u32 {
    return @as(u32, hash(index)) | (@as(u32, arity) << 24);
}
pub const signature = SymbolsEnum.signature;
pub fn fromHash(aHash: u64) Object {
    const index = unhash(@truncate(aHash));
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
        sym.header = HeapHeader{ .classIndex = .Symbol, .hash = s_hash, .format = .notIndexable, .age = .static, .length = 1 };
        sym.data.unsigned = s_hash | @as(u64, symbol.numArgs()) << 24;
    }
    pub inline fn numArgs(self: SymbolsEnum) u4 {
        return @truncate(@intFromEnum(self) >> 24);
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
            return Object.makeImmediate(.Symbol, hash_of(index, @truncate(@intFromEnum(self) >> 24)));
        }
        const O = packed struct { sym: *const PointedObject };
        return @bitCast(O{ .sym = &staticSymbols[index - 1] });
    }
    fn signature(sym: SymbolsEnum, primitive: u16) Signature {
        const int = @intFromEnum(sym);
        return Signature.fromHashPrimitive(hash_of(@truncate(int), @truncate(int >> 24)), primitive);
    }
    inline fn symbol_of(index: u24, nArgs: u4) Object {
        if (config.immediateSymbols) {
            return Object.makeImmediate(.Symbol, hash_of(index, nArgs));
        }
        const O = packed struct { sym: *const PointedObject };
        return @bitCast(O{ .sym = &staticSymbols[index - 1] });
    }
};
const initialSymbolStrings = heap.compileStrings(.{ // must be in exactly same order as above
    "=",                                   "value",                    "value:",                 "cull:",                   "yourself",        "doesNotUnderstand:",
    "at:",                                 "new:",                     "valueWithArguments:",    "ifTrue:",                 "ifFalse:",        "ifNil:",
    "ifNotNil:",                           "perform:",                 "+",                      "-",                       "*",               "~=",
    "==",                                  "~~",                       "<",                      "<=",                      ",>=",             ">",
    "at:put:",                             "value:value:",             "cull:cull:",             "ifTrue:ifFalse",          "ifFalse:ifTrue:", "ifNil:ifNotNil",
    "ifNotNil:ifNil:",                     "perform:with:",            "perform:withArguments:", "value:value:value:",      "cull:cull:cull:", "perform:with:with:",
    "perform:withArguments:inSuperclass:", "value:value:value:value:", "cull:cull:cull:cull:",   "perform:with:with:with:", "size",            "negated",
    "new",                                 "self",                     "name",                   "class",                   "Class",           "Behavior",
    "ClassDescription",                    "Metaclass",                "SmallInteger",           "noFallback",
    // add any new values here
                 "fibonacci",       "Object",
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
pub inline fn lookup(string: Object) Object {
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
            .treap = SymbolTreap.initEmpty(object.compareObject, Nil()),
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
        self.loadSymbols(initialSymbolStrings[0..initialSymbolStrings.len]);
        return &self.treap;
    }
    fn deinit(self: *Self) void {
        self.allocator.free(self.mem);
        self.* = undefined;
    }
    fn asString(self: *Self, index: u32) Object {
        return self.theTreap(0).getKey(index);
    }
    pub fn lookup(self: *Self, string: Object) Object {
        return lookupDirect(self.theTreap(0), string);
    }
    fn lookupDirect(trp: *SymbolTreap, string: Object) Object {
        const index = trp.lookup(string);
        if (index > 0) {
            if (config.immediateSymbols) {
                const nArgs = numArgs(string);
                return SymbolsEnum.symbol_of(@truncate(index), nArgs);
            }
            return @bitCast(@intFromPtr(trp.getValue(index)));
        }
        return Nil();
    }
    fn intern(self: *Self, string: Object) Object {
        const trp = self.theTreap(1);
        while (true) {
            const lu = lookupDirect(trp, string);
            if (!lu.isNil()) return lu;
            const result = internDirect(self, trp, string);
            if (!result.isNil()) return result;
            unreachable; // out of space
        }
        unreachable;
    }
    fn internDirect(self: *Self, trp: *SymbolTreap, string: Object) Object {
        const result = lookupDirect(trp, string);
        if (!result.isNil()) return result;
        const str = string.promoteToUnmovable() catch return Nil();
        const index = trp.insert(str) catch unreachable;
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
    const expect = std.testing.expect;
    var symbol = SymbolTable.init(&globalAllocator);
    defer symbol.deinit();
    symbol.loadSymbols(initialSymbolStrings[0 .. initialSymbolStrings.len - 1]);
    const debugging = false;
    if (debugging) {
        for (&symbols.staticSymbols, 0..) |ss, i|
            trace("\nss[{}] {x} {x}", .{ i, ss.header.hash, ss.data.unsigned });
    }
    try expectEqual(1, symbolIndex(symbols.@"="));
    try expectEqual(1, symbolArity(symbols.@"="));
    try expectEqual(2, symbolIndex(symbols.value));
    try expectEqual(0, symbolArity(symbols.value));
    try expectEqual(0, symbolArity(symbols.Object));
    try expectEqual(2, symbolArity(symbols.@"at:put:"));
    try expectEqual(1, symbolArity(symbols.@"<="));
    try expectEqual(SymbolsEnum.lastPredefinedSymbol, symbolIndex(symbols.Object));
    try expectEqual(0, symbolArity(symbols.Object));
    switch (config.objectEncoding) {
        .zag => {
            try expectEqual(0x5FB38689, symbols.Object.testU());
            try expectEqual(0x211A24A89, symbols.@"value:value:".testU());
        },
        else => {},
    }
    // test a few at random to verify arity
    try symbol.verify(symbols.@"=");
    try symbol.verify(symbols.@"cull:");
    try symbol.verify(symbols.@"cull:cull:");
    try symbol.verify(symbols.@"cull:cull:cull:");
    try symbol.verify(symbols.@"cull:cull:cull:cull:");
    try symbol.verify(symbols.value);
    try symbol.verify(symbols.@"perform:");
    try symbol.verify(symbols.@"at:put:");
    try symbol.verify(symbols.@"<=");
    try symbol.verify(symbols.@"+");
    try symbol.verify(symbols.size);
    try symbol.verify(symbols.Object);
    try expect(mem.eql(u8, "valueWithArguments:"[0..], try symbol.asString(symbols.@"valueWithArguments:").arrayAsSlice(u8)));
    trace("yourself: {x}\n", .{@as(u64, @bitCast(symbols.yourself))});
    trace("verified: symbols match initialized symbol table\n", .{});
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
    try config.skipNotZag();
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
        trace("mask  = {b:0>64}\nmatch = {b:0>64}\n", .{ mask, match });
    }
    trace("=     - {b:0>64}\nvalue - {b:0>64}\nvalue:- {b:0>64}\ncull: - {b:0>64}\n", .{ symbols.@"=".rawU(), symbols.value.rawU(), symbols.@"value:".rawU(), symbols.@"cull:".rawU() });
    trace("mask  = 0x{x:0>8} match = 0x{x:0>8}\n", .{ mask, match });
    try std.testing.expectEqual(mask, QuickSelectorsMask);
    try std.testing.expectEqual(match, QuickSelectorsMatch);
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
    //_ = symbol.allocator.allocArray(49,480,u8);
}
