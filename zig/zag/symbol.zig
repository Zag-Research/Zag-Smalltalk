const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const object = @import("object.zig");
const Nil = object.Nil;
const class = @import("class.zig");
const heap = @import("heap.zig");
const Treap = @import("utilities.zig").Treap;
const thread = @import("thread.zig");
inline fn symbol_of(index: u32, arity: u8) object.Object {
    return symbol0(index|(@as(u32,arity)<<24));
}
pub inline fn symbol0(index: u32) object.Object {
    return @bitCast(object.Object,index|((@as(u64,0xfff70000)+class.Symbol_I)<<32));
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
var symbolTable : ?Symbol_Table = null;

pub fn init(initialSymbolTableSize:usize,str:[]const heap.HeapConstPtr) !Symbol_Table {
    var arena = heap.globalArena.asArena();
    var st = symbolTable orelse try Symbol_Table.init(arena,initialSymbolTableSize);
    symbolTable = st;
    st.loadSymbols(initialSymbolStrings[0..]);
    st.loadSymbols(str);
    return st;
}
pub fn deinit(thr: *thread.Thread) void {
    _ = thr;
    (symbolTable orelse unreachable).deinit();
}
pub fn asString(string: object.Object) object.Object {
    return (symbolTable orelse unreachable).asString(string);
}
pub fn loadSymbols(thr: *thread.Thread,str:[]const heap.HeapConstPtr) void {
    var arena = thr.getArena().getGlobal();
    (symbolTable orelse unreachable).loadSymbols(arena,str);
}
// pub fn lookupLiteral(string: []const u8) object.Object {
//     return (symbolTable orelse unreachable).lookupLiteral(string);
// }
// pub fn internLiteral(arena: *heap.Arena, string: []const u8) object.Object {
//     const result = (symbolTable orelse unreachable).internLiteral(arena, string);
//     if (!result.isNil()) return result;
//     unreachable; // out of space
// }
pub fn lookup(_: *thread.Thread,string: object.Object) object.Object {
    return (symbolTable orelse unreachable).lookup(string);
}
pub fn intern(string: object.Object) object.Object {
    return (symbolTable orelse unreachable).intern(string);
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
const Symbol_Table = struct {
    theObject: object.Object,
    const Self = @This();
    fn init(arena: *heap.Arena, initialSymbolTableSize:usize) !Self {
        var theHeapObject = try arena.allocObject(class.SymbolTable_I,
                                                  0,initialSymbolTableSize*2,object.Object,heap.Age.stack);
        _ = objectTreap.init(theHeapObject.arrayAsSlice(u8),object.compareObject,Nil);
        return Symbol_Table {
            .theObject = theHeapObject.asObject(),
        };
    }
    fn deinit(s: *Self) void {
        s.*=undefined;
    }
    fn asString(s: *Self,string: object.Object) object.Object {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject,Nil);
        return trp.getKey(@truncate(u24,string.hash32()));
    }
    fn lookup(s: *Self,string: object.Object) object.Object {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject,Nil);
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
    fn intern(s: *Self,string: object.Object) object.Object {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject,Nil);
        while (true) {
            const lu = lookupDirect(&trp,string);
            if (!lu.isNil()) return lu;
            const result = internDirect(&trp,string);
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
    fn loadSymbols(s: *Self, strings: [] const heap.HeapConstPtr) void {
        var trp = objectTreap.ref(s.theObject.arrayAsSlice(u8),object.compareObject,Nil);
        for (strings) |string|
            _ = internDirect(&trp,string.asObject());
    }
};
pub const noStrings = &[0]heap.HeapConstPtr{};
fn verify(symbol: object.Object) !void {
    try std.testing.expectEqual(symbol,(symbolTable orelse unreachable).lookup(initialSymbolStrings[symbol.hash24()].asObject()));
}
test "symbols match initialized symbol table" {
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    var symbol = symbolTable orelse try init(250,noStrings);
    defer symbol.deinit();
    for(initialSymbolStrings) |string,idx|
        try expectEqual(idx+1,symbol.lookup(string.asObject()).hash24());
    // test a few at random to verify arity
    try verify(symbols.@"cull:");
    try verify(symbols.@"cull:cull:");
    try verify(symbols.@"cull:cull:cull:");
    try verify(symbols.@"cull:cull:cull:cull:");
    try verify(symbols.value);
    try verify(symbols.@"+");
    try verify(symbols.size);
    try verify(symbols.Object);
    try expect(mem.eql(u8,"valueWithArguments:"[0..],symbol.asString(symbols.@"valueWithArguments:").arrayAsSlice(u8)));
}
