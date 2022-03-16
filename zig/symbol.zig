const std = @import("std");
const ast = struct {
    const object = @import("object.zig");
    const heap = @import("heap.zig");
    const treap = @import("treap.zig");
    const thread = @import("thread.zig");
};
pub inline fn symbol_of(index: u64, arity: u64) ast.object.Object {
    return @bitCast(ast.object.Object,index|(arity<<24)|(0x7ffd<<49));
}
pub inline fn symbol0(index: u64) ast.object.Object {
    return @bitCast(ast.object.Object,index|(0x7ffd<<49));
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

 
pub fn init(thread: ast.thread.Thread) void {
    _ = thread;
}
fn symbol_table() type {
    return struct {
        theObject: ast.object.Object,
        const Self = @This();
        fn lookup(s: *Self,thread: ast.thread.Thread,str: []const u8) ast.object.Object {
            _ = str;
            _ = s;
            _ = thread;
            return ast.object.Nil;
        }
        fn init() Self {
            return .{
                .theObject = ast.object.Nil,
            };
        }
        fn deinit(s: *Self) void {
            s.*=undefined;
        }
        fn loadInitialSymbols(s: *Self,thread: ast.thread.Thread) void {
            const initialSymbols = .{
                "valueWithArguments:", "cull:", "cull:cull:", "cull:cull:cull:", "cull:cull:cull:cull:", 
                "value", "value:", "value:value:", "value:value:value:", "value:value:value:value:",
                "self",
                "Object", "BlockClosure", "False", "True",
                "UndefinedObject", "SmallInteger", "Symbol", "Character",
                "Float", "Array", "String", "Class", "Metaclass",
                "Behavior", "Magnitude", "Number", "Method", "System",
                "Return","Send","Literal","Load","Store",
                "SymbolTable", "Dispatch",
                "yourself", "==", "~~", "~=", "=", "+", "-", "*", "size",
            };
            _ = initialSymbols;
            _ = s;
            _ = thread;
        }
    };
}
    
test "symbols match initialized symbol table" {
    const expectEqual = std.testing.expectEqual;
    var thread = try ast.thread.Thread.init();
    defer thread.deinit();
    var symbol = symbol_table().init();
    symbol.loadInitialSymbols(thread);
    defer symbol.deinit();
    try expectEqual(valueWithArguments_,symbol.lookup(thread,"valueWithArguments:"));
    try expectEqual(cull_,symbol.lookup(thread,"cull:"));
    try expectEqual(cull_cull_,symbol.lookup(thread,"cull:cull:"));
    try expectEqual(cull_cull_cull_,symbol.lookup(thread,"cull:cull:cull:"));
    try expectEqual(cull_cull_cull_cull_,symbol.lookup(thread,"cull:cull:cull:cull:"));
    try expectEqual(value,symbol.lookup(thread,"value"));
    try expectEqual(value_,symbol.lookup(thread,"value:"));
    try expectEqual(value_value_,symbol.lookup(thread,"value:value:"));
    try expectEqual(value_value_value_,symbol.lookup(thread,"value:value:value:"));
    try expectEqual(value_value_value_value_,symbol.lookup(thread,"value:value:value:value:"));
    try expectEqual(self,symbol.lookup(thread,"self"));
    try expectEqual(Object,symbol.lookup(thread,"Object"));
    try expectEqual(BlockClosure,symbol.lookup(thread,"BlockClosure"));
    try expectEqual(False,symbol.lookup(thread,"False"));
    try expectEqual(True,symbol.lookup(thread,"True"));
    try expectEqual(UndefinedObject,symbol.lookup(thread,"UndefinedObject"));
    try expectEqual(SmallInteger,symbol.lookup(thread,"SmallInteger"));
    try expectEqual(Symbol,symbol.lookup(thread,"Symbol"));
    try expectEqual(Character,symbol.lookup(thread,"Character"));
    try expectEqual(Float,symbol.lookup(thread,"Float"));
    try expectEqual(Array,symbol.lookup(thread,"Array"));
    try expectEqual(String,symbol.lookup(thread,"String"));
    try expectEqual(Class,symbol.lookup(thread,"Class"));
    try expectEqual(Metaclass,symbol.lookup(thread,"Metaclass"));
    try expectEqual(Behavior,symbol.lookup(thread,"Behavior"));
    try expectEqual(Method,symbol.lookup(thread,"Method"));
    try expectEqual(Magnitude,symbol.lookup(thread,"Magnitude"));
    try expectEqual(Number,symbol.lookup(thread,"Number"));
    try expectEqual(System,symbol.lookup(thread,"System"));
    try expectEqual(Return,symbol.lookup(thread,"Return"));
    try expectEqual(Send,symbol.lookup(thread,"Send"));
    try expectEqual(Literal,symbol.lookup(thread,"Literal"));
    try expectEqual(Load,symbol.lookup(thread,"Load"));
    try expectEqual(Store,symbol.lookup(thread,"Store"));
    try expectEqual(SymbolTable,symbol.lookup(thread,"SymbolTable"));
    try expectEqual(Dispatch,symbol.lookup(thread,"Dispatch"));
    try expectEqual(yourself,symbol.lookup(thread,"yourself"));
    try expectEqual(@"==",symbol.lookup(thread,"=="));
    try expectEqual(@"~~",symbol.lookup(thread,"~~"));
    try expectEqual(@"~=",symbol.lookup(thread,"~="));
    try expectEqual(@"=",symbol.lookup(thread,"="));
    try expectEqual(@"+",symbol.lookup(thread,"+"));
    try expectEqual(@"-",symbol.lookup(thread,"-"));
    try expectEqual(@"*",symbol.lookup(thread,"*"));
    try expectEqual(size,symbol.lookup(thread,"size"));
}
