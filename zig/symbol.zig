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
        fn lookup(s: *Self,str: []const u8) ast.object.Object {
            _ = str;
            _ = s;
            return ast.object.Nil;
        }
        fn init(thread: ast.thread.Thread) Self {
            _ = thread;
            return .{
                .theObject = ast.object.Nil,
            };
        }
        fn deinit(s: *Self) void {
            s.*=undefined;
        }
        fn intern(s: *Self,thread: ast.thread.Thread,string: ast.object.Object) ast.object.Object {
            while (true) {
                const result = s.internDirect(string);
                _ = thread;
                return result;
            }
            unreachable;
        }
        fn internDirect(s: *Self,string: ast.object.Object) ast.object.Object {
            _ = s;
            _ = string;
            return ast.object.Nil;
        }
        fn internLiteral(s: *Self,string: []const u8) ast.object.Object {
            var buffer: [200]u8 align(8)= undefined;
            var tempArena = ast.heap.tempArena(&buffer);
            const str = try tempArena.allocString(string);
            return s.internDirect(str);
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
    var symbol = symbol_table().init(thread);
    symbol.loadInitialSymbols(thread);
    defer symbol.deinit();
    try expectEqual(valueWithArguments_,symbol.lookup("valueWithArguments:"));
    try expectEqual(cull_,symbol.lookup("cull:"));
    try expectEqual(cull_cull_,symbol.lookup("cull:cull:"));
    try expectEqual(cull_cull_cull_,symbol.lookup("cull:cull:cull:"));
    try expectEqual(cull_cull_cull_cull_,symbol.lookup("cull:cull:cull:cull:"));
    try expectEqual(value,symbol.lookup("value"));
    try expectEqual(value_,symbol.lookup("value:"));
    try expectEqual(value_value_,symbol.lookup("value:value:"));
    try expectEqual(value_value_value_,symbol.lookup("value:value:value:"));
    try expectEqual(value_value_value_value_,symbol.lookup("value:value:value:value:"));
    try expectEqual(self,symbol.lookup("self"));
    try expectEqual(Object,symbol.lookup("Object"));
    try expectEqual(BlockClosure,symbol.lookup("BlockClosure"));
    try expectEqual(False,symbol.lookup("False"));
    try expectEqual(True,symbol.lookup("True"));
    try expectEqual(UndefinedObject,symbol.lookup("UndefinedObject"));
    try expectEqual(SmallInteger,symbol.lookup("SmallInteger"));
    try expectEqual(Symbol,symbol.lookup("Symbol"));
    try expectEqual(Character,symbol.lookup("Character"));
    try expectEqual(Float,symbol.lookup("Float"));
    try expectEqual(Array,symbol.lookup("Array"));
    try expectEqual(String,symbol.lookup("String"));
    try expectEqual(Class,symbol.lookup("Class"));
    try expectEqual(Metaclass,symbol.lookup("Metaclass"));
    try expectEqual(Behavior,symbol.lookup("Behavior"));
    try expectEqual(Method,symbol.lookup("Method"));
    try expectEqual(Magnitude,symbol.lookup("Magnitude"));
    try expectEqual(Number,symbol.lookup("Number"));
    try expectEqual(System,symbol.lookup("System"));
    try expectEqual(Return,symbol.lookup("Return"));
    try expectEqual(Send,symbol.lookup("Send"));
    try expectEqual(Literal,symbol.lookup("Literal"));
    try expectEqual(Load,symbol.lookup("Load"));
    try expectEqual(Store,symbol.lookup("Store"));
    try expectEqual(SymbolTable,symbol.lookup("SymbolTable"));
    try expectEqual(Dispatch,symbol.lookup("Dispatch"));
    try expectEqual(yourself,symbol.lookup("yourself"));
    try expectEqual(@"==",symbol.lookup("=="));
    try expectEqual(@"~~",symbol.lookup("~~"));
    try expectEqual(@"~=",symbol.lookup("~="));
    try expectEqual(@"=",symbol.lookup("="));
    try expectEqual(@"+",symbol.lookup("+"));
    try expectEqual(@"-",symbol.lookup("-"));
    try expectEqual(@"*",symbol.lookup("*"));
    try expectEqual(size,symbol.lookup("size"));
}
