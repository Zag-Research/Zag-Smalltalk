const ObjectT = @import("object.zig").Object;
pub fn symbol_of(comptime index: u64,comptime arity: u64) callconv(.Inline) ObjectT {
    return @bitCast(ObjectT,index|(arity<<24)|(0x7ffd<<49));
}
pub const yourself = symbol_of(2,0);
pub const bar_ = symbol_of(3,1);
pub const Object = symbol_of(4,0);
pub const Class = symbol_of(5,0);
pub const Magnitude = symbol_of(5,0);
pub const Number = symbol_of(5,0);
pub const SmallInteger = symbol_of(6,0);
pub const negated = symbol_of(7,0);
