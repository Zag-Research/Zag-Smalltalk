// Implementation of a treap
const std = @import("std");
const math = std.math;
const Order = math.Order;
const mem = std.mem;
const object = @import("object.zig");
const Object = object.Object;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const class = @import("class.zig");
fn priority(pos:i32) u32 {
    return @intCast(u32,pos+1)*%@as(u32,1999999973);
}
pub fn Treap(comptime K:type) type {
    const Element = packed struct {
        left: u32,
        right: u32,
        value: K,
    };
    const Compare = fn (K,K) Order;
    return struct {
        table: []Element,
        root: u32,
        free: usize,
        compare: Compare,
        const Self = @This();
        const Equal = Order.eq;
        const Less = Order.lt;
        const Greater = Order.gt;
        pub fn init(memory: []u8, compare: Compare) Self {
            // every 2 Objects in the heap object will form an Element
            // K must be an object - size element, and if it's in the pointer range must point into the heap (either current or more permanent)
            // left,right as an object will together look like an f64 or a positive SmallInteger (because the only negative is -1)
            // in either case it will be ignored by garbage collection
            const table = mem.bytesAsSlice(Element,memory);
            return .{
                .table = table,
                .root = 0,
                .free = 1,
                .compare = compare,
            };
        }
        pub inline fn lookupElement(self: *Self,key:K) u32 {
            return self.lookup(self.root,key);
        }
        fn lookup(self: *Self,current:u32,key:K) u32 {
            if (current==0) {
                return 0;
            } else switch (self.compare(self.table[current].value,key)) {
                Equal => return current,
                Less => return self.lookup(self.table[current].right,key),
                Greater => return self.lookup(self.table[current].left ,key),
            }
        }
    };
}
fn compareObject(left: Object, right: Object) Order {
    const l = @bitCast(u64,left);
    const r = @bitCast(u64,right);
    if (l==r) return Order.eq;
    if (l<r) return Order.lt;
    if (l>r) return Order.gt;
    unreachable;
}
fn compareU64(l: u64, r: u64) Order {
    return math.order(l,r);
}
test "simple u64 treap alloc" {
    const expectEqual = @import("std").testing.expectEqual;
    const n = 1;
    var memory = [_]u8{0} ** (n*48);
    var treap = Treap(u64).init(memory[0..],compareU64);
    try expectEqual(treap.lookupElement(42),0);
    try std.io.getStdOut().writer().print("treap={}\n",.{treap});
}
