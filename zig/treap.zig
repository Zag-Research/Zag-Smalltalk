// Implementation of a treap
const std = @import("std");
const math = std.math;
const Order = math.Order;
const mem = std.mem;
pub fn Treap(comptime K:type) type {
    const Element = packed struct {
        left: u32,
        right: u32,
        key: K,
    };
    const Compare = fn (K,K) Order;
    return struct {
        table: []Element,
        compare: Compare,
        const Self = @This();
        const Equal = Order.eq;
        const Less = Order.lt;
        const Greater = Order.gt;
        pub fn init(memory: []u8, compare: Compare, empty: K) Self {
            // if this is allocated on an object heap, left,right as an object will together look like an f64 so it will be ignored by garbage collection
            var treap = ref(memory,compare);
            treap.setRoot(0);
            treap.extend(1,empty);
            return treap;
        }
        pub fn ref(memory: []u8, compare: Compare) Self {
            return Self {
                .table = mem.bytesAsSlice(Element,memory),
                .compare = compare,
            };
        }
        pub fn extend(self: *Self, start: u32, empty: K) void {
            self.setFree(start);
            var index = start+1;
            for (self.table[start..]) |*element| {
                element.key=empty;
                element.left=index;
                element.right=0;
                index += 1;
            }
        }
        inline fn root(self: *const Self) u32 {
            return self.table[0].right;
        }
        inline fn setRoot(self: *Self,r: u32) void {
            self.table[0].right=r;
        }
        inline fn free(self: *const Self) u32 {
            return self.table[0].left;
        }
        inline fn setFree(self: *Self,f: u32) void {
            self.table[0].left=f;
        }
        inline fn priority(pos:u32) u32 { // "random" number based on position in the array
            return pos *% 0xa1fdc7a3;
        }
        pub inline fn lookup(self: *const Self, key: K) u32 {
            return self.lookupElement(self.root(),key);
        }
        fn lookupElement(self: *const Self,current:u32,key:K) u32 {
            if (current==0) {
                return 0;
            } else switch (self.compare(self.table[current].key,key)) {
                Equal => return current,
                Less => return self.lookupElement(self.table[current].right,key),
                Greater => return self.lookupElement(self.table[current].left ,key),
            }
        }
        pub fn insert(self: *Self,key: K) !u32 {
            const result = self.lookup(key);
            if (result>0) {
                // might have been added while waiting for the write lock
                return result;
            }
            const pos = self.free();
            if (pos>=self.table.len) return error.OutOfSpace;
            self.setFree(self.table[pos].left);
            self.table[pos].key=key;
            self.table[pos].left=0;
            self.setRoot(self.insertElement(pos,self.root()));
            return pos;
        }
        fn insertElement(self: *Self,target:u32,current:u32) u32 {
            if (current==0)
                return target;
            switch (self.compare(self.table[target].key,self.table[current].key)) {
                Equal => @panic("shouldn't be inserting an existing key"),
                Less => {
                    self.table[current].left = self.insertElement(target,self.table[current].left);
                    // Fix Heap property if it is violated
                    if (priority(self.table[current].left) > priority(current)) {
                        return self.rightRotate(current);
                    } else
                        return current;
                },
                Greater => {
                    self.table[current].right = self.insertElement(target,self.table[current].right);
                    // Fix Heap property if it is violated
                    if (priority(self.table[current].right) > priority(current)) {
                        return self.leftRotate(current);
                    } else
                        return current;
                },
            }
        }
        // T1, T2 and T3 are subtrees of the tree rooted with y
        // (on left side) or x (on right side)
        //           y                               x
        //          / \     Right Rotation          /  \
        //         x   T3   – – – – – – – >        T1   y
        //        / \       < - - - - - - -            / \
        //       T1  T2     Left Rotation            T2  T3
        //
        fn rightRotate(self: *Self,y:u32) u32 {
            const x = self.table[y].left;
            const t2 = self.table[x].right;
            // Perform rotation
            self.table[x].right = y;
            self.table[y].left = t2;
            return x;
        }
        fn leftRotate(self: *Self,x:u32) u32 {
            const y = self.table[x].right;
            const t2 = self.table[y].left;
            // Perform rotation
            self.table[y].left = x;
            self.table[x].right = t2;
            return y;
        }
        // Only for tests
        pub fn depths(self: *Self, data: []u32) void {
            self.walk(self.root(),1,data);
        }
        fn walk(self: *Self, pos: u32, depth: u32, data: []u32) void {
            data[pos]=depth;
            if (self.table[pos].left>0) {
                self.walk(self.table[pos].left,depth+1,data);
                if (self.table[pos].right>0)
                    self.walk(self.table[pos].right,depth+1,data);
            } else if (self.table[pos].right>0) {
                self.walk(self.table[pos].right,depth+1,data);
            }
        }
    };
}
fn compareU64(l: u64, r: u64) Order {
    return math.order(l,r);
}
const Treap_u64 = Treap(u64);
test "simple u64 treap alloc" {
    const expectEqual = @import("std").testing.expectEqual;
    const n = 2;
    var memory = [_]u8{0} ** (n*48);
    var treap = Treap_u64.init(memory[0..],compareU64,0);
    try expectEqual(treap.lookup(42),0);
    const f2 = try treap.insert(42);
    try expectEqual(f2,1);
    try expectEqual(treap.lookup(42),1);
    const f1 = try treap.insert(17);
    try expectEqual(f1,2);
    const f3 = try treap.insert(94);
    const t3 = try treap.insert(94);
    try expectEqual(f3,3);
    try expectEqual(f3,t3);
    _ = try treap.insert(43);
    _ = try treap.insert(44);
    try expectEqual(treap.lookup(44),5);
    var depths = [_]u32{0} ** (n*3);
    treap.depths(depths[0..]);
    // try std.io.getStdOut().writer().print("treap={}\n",.{treap});
    // try std.io.getStdOut().writer().print("depths={any}\n",.{depths});
}
test "full u64 treap alloc" {
    const expectEqual = @import("std").testing.expectEqual;
    const n = 21;
    var memory = [_]u8{0} ** (n*48);
    var treap = Treap_u64.init(memory[0..],compareU64,0);
    var index : u64 = 1;
    while (index<n*3) : (index += 1) {
        _ = try treap.insert(index);
    }
    try expectEqual(treap.lookup(44),44);
    var depths = [_]u32{0} ** (n*3);
    treap.depths(depths[0..]);
    // try std.io.getStdOut().writer().print("depths={any}\n",.{depths});
    // try std.io.getStdOut().writer().print("treap={}\n",.{treap});
}
