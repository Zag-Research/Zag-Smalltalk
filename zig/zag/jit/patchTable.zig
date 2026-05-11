pub fn PatchTable(AddressType: anytype, InfoType: anytype, mapSize: usize, patchSize: usize) type {
    const MapElement = struct {
        source: ?AddressType,
        status: Status,
        pending: ?*Self,
        resolution: ?AddressType,
        const Self = @This();
        const Status = enum { free, new, defined, referenced };
        fn before(self: *Self, other: *Self) bool {
            return @intFromPtr(self.source) < @intFromPtr(other.source);
        }
    };
    const PatchElement = struct {
        next: ?*Self,
        address: AddressType,
        info: InfoType,
        const Self = @This();
    };
    return struct {
        map: [mapSize]MapElement,
        patch: [patchSize]PatchElement,
        freePatch: ?*PatchElement,
        pending: ?*MapElement,
        const Self = @This();
        pub fn init(self: *Self) void {
            var last: ?*PatchElement = null;
            for (&self.patch) |*pe| {
                pe.next = last;
                last = pe;
            }
            self.freePatch = last;
            self.clearMap();
        }
        pub fn clearMap(self: *Self) void {
            for (&self.map) |*am| {
                am.source = null;
                am.status = .free;
                am.pending = null;
            }
            self.pending = null;
        }
        pub fn deinit(_: *Self) void {}
        fn atOrDefine(self: *Self, source: AddressType) *MapElement {
            const hashed = (@intFromPtr(source) >> 8) % mapSize;
            // linear probe from there to the end
            for (self.map[hashed..mapSize]) |*element| {
                if (element.source) |addr| {
                    if (addr == source) return element;
                } else {
                    assert(element.status == .free);
                    element.source = source;
                    element.status = .new;
                    return element;
                }
            }
            // rest of linear probe for the first part of the array
            for (self.map[0..hashed]) |*element| {
                if (element.source) |addr| {
                    if (addr == source) return element;
                } else {
                    assert(element.status == .free);
                    element.source = source;
                    element.status = .new;
                    return element;
                }
            }
            @panic("map too small");
        }
        const PatchIterator = struct {
            nextElement: ?*PatchElement,
            current: ?*PatchElement,
            freelist: *?*PatchElement,
            pub fn next(self: *PatchIterator) ?*PatchElement {
                if (self.current) |pe| {
                    pe.next = self.freelist.*;
                    self.freelist.* = pe;
                    const nxt = self.nextElement;
                    self.current = nxt;
                    if (nxt) |then|
                        self.nextElement = then.next;
                    return pe;
                } else
                    return null;
            }
            fn new(pe:?*PatchElement, freeList:*?*PatchElement) PatchIterator {
                return .{
                    .nextElement = if (pe) |tpe| tpe.next else null,
                    .current = pe,
                    .freelist = freeList,
                };
            }
        };
        pub fn definition(self: *Self, define: AddressType, as: AddressType) PatchIterator {
            const entry = self.atOrDefine(define);
            switch (entry.status) {
                .new => {
                    entry.resolution = as;
                    entry.status = .defined;
                },
                .referenced => {
                    const queue:?*PatchElement = @ptrCast(entry.resolution);
                    entry.resolution = as;
                    entry.status = .defined;
                    return PatchIterator.new(queue, &self.freePatch);
                },
                .defined => @panic("multiply defined"),
                .free => unreachable,
            }
            return PatchIterator.new(null, &self.freePatch);
        }

        pub fn reference(self: *Self, target: AddressType, from: AddressType, info: InfoType) ?AddressType {
            const entry = self.atOrDefine(target);
            sw: switch (entry.status) {
                .new => {
                    entry.resolution = null;
                    entry.status = .referenced;
                    self.addPending(entry);
                    continue :sw .referenced;
                },
                .defined => {
                    return entry.resolution;
                },
                .referenced => {
                    if (self.freePatch) |patch| {
                        self.freePatch = patch.next;
                        patch.address = from;
                        patch.info = info;
                        patch.next = @ptrCast(entry.resolution);
                        entry.resolution = @ptrCast(patch);
                    } else @panic("no free patches");
                    return null;
                },
                .free => unreachable,
            }
        }
        pub fn externalReference(self: *Self, target: AddressType) void {
            const entry = self.atOrDefine(target);
            entry.status = .referenced;
            self.addPending(entry);
        }
        fn addPending(self: *Self, entry: *MapElement) void {
            if (self.pending) |pending| {
                if (pending.before(entry)) {
                    var me = pending;
                    while (true) {
                        if (me.pending) |next| {
                            if (entry.before(next)) {
                                entry.pending = next;
                                me.pending = entry;
                                return;
                            }
                            me = next;
                        } else {
                            entry.pending = null;
                            me.pending = entry;
                            return;
                        }
                    }
                } else {
                    entry.pending = self.pending;
                    self.pending = entry;
                    return;
                }
            }
            entry.pending = null;
            self.pending = entry;
        }
        pub fn getPending(self: *Self) ?AddressType {
            while (self.pending) |pending| {
                self.pending = pending.pending;
                pending.pending = null;
                if (pending.status == .referenced)
                    return pending.source;
            }
            return null;
        }
    };
}
test "patchTable" {
    var pt: PatchTable(*u64, u64, 5, 5) = undefined;
    pt.init();
    defer pt.deinit();
    var data = [_]u64{1,2,3,4,5,6,7,8,9,10};
    pt.externalReference(&data[2]);
    try expectEqual(&data[2], pt.getPending());
    try expectEqual(null, pt.getPending());
    pt.clearMap();
    var d1 = pt.definition(&data[2], &data[4]);
    try expectEqual(null, d1.next());
    try expectEqual(&data[4], pt.reference(&data[2], &data[5], 42));
    pt.init();
    // reference before definition returns an iterator
    try expectEqual(null, pt.reference(&data[2], &data[6], 17));
    try expectEqual(null, pt.reference(&data[2], &data[5], 42));
    try expectEqual(null, pt.reference(&data[3], &data[7], 91));
    try expectEqual(null, pt.reference(&data[1], &data[8], 97));
    try expectEqual(&data[1], pt.getPending());
    try expectEqual(&data[2], pt.getPending());
    try expectEqual(&data[3], pt.getPending());
    try expectEqual(null, pt.getPending());
    var d2 = pt.definition(&data[3], &data[1]);
    if (d2.next()) |p| {
        try expectEqual(&data[7], p.address);
        try expectEqual(91, p.info);
    } else return error.NoFixupTable;
    try expectEqual(null, d2.next());
    var d3 = pt.definition(&data[2], &data[4]);
    if (d3.next()) |p| {
        try expectEqual(&data[5], p.address);
        try expectEqual(42, p.info);
    } else return error.NoFixupTable;
    if (d3.next()) |p| {
        try expectEqual(&data[6], p.address);
        try expectEqual(17, p.info);
    } else return error.ShortFixupTable;
    try expectEqual(null, d3.next());
    try expectEqual(&data[4], pt.reference(&data[2], &data[5], 42));
}
const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
