pub fn PatchTable(AddressType: anytype, InfoType: anytype, mapSize: usize, patchSize: usize) type {
    const MapElement = struct {
        source: ?AddressType,
        target: ?AddressType,
        pending: ?*Self,
        status: Status,
        const Self = @This();
        const Status = enum { free, new, defined, referenced };
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
            }
        }
        pub fn deinit(_: *Self) void {}
        fn atOrDefine(map: []MapElement, source: AddressType) !*Self {
            const hashed = (@intFromPtr(source) >> 8) % map.len;
            // linear probe from there to the end
            for (map[hashed..map.len]) |*element| {
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
            for (map[0..hashed]) |*element| {
                if (element.source) |addr| {
                    if (addr == source) return element;
                } else {
                    assert(element.status == .free);
                    element.source = source;
                    element.status = .new;
                    return element;
                }
            }
            return error.TooSmall;
        }
        const PatchIterator = struct {
            nextElement: ?*PatchElement,
            current: ?*PatchElement,
            freelist: *?*PatchElement,
            pub fn next(self: *PatchIterator) ?PatchElement {
                if (self.current) |*pe| {
                    pe.next = self.freelist.*;
                    self.freelist.* = pe;
                    const nxt = self.nextElement;
                    self.current = nxt;
                    if (nxt) |_|
                        self.nextElement = nxt.next;
                    return nxt;
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
            const entry = atOrDefine(&self.map, define);
            switch (entry.status) {
                .new => {
                    entry.target = as;
                    entry.status = .defined;
                },
                .referenced => {
                    const queue:?*PatchElement = @ptrCast(entry.target);
                    entry.target = as;
                    entry.status = .defined;
                    return PatchIterator.new(queue, &self.freePatch);
                },
                .defined => @panic("multiply defined"),
                .free => unreachable,
            }
            return null;
        }

        pub fn reference(self: *Self, target: AddressType, from: AddressType, info: InfoType) ?AddressType {
            const entry = atOrDefine(&self.map, target);
            sw: switch (entry.status) {
                .new => {
                    entry.target = null;
                    entry.status = .referenced;
                    continue :sw .referenced;
                },
                .defined => {
                    return entry.target;
                },
                .referenced => {
                    if (self.freePatch) |patch| {
                        patch.next = @ptrCast(entry.target);
                        patch.address = from;
                        patch.info = info;
                        entry.target = @ptrCast(patch);
                    } else @panic("no free patches");
                    return null;
                },
                .free => unreachable,
            }
            self.addPending(entry);
        }
        fn addPending(self: *Self, entry: *Self) void {
            entry.pending = self.pending;
            self.pending = entry;
        }
        pub fn externalReference(self: *Self, target: AddressType) void {
            const entry = atOrDefine(&self.map, target);
            entry.status = .referenced;
            self.addPending(entry);
        }
        pub fn getPending(self: *Self) ?AddressType {
            while (self.pending) |pending| {
                self.pending = pending.pending;
                if (pending.status == .referenced)
                    return pending;
            }
            return null;
        }
    };
}
test "patchTable" {
    var pt: PatchTable(*u64, u64, 10, 10) = undefined;
    pt.init();
    defer pt.deinit();


}
const std = @import("std");
const assert = std.debug.assert;
