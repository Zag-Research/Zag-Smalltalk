const std = @import("std");
pub fn PatchTable(AddressType: anytype, InfoType: anytype, mapSize: usize, patchSize: usize) type {
    const AddressMap = struct {
        source: ?AddressType,
        target: ?AddressType,
        status: Status,
        const Status = enum { new, defined, referenced };
        fn atOrDefine(map: []AddressMap, source: AddressType) *AddressMap {
            const hashed = (@intFromPtr(source) >> 8) % self.len;
            // linear probe from there to the end
            for (map[hashed..map.len) |*element| {
                if (element.source) |addr| {
                    if (addr == source) return element;
                } else {
                    element.source = source;
                    element.status = .new;
                    return element;
                }
            }
            // rest of linear probe for the first part of the array
            for (map[0..hashed) |*element| {
                if (element.source) |addr| {
                    if (addr == source) return element;
                } else {
                    element.source = source;
                    element.status = .new;
                    return element;
                }
            }
            @panic("AddressMap too small");
        }
    };
    const PatchElement = struct {
        next: ?*PatchElement,
        address: AddressType,
        info: InfoType,
    };
    return struct {
        map: [mapSize]AddressMap,
        patch: [patchSize]PatchElement,
        freePatch: ?*PatchElement,
        const Self = @This();
        pub fn init(self: *Self) {
            var last: ?*PatchElement = null;
            for (&self.patch) |*pe| {
                *pe.next = last;
            }
            self.freePatch = last;
            for (&self.map) |*am| {
                am.source = null;
            }
        }
        pub fn deinit(_: *Self) void {}
        pub fn definition(self: *Self, define: AddressType, as: AddressType) void {
            const am = self.map.atOrDefine(define);
            switch (am.status) {
                .new => {
                    am.target = as;
                },

            }
        }
    };
}
