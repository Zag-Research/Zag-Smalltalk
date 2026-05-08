const std = @import("std");

pub fn PatchTable(AddressType: type, InfoType: type, comptime map_size: usize, comptime patch_size: usize) type {
    const PatchElement = struct {
        target: AddressType,
        patch_address: usize,
        info: InfoType,
    };

    const AddressMap = struct {
        source: AddressType = undefined,
        target: ?usize = null,
        status: Status = .empty,
        pending: bool = false,
        // @TODO: Add a per-entry pending list rather a flat array. Remove existing global patch list.

        const Status = enum { empty, referenced, defined };
    };

    return struct {
        map: [map_size]AddressMap = [_]AddressMap{.{}} ** map_size,
        patch: [patch_size]PatchElement = undefined,
        patch_count: usize = 0,
        pending_index: usize = 0,

        const Self = @This();

        pub fn init(self: *Self) void {
            self.* = .{};
        }

        pub fn deinit(_: *Self) void {}

        pub fn externalReference(self: *Self, source: AddressType) void {
            const entry = self.atOrInsert(source);
            if (entry.status == .empty) entry.status = .referenced;
            entry.pending = true;
        }

        pub fn getPending(self: *Self) ?AddressType {
            while (self.pending_index < self.map.len) : (self.pending_index += 1) {
                const entry = &self.map[self.pending_index];
                if (entry.status != .empty and entry.pending) {
                    entry.pending = false;
                    self.pending_index += 1;
                    return entry.source;
                }
            }
            self.pending_index = 0;
            return null;
        }

        pub fn define(self: *Self, source: AddressType, target: usize) void {
            const entry = self.atOrInsert(source);
            entry.target = target;
            entry.status = .defined;
            entry.pending = false;
            // @TODO: Resolve pending patch records that target source.
            // @TODO: handle duplicate definitions
        }

        pub fn reference(self: *Self, source: AddressType, patch_address: usize, info: InfoType) void {
            const entry = self.atOrInsert(source);
            if (entry.status == .empty) entry.status = .referenced;
            // @TODO: Patch immediately when source is already defined.

            if (self.patch_count >= self.patch.len) @panic("PatchTable patch list too small");
            self.patch[self.patch_count] = .{
                .target = source,
                .patch_address = patch_address,
                .info = info,
            };
            self.patch_count += 1;
        }

        fn atOrInsert(self: *Self, source: AddressType) *AddressMap {
            // Moved to non-hash version bceause the PC is a non-pointer type
            // @CHECK: Revert if hashing is required
            for (&self.map) |*entry| {
                if (entry.status != .empty and std.meta.eql(entry.source, source)) return entry;
            }
            for (&self.map) |*entry| {
                if (entry.status == .empty) {
                    entry.source = source;
                    return entry;
                }
            }
            @panic("PatchTable map too small");
        }
    };
}
