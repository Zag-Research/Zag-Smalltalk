pub const nRegisters = 32;
pub const maxInstructionsPerTemplate = 4096;

pub fn getInstruction(_: Address) Operation {
    // @TODO: Placeholder
    return .stop;
}

pub fn emit(operation: Operation, buffer: anytype) void {
    // @TODO: Placeholder
    _ = operation;
    _ = buffer;
}

/// Advance from the current native instruction address to the next native instruction address.
pub fn skip(_: Operation, address: Address) Address {
    return .{ .address = @ptrFromInt(@intFromPtr(address.address) + 4) };
}

pub const Arm64 = @This();

const machine_ir = @import("machine_ir.zig");
const Address = machine_ir.Address;
const Operation = machine_ir.Operation;
