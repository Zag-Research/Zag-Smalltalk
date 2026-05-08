pub const nRegisters = floatOffset + floatRegisters;
const intRegisters = 31;
const floatRegisters = 32;
const floatOffset = intRegisters + 1;
pub const pcRegister = 0;
pub const spRegister = 1;
pub const processRegister = 2;
pub const contextRegister = 3;
pub const extraRegister = 4;

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

pub fn registerTypes() [nRegisters]RegisterContents {
    return [_]RegisterContents{.pc, .sp, .process, .context, .extra}
        ++ [_]RegisterContents{.unknown} ** (floatOffset-5)
        ++ [_]RegisterContents{.randFloat} ** floatRegisters;
}

pub const MockArch = @This();

const jit_ir = @import("../jit_ir.zig");
const Address = jit_ir.Address;
const Operation = jit_ir.Operation;
const RegisterContents = jit_ir.RegisterContents;
