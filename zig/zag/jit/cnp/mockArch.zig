pub const nRegisters = floatOffset + floatRegisters;
const intRegisters = 31;
const floatRegisters = 32;
const floatOffset = intRegisters + 1;
pub const pcRegister = 0;
pub const spRegister = 1;
pub const processRegister = 2;
pub const contextRegister = 3;
pub const extraRegister = 4;

const Decoder = struct {
    address: [*]const Operation,
    const Self = @This();
    pub fn nextInstruction(self: *Self) Operation {
        const current = self.address;
        self.address = self.address + 1;
        return current[0];
    }
    fn new(address: [*]const Operation) Self {
        return .{.address = address};
    }
    pub fn getAddress(self: *Self) Address {
        return @constCast(@ptrCast(&self.address[0]));
    }
};
pub const decoder = Decoder.new;

pub fn emit(operation: Operation, buffer: anytype) void {
    @as([*]operation, @ptrFromInt(buffer.currentOffset()))[0] = operation;
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
