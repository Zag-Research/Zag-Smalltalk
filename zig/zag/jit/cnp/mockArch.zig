pub fn MockArch(AddressType: anytype) type {
    return struct {
        pub const nRegisters = floatOffset + floatRegisters;
        const intRegisters = 31;
        const floatRegisters = 32;
        const floatOffset = intRegisters + 1;
        pub const pcRegister = 0;
        pub const spRegister = 1;
        pub const processRegister = 2;
        pub const contextRegister = 3;
        pub const extraRegister = 4;
        pub const Address = AddressType;
        const Decoder = struct {
            address: [*]const Operation,
            const Self = @This();
            fn new(address: [*]const Operation) Self {
                return .{ .address = address };
            }
            pub fn nextInstruction(self: *Self) Operation {
                const current = self.address;
                self.address = self.address + 1;
                return current[0];
            }
            pub fn getAddress(self: *Self) [*]const Operation {
                return self.address;
            }
            pub fn goto(self: *Self, address: [*]const Operation) void {
                self.address = address;
            }
        };
        pub const decoder = Decoder.new;

        pub fn emit(operation: Operation, buffer: anytype) void {
            const oBuff = [_]Operation{operation};
            buffer.append(&oBuff);
        }

        pub fn patch(from: anytype, to: anytype, info: Operation) void {
            // Note: This only works for [*]Operation AddressType. Maybe add different variants to this.
            // or add a comptime assert to check for the type. There is same assumption for the decoder
            const slot: [*]Operation = @ptrCast(@alignCast(from));
            slot[0] = switch (info) {
                .branch => .{ .branch = .{ .address = @ptrCast(to) } },
                .branchConditional => |branch| .{ .branchConditional = .{
                    .condition = branch.condition,
                    .address = @ptrCast(to),
                } },
                else => @panic("unsupported patch operation"),
            };
        }

        /// Advance from the current native instruction address to the next native instruction address.
        pub fn skip(_: Operation, address: Address) Address {
            return .{ .address = @ptrFromInt(@intFromPtr(address.address) + 4) };
        }

        pub fn registerTypes() [nRegisters]RegisterContents {
            return [_]RegisterContents{ .pc, .sp, .process, .context, .extra } ++ [_]RegisterContents{.unknown} ** (floatOffset - 5) ++ [_]RegisterContents{.randFloat} ** floatRegisters;
        }
    };
}
const jit_ir = @import("../jit_ir.zig");
const Operation = jit_ir.Operation;
const RegisterContents = jit_ir.RegisterContents;
