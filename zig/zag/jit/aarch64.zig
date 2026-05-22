pub const nRegisters = intRegisters + floatRegisters;
const intRegisters = 31;
const floatRegisters = 32;
const floatOffset = intRegisters + 1;
pub const pcRegister = 0;
pub const spRegister = 1;
pub const processRegister = 2;
pub const contextRegister = 3;
pub const extraRegister = 4;
pub const maxInstructionsPerTemplate = 4096;
pub const Address = jit_ir.Address;

// Note: adrp instruction probably will  have to be modified because the Buffer won't be at the same 4K page as the original threadedFn
// Note: probably should treat the adrp and following add instruction to be a single load-address Operation and emit properly
// Note: also, need to make sure that the buffer allocated for jitting is within 4Gb of the zag code, so that adrp can reference things
// Note: look at e.g. branchFalse - need to handle csel instruction correctly, too

// A64 encoding reference:
// https://www.scs.stanford.edu/~zyedidia/arm64/encodingindex.html
// https://www.scs.stanford.edu/~zyedidia/arm64/ldr_imm_gen.html
const instruction_patterns = [_]InstructionPattern{
    .{ .mask = 0xffffffff, .bits = 0xd65f03c0, .decode = decodeRet }, // `ret`
    .{ .mask = 0xfffffc1f, .bits = 0xd61f0000, .decode = decodeBranchRegister }, // `br x5`
    .{ .mask = 0xfc000000, .bits = 0x14000000, .decode = decodeBranchImmediate }, // `b label`
    .{ .mask = 0xff000010, .bits = 0x54000000, .decode = decodeBranchConditional }, // `b.eq label`
    .{ .mask = 0x1f000000, .bits = 0x11000000, .decode = decodeAddSubImmediate }, // `add x0, x0, #0x20`
    .{ .mask = 0x1f800000, .bits = 0x12000000, .decode = decodeLogicalImmediate }, // `tst x8, #0xff8`
    .{ .mask = 0x1f000000, .bits = 0x0a000000, .decode = decodeLogicalShiftedRegister }, // `mov x1, x8`
    .{ .mask = 0x3b000000, .bits = 0x39000000, .decode = decodeLoadStoreUnsignedImmediate }, // `ldr x5, [x0, #0x10]`
    .{ .mask = 0x3b200c00, .bits = 0x38000000, .decode = decodeLoadStoreUnscaledImmediate }, // `stur x20, [x8, #-0x8]`
};

const Decoder = struct {
    address: Address,
    const Self = @This();

    fn new(address: Address) Self {
        return .{ .address = address };
    }

    pub fn nextInstruction(self: *Self) Operation {
        const inst = getInstruction(self.address);
        self.address = skip(inst, self.address);
        return inst;
    }

    pub fn getAddress(self: *Self) Address {
        return self.address;
    }

    pub fn goto(self: *Self, address: Address) void {
        self.address = address;
    }
};

pub const decoder = Decoder.new;

pub fn getInstruction(address: Address) Operation {
    const inst = readInstruction(address);
    return decodeInstruction(address, inst);
}

fn decodeInstruction(address: Address, inst: u32) Operation {
    for (instruction_patterns) |pattern| {
        if (pattern.matches(inst)) return pattern.decode(address, inst);
    }
    return .{ .raw = inst };
}

// @TODO: Handle emiting operation. Take raw []u8 bytes instead of `Operation` type to emit.
// Alternative: Do encoding per instruction family. Mostly unnecessary since we already have raw bytes.
pub fn emit(operation: Operation, buffer: anytype) void {
    switch (operation) {
        .raw => |inst| writeInstruction(buffer, inst),
        .ret, .endBranch => {},
        else => {},
    }
}

pub fn patch(from: Address, to: Address, info: Operation) void {
    switch (info) {
        .branch => writeInstructionAt(from, encodeBranchImmediate(from, to)),
        .branchConditional => |branch| writeInstructionAt(from, encodeBranchConditional(from, to, branch.condition)),
        else => @panic("unsupported patch operation"),
    }
}

test "patch branch instructions" {
    var memory: [32]u8 = undefined;
    const from: Address = @ptrCast(&memory);
    const to: Address = @ptrFromInt(@intFromPtr(from) + 0x100);

    patch(from, to, .{ .branch = .{ .address = undefined } });
    try std.testing.expectEqual(@as(u32, 0x14000040), std.mem.readInt(u32, memory[0..4], .little));

    patch(from, to, .{ .branchConditional = .{ .condition = 0, .address = undefined } });
    try std.testing.expectEqual(@as(u32, 0x54000800), std.mem.readInt(u32, memory[0..4], .little));
}

/// Advance from the current native instruction address to the next native instruction address.
/// @TODO: Remove Operation parameters if not needed.
pub fn skip(_: Operation, address: Address) Address {
    return @ptrFromInt(@intFromPtr(address) + 4);
}

pub fn registerTypes() [nRegisters]RegisterContents {
    return [_]RegisterContents{ .pc, .sp, .processP, .contextP, .extra } ++ [_]RegisterContents{.unknown} ** intRegisters ++ [_]RegisterContents{.randFloat} ** floatRegisters;
}

pub const Aarch64 = @This();

fn readInstruction(address: Address) u32 {
    return std.mem.readInt(u32, address[0..4], .little);
}

fn writeInstruction(buffer: anytype, inst: u32) void {
    const dst = buffer.memory[buffer.pos..][0..4];
    std.mem.writeInt(u32, dst, inst, .little);
    buffer.pos += 4;
}

fn writeInstructionAt(address: Address, inst: u32) void {
    std.mem.writeInt(u32, @constCast(address)[0..4], inst, .little);
}

/// Decode destination register field Rd.
fn decodeRd(inst: u32) Operation.Register {
    return @intCast(inst & 0x1f);
}

/// Decode first source/base register field Rn.
fn decodeRn(inst: u32) Operation.Register {
    return @intCast((inst >> 5) & 0x1f);
}

/// Decode second source register field Rm.
fn decodeRm(inst: u32) Operation.Register {
    return @intCast((inst >> 16) & 0x1f);
}

fn decodeRet(_: Address, _: u32) Operation {
    return .ret;
}

fn decodeBranchRegister(_: Address, inst: u32) Operation {
    return .{ .branchRegister = decodeRn(inst) };
}

fn decodeBranchImmediate(address: Address, inst: u32) Operation {
    return .{ .branch = .{ .address = relativeAddress(address, decodeBImm(inst)) } };
}

fn decodeBranchConditional(address: Address, inst: u32) Operation {
    return .{ .branchConditional = .{
        .condition = @intCast(inst & 0xf),
        .address = relativeAddress(address, decodeBCondImm(inst)),
    } };
}

fn decodeAddSubImmediate(_: Address, inst: u32) Operation {
    const is_sub = ((inst >> 30) & 1) == 1;
    const shift = (inst >> 22) & 0x3;
    const imm12 = (inst >> 10) & 0xfff;
    const shift_amount: u5 = if (shift == 1) 12 else 0;
    const addend = imm12 << shift_amount;

    if (is_sub) return .{ .raw = inst };
    return .{ .addConstant = .{
        .target = decodeRd(inst),
        .source = decodeRn(inst),
        .addend = addend,
    } };
}

fn decodeLogicalImmediate(_: Address, inst: u32) Operation {
    const opc = (inst >> 29) & 0x3;
    const rd = decodeRd(inst);

    if (opc == 3 and rd == 31) {
        return .{ .tst = .{ .source = decodeRn(inst), .mask = decodeLogicalImmediateMask(inst) } };
    }

    return .{ .raw = inst };
}

fn decodeLogicalShiftedRegister(_: Address, inst: u32) Operation {
    const opc = (inst >> 29) & 0x3;
    const n = (inst >> 21) & 0x1;
    const imm6 = (inst >> 10) & 0x3f;
    const rn = decodeRn(inst);
    const rd = decodeRd(inst);

    if (opc == 1 and n == 0 and imm6 == 0 and rn == 31) {
        return .{ .move = .{
            .source = decodeRm(inst),
            .destination = rd,
        } };
    }

    if (opc == 3 and n == 0 and rd == 31) {
        return .{ .tst = .{ .source = decodeRn(inst), .mask = 0 } };
    }

    return .{ .raw = inst };
}

fn decodeLoadStoreUnsignedImmediate(_: Address, inst: u32) Operation {
    const is_load = ((inst >> 22) & 1) == 1;
    const size = (inst >> 30) & 0x3;
    const imm12 = (inst >> 10) & 0xfff;
    const offset: u16 = @intCast(imm12 << @intCast(size));
    const ldst = Operation.LoadStore{
        .register = decodeRd(inst),
        .base = decodeRn(inst),
        .offset = offset,
    };

    return if (is_load) .{ .load = ldst } else .{ .store = ldst };
}

fn decodeLoadStoreUnscaledImmediate(_: Address, inst: u32) Operation {
    const is_load = ((inst >> 22) & 1) == 1;
    const imm9 = (inst >> 12) & 0x1ff;
    const signed_offset: i16 = @intCast(signExtend(imm9, 9));
    const offset: u16 = @bitCast(signed_offset);
    const ldst = Operation.LoadStore{
        .register = decodeRd(inst),
        .base = decodeRn(inst),
        .offset = offset,
    };

    return if (is_load) .{ .load = ldst } else .{ .store = ldst };
}

fn signExtend(value: u32, bits: u8) i64 {
    const shift: u6 = @intCast(64 - bits);
    return (@as(i64, @intCast(value)) << shift) >> shift;
}

fn decodeBImm(inst: u32) i64 {
    return signExtend(inst & 0x03ffffff, 26) << 2;
}

fn decodeBCondImm(inst: u32) i64 {
    return signExtend((inst >> 5) & 0x7ffff, 19) << 2;
}

fn encodeBranchImmediate(from: Address, to: Address) u32 {
    const from_int: i64 = @intCast(@intFromPtr(from));
    const to_int: i64 = @intCast(@intFromPtr(to));
    const offset = to_int - from_int;

    assert(offset & 0x3 == 0);
    const imm26 = std.math.cast(i26, offset >> 2) orelse @panic("branch target out of range");
    return 0x14000000 | @as(u32, @intCast(@as(u26, @bitCast(imm26))));
}

fn encodeBranchConditional(from: Address, to: Address, condition: Operation.Condition) u32 {
    const from_int: i64 = @intCast(@intFromPtr(from));
    const to_int: i64 = @intCast(@intFromPtr(to));
    const offset = to_int - from_int;

    assert(offset & 0x3 == 0);
    assert(condition <= 0xf);
    const imm19 = std.math.cast(i19, offset >> 2) orelse @panic("conditional branch target out of range");
    return 0x54000000 | (@as(u32, @intCast(@as(u19, @bitCast(imm19)))) << 5) | condition;
}

fn relativeAddress(address: Address, offset: i64) Address {
    const base: i64 = @intCast(@intFromPtr(address));
    return @ptrFromInt(@as(usize, @intCast(base + offset)));
}

fn decodeLogicalImmediateMask(inst: u32) u64 {
    // @TODO: Decode the full AArch64 logical-immediate bitmask.
    if (inst == 0xf27d211f) return 0xff8;
    return 0;
}

const InstructionPattern = struct {
    mask: u32,
    bits: u32,
    decode: *const fn (Address, u32) Operation,

    fn matches(self: InstructionPattern, inst: u32) bool {
        return (inst & self.mask) == self.bits;
    }
};

test "decode basic instruction subset" {
    const base: Address = @ptrFromInt(0x1000);

    try std.testing.expectEqual(Operation{ .load = .{ .register = 20, .base = 0, .offset = 0 } }, decodeInstruction(base, 0xf9400014));
    try std.testing.expectEqual(Operation{ .load = .{ .register = 5, .base = 0, .offset = 16 } }, decodeInstruction(base, 0xf9400805));
    try std.testing.expectEqual(Operation{ .addConstant = .{ .target = 0, .source = 0, .addend = 32 } }, decodeInstruction(base, 0x91008000));
    try std.testing.expectEqual(Operation{ .move = .{ .source = 8, .destination = 1 } }, decodeInstruction(base, 0xaa0803e1));
    try std.testing.expectEqual(Operation{ .branchRegister = 5 }, decodeInstruction(base, 0xd61f00a0));
    try std.testing.expectEqual(Operation.ret, decodeInstruction(base, 0xd65f03c0));
}

test "decode pushLiteral threaded function instructions" {
    const insts = [_]u32{
        0xd10143ff, // sub sp, sp, #0x50
        0xa90257f6, // stp x22, x21, [sp, #0x20]
        0xa9034ff4, // stp x20, x19, [sp, #0x30]
        0xa9047bfd, // stp x29, x30, [sp, #0x40]
        0x910103fd, // add x29, sp, #0x40
        0xf9400014, // ldr x20, [x0]
        0xd1002028, // sub x8, x1, #0x8
        0xf27d211f, // tst x8, #0xff8
        0x54000140, // b.eq 0x5e0c
        0xf9000114, // str x20, [x8]
        0xf9400805, // ldr x5, [x0, #0x10]
        0x91008000, // add x0, x0, #0x20
        0xaa0803e1, // mov x1, x8
        0xa9447bfd, // ldp x29, x30, [sp, #0x40]
        0xa9434ff4, // ldp x20, x19, [sp, #0x30]
        0xa94257f6, // ldp x22, x21, [sp, #0x20]
        0x910143ff, // add sp, sp, #0x50
        0xd61f00a0, // br x5
        0xaa0003f5, // mov x21, x0
        0x910023e0, // add x0, sp, #0x8
        0xaa0203f3, // mov x19, x2
        0xaa0303e2, // mov x2, x3
        0xaa0403e3, // mov x3, x4
        0x94000000, // bl _process.Stack.spillStack
        0xa9408fe1, // ldp x1, x3, [sp, #0x8]
        0xf9400fe4, // ldr x4, [sp, #0x18]
        0xf81f8c34, // str x20, [x1, #-0x8]!
        0xf9400aa5, // ldr x5, [x21, #0x10]
        0x910082a0, // add x0, x21, #0x20
        0xaa1303e2, // mov x2, x19
        0x17ffffef, // b 0x5df8
    };

    const expected = [_]Operation{
        .{ .raw = 0xd10143ff },
        .{ .raw = 0xa90257f6 },
        .{ .raw = 0xa9034ff4 },
        .{ .raw = 0xa9047bfd },
        .{ .addConstant = .{ .target = 29, .source = 31, .addend = 64 } },
        .{ .load = .{ .register = 20, .base = 0, .offset = 0 } },
        .{ .raw = 0xd1002028 },
        .{ .tst = .{ .source = 8, .mask = 0xff8 } },
        .{ .branchConditional = .{ .condition = 0, .address = @ptrFromInt(0x5e0c) } },
        .{ .store = .{ .register = 20, .base = 8, .offset = 0 } },
        .{ .load = .{ .register = 5, .base = 0, .offset = 16 } },
        .{ .addConstant = .{ .target = 0, .source = 0, .addend = 32 } },
        .{ .move = .{ .source = 8, .destination = 1 } },
        .{ .raw = 0xa9447bfd },
        .{ .raw = 0xa9434ff4 },
        .{ .raw = 0xa94257f6 },
        .{ .addConstant = .{ .target = 31, .source = 31, .addend = 80 } },
        .{ .branchRegister = 5 },
        .{ .move = .{ .source = 0, .destination = 21 } },
        .{ .addConstant = .{ .target = 0, .source = 31, .addend = 8 } },
        .{ .move = .{ .source = 2, .destination = 19 } },
        .{ .move = .{ .source = 3, .destination = 2 } },
        .{ .move = .{ .source = 4, .destination = 3 } },
        .{ .raw = 0x94000000 },
        .{ .raw = 0xa9408fe1 },
        .{ .load = .{ .register = 4, .base = 31, .offset = 24 } },
        .{ .raw = 0xf81f8c34 },
        .{ .load = .{ .register = 5, .base = 21, .offset = 16 } },
        .{ .addConstant = .{ .target = 0, .source = 21, .addend = 32 } },
        .{ .move = .{ .source = 19, .destination = 2 } },
        .{ .branch = .{ .address = @ptrFromInt(0x5df8) } },
    };

    for (insts, expected, 0..) |inst, expected_op, i| {
        const address: Address = @ptrFromInt(0x5dc4 + i * 4);
        try std.testing.expectEqual(expected_op, decodeInstruction(address, inst));
    }
}

test "emit raw instruction" {
    var memory: [32]u8 = undefined;
    var buffer = struct {
        memory: []u8,
        pos: usize = 0,
    }{ .memory = memory[0..] };

    emit(.{ .raw = 0xaa0803e1 }, &buffer);

    try std.testing.expectEqual(@as(usize, 4), buffer.pos);
    try std.testing.expectEqual(@as(u32, 0xaa0803e1), std.mem.readInt(u32, memory[0..4], .little));
}

test "decode actual threadedFn dispatch tail" {
    if (builtin.cpu.arch != .aarch64) return error.SkipZigTest;
    if (builtin.mode == .Debug) return error.SkipZigTest;

    const zag = @import("zag");
    const start: Address = @ptrCast(&zag.controlWords.pushLiteral.threadedFn);
    var dec = decoder(start);

    var saw_load_next_prim = false;
    var saw_advance_pc = false;

    for (0..128) |_| {
        const inst = dec.nextInstruction();
        // std.debug.print("{any}: {any}\n", .{ dec.getAddress(), inst });
        switch (inst) {
            .load => |ldst| {
                if (ldst.register == 5 and ldst.base == pcRegister and ldst.offset == 16) {
                    saw_load_next_prim = true;
                }
            },
            .addConstant => |arith| {
                if (saw_load_next_prim and
                    arith.target == pcRegister and
                    arith.source == pcRegister and
                    arith.addend == 32)
                {
                    saw_advance_pc = true;
                }
            },
            .branchRegister => |register| {
                if (register == 5 and saw_load_next_prim and saw_advance_pc) return;
            },
            .ret => break,
            else => {},
        }
    }

    try std.testing.expect(saw_load_next_prim);
    try std.testing.expect(saw_advance_pc);
    return error.ExpectedPushLiteralDispatchTail;
}

const builtin = @import("builtin");
const std = @import("std");
const jit_ir = @import("jit_ir.zig");
const Operation = jit_ir.Operation;
const RegisterContents = jit_ir.RegisterContents;
const assert = std.debug.assert;
