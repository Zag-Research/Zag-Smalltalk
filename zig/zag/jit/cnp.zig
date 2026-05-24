/// Example Usage:
/// var jit: CopyAndPatch(currentArch) = undefined;
/// try jit.init();
/// defer jit.deinit();
/// const code = try jit.jitMethod(method);
/// method.jitted = @ptrCast(@alignCast(code.ptr));
pub fn CopyAndPatch(Code: anytype, Arch: anytype, JitBuffer: anytype) type {
    const Address = Arch.Address;
    const NativePatchType = PatchTable(Address, Address, Operation, mapSize, patchSize);
    const ThreadedPatchType = PatchTable([*]const Code, Address, Operation, threadedMapSize, threadedPatchSize);

    return struct {
        buffer: JitBuffer,
        reg_type: [Arch.nRegisters]RegisterContents,
        reg_value: [Arch.nRegisters]u64,
        native_patch: NativePatchType,
        threaded_patch: ThreadedPatchType,
        const Self = @This();

        pub fn init(self: *Self) !void {
            self.buffer = try JitBuffer.init(maxMethodJitSize);
            self.native_patch.init();
            self.threaded_patch.init();
        }

        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }

        pub fn jitMethod(self: *Self, method: *const CompiledMethod) !void {
            return self.jitCode(method.codeSlice());
        }

        fn jitCode(self: *Self, code: []const Code) !void {
            self.buffer.makeWritable();
            self.threaded_patch.externalReference(code.ptr);
            while (self.threaded_patch.popPending()) |cp| {
                try self.abstractInterpret(cp);
            }
            self.buffer.makeExecutable();
        }

        fn abstractInterpret(self: *Self, initial_cp: [*]const Code) !void {
            const entry_pc = @intFromPtr(initial_cp + 1);
            self.resetAbstractState(entry_pc);
            self.native_patch.clearMap();

            var decoder = Arch.decoder(@ptrCast(initial_cp[0].threadedFn));
            self.define(&self.threaded_patch, initial_cp);

            nextInstruction: while (true) {
                const instruction = decoder.nextInstruction();
                var inst: Operation = instruction.operation;
                self.define(&self.native_patch, instruction.address);
                instSw: switch (inst) {
                    .ret => break,
                    .move => |move| {
                        self.reg_type[move.destination] = self.reg_type[move.source];
                        self.reg_value[move.destination] = self.reg_value[move.source];
                    },
                    .load => |ldst| {
                        switch (self.reg_type[ldst.base]) {
                            .pc => {
                                self.reg_type[ldst.register] = .codeAddress;
                                self.reg_value[ldst.register] = self.reg_value[ldst.base] + ldst.offset;
                                continue;
                            },
                            .context => {
                                switch (ldst.offset) {
                                    @offsetOf(Context, "prevCtxt") => self.reg_type[ldst.register] = .context,
                                    @offsetOf(Context, "tpc") => self.reg_type[ldst.register] = .unknownPc,
                                    @offsetOf(Context, "npc") => self.reg_type[ldst.register] = .executableAddress,
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    },
                    .loadPostIndex => |ldst| {
                        switch (self.reg_type[ldst.base]) {
                            .pc, .codeAddress => {
                                self.reg_type[ldst.register] = .codeAddress;
                                self.reg_value[ldst.register] = self.reg_value[ldst.base];
                                self.reg_value[ldst.base] += ldst.offset;
                                continue :nextInstruction;
                            },
                            else => {},
                        }
                    },
                    .addConstant => |arith| {
                        switch (self.reg_type[arith.source]) {
                            .pc, .codeAddress => {
                                self.reg_type[arith.target] = self.reg_type[arith.source];
                                self.reg_value[arith.target] = self.reg_value[arith.source] + arith.addend;
                                continue :nextInstruction;
                            },
                            else => {},
                        }
                    },
                    .add => |arith| {
                        if (self.reg_type[arith.source] == self.reg_type[arith.addend]) {
                            self.reg_type[arith.target] = self.reg_type[arith.source];
                            self.reg_value[arith.target] = self.reg_value[arith.source] + self.reg_value[arith.addend];
                            continue :nextInstruction;
                        }
                    },
                    .branchConditional => |branch| {
                        _ = self.native_patch.reference(self.nativeAddress(branch.address), self.bufferAddress(), inst);
                        self.emitOperation(inst);
                        continue :nextInstruction;
                    },
                    .branchRegister => |register| {
                        sw: switch (self.reg_type[register]) {
                            .codeAddress => {
                                inst = .{ .addConstant = .{
                                    .source = Arch.pcRegister,
                                    .target = Arch.pcRegister,
                                    .addend = self.reg_value[Arch.pcRegister] - entry_pc,
                                } };
                                self.emitOperation(inst);

                                const target: [*]const Code = @ptrFromInt(self.reg_value[register]);

                                inst = .{ .branch = .{ .address = undefined } };
                                _ = self.threaded_patch.reference(target, self.bufferAddress(), inst);

                                continue :sw .executableAddress;
                            },
                            .executableAddress => {
                                // for debugging, should check that the 5 registers have the right types
                                assert(self.reg_type[Arch.pcRegister] == .pc or self.reg_type[Arch.pcRegister] == .unknownPc);
                                assert(self.reg_type[Arch.spRegister] == .sp);
                                assert(self.reg_type[Arch.processRegister] == .process);
                                assert(self.reg_type[Arch.contextRegister] == .context);
                                assert(self.reg_type[Arch.extraRegister] == .extra);
                                continue :instSw .endBranch;
                            },
                            else => @panic("branchRegister to non-executable"),
                        }
                    },
                    .branch => |branch| {
                        _ = self.native_patch.reference(self.nativeAddress(branch.address), self.bufferAddress(), inst);
                        continue :instSw .endBranch;
                    },
                    .endBranch => {
                        self.emitOperation(inst);

                        if (self.native_patch.popPending()) |addr| {
                            decoder.goto(addr);
                            continue :nextInstruction;
                        } else break;
                    },
                    else => {},
                }

                Arch.emitInstruction(instruction, &self.buffer);
            }
        }

        fn emitOperation(self: *Self, operation: Operation) void {
            Arch.emitInstruction(.{
                .address = self.bufferAddress(),
                .raw = undefined,
                .operation = operation,
            }, &self.buffer);
        }

        fn bufferAddress(self: *Self) Address {
            return @ptrCast(self.buffer.getAddress());
        }

        fn nativeAddress(_: *Self, address: jit_ir.Address) Address {
            return @ptrCast(@alignCast(@constCast(address)));
        }

        fn define(self: *Self, patch_table: anytype, source_address: anytype) void {
            const emitted_address = self.bufferAddress();

            var patches = patch_table.definition(
                source_address,
                emitted_address,
            );

            while (patches.next()) |patch| {
                Arch.patch(
                    patch.address,
                    emitted_address,
                    patch.info,
                );
            }
        }

        fn resetAbstractState(self: *Self, pc: u64) void {
            self.reg_type = Arch.registerTypes();
            self.reg_value = [_]u64{0} ** Arch.nRegisters; // don't need to, but may be useful for debugging
            self.reg_value[Arch.pcRegister] = pc;
        }
    };
}

const threadedMapSize = 100;
const threadedPatchSize = 200;
const mapSize = 1000;
const patchSize = 2000;
const maxMethodJitSize = 32768;

const TestCode = union(enum) {
    object: i64,
    threadedFn: [*]const Operation,
    codePtr: *@This(),
};

const TestJitBuffer = struct {
    buffer: [10]Operation = undefined,
    pos: usize = 0,
    const Self = @This();
    const Address = [*]const Operation;
    fn init(_: usize) !Self {
        return .{};
    }
    pub fn deinit(_: *Self) void {}
    pub fn makeWritable(_: *Self) void {}
    pub fn makeExecutable(_: *Self) void {}
    pub fn append(self: *Self, values: []const Operation) void {
        for (values) |op| {
            self.buffer[self.pos] = op;
            self.pos = self.pos + 1;
        }
    }
    pub fn getAddress(self: *Self) Address {
        return @as(Address, @ptrCast(&self.buffer)) + self.pos;
    }
    pub fn slice(self: *Self) []std.meta.Child(Address) {
        return self.buffer[0..self.pos];
    }
};

const TestArch = MockArch(TestJitBuffer.Address);

// this command (with the magic corrected) can be used to test CopyAndPatch
// zig test --dep zag -Mroot=cnp.zig --dep options -Mzag=../zag.zig -Moptions=.../.zig-cache/c/.../options.zig
// to find the right path, use the most recent of:
// find ../../.zig-cache/c -name options.zig -ls

test "smoke: copy linear test" {
    const tf1 = [_]Operation{ .{ .tst = .{ .source = 5, .mask = 7 } }, .ret };
    const m1 = [_]TestCode{.{ .threadedFn = &tf1 }};

    var cnp: CopyAndPatch(TestCode, TestArch, TestJitBuffer) = undefined;
    try cnp.init();
    defer cnp.deinit();

    try cnp.jitCode(&m1);

    try std.testing.expectEqualSlices(Operation, &.{
        .{ .tst = .{ .source = 5, .mask = 7 } },
    }, cnp.buffer.slice());
}

test "patch threaded branch" {
    const dispatch = [_]Operation{
        .{ .load = .{ .register = 5, .base = TestArch.pcRegister, .offset = 0 } },
        .{ .branchRegister = 5 },
    };

    const continuation = [_]Operation{
        .{ .tst = .{ .source = 7, .mask = 3 } },
        .ret,
    };

    const method = [_]TestCode{
        .{ .threadedFn = &dispatch },
        .{ .threadedFn = &continuation },
    };

    var cnp: CopyAndPatch(TestCode, TestArch, TestJitBuffer) = undefined;
    try cnp.init();
    defer cnp.deinit();

    try cnp.jitCode(&method);

    const emitted = cnp.buffer.slice();

    try std.testing.expectEqual(@as(usize, 3), emitted.len);

    try std.testing.expectEqual(Operation{
        .addConstant = .{
            .addend = 0,
            .target = 0,
            .source = 0,
        },
    }, emitted[0]);

    try std.testing.expectEqual(Operation{
        .branch = .{
            .address = @ptrCast(&emitted[2]),
        },
    }, emitted[1]);

    try std.testing.expectEqual(Operation{
        .tst = .{ .source = 7, .mask = 3 },
    }, emitted[2]);
}

test "operand-consuming threaded word " {
    const dispatch = [_]Operation{
        // Entry PC is &method[1]. This simulates consuming one operand/literal slot,
        // so the next threaded function lives at &method[2].
        .{ .load = .{
            .register = 5,
            .base = TestArch.pcRegister,
            .offset = @sizeOf(TestCode),
        } },

        .{ .addConstant = .{
            .source = TestArch.pcRegister,
            .target = TestArch.pcRegister,
            .addend = @sizeOf(TestCode),
        } },

        .{ .addConstant = .{
            .source = TestArch.pcRegister,
            .target = TestArch.pcRegister,
            .addend = @sizeOf(TestCode),
        } },

        .{ .branchRegister = 5 },
    };

    const literal_operand = TestCode{ .object = 123 };

    const continuation = [_]Operation{
        .{ .tst = .{ .source = 7, .mask = 3 } },
        .ret,
    };

    const method = [_]TestCode{
        .{ .threadedFn = &dispatch },
        literal_operand,
        .{ .threadedFn = &continuation },
    };

    var cnp: CopyAndPatch(TestCode, TestArch, TestJitBuffer) = undefined;
    try cnp.init();
    defer cnp.deinit();

    try cnp.jitCode(&method);

    const emitted = cnp.buffer.slice();

    try std.testing.expectEqual(@as(usize, 3), emitted.len);

    try std.testing.expectEqual(Operation{
        .addConstant = .{
            .source = TestArch.pcRegister,
            .target = TestArch.pcRegister,
            .addend = 2 * @sizeOf(TestCode),
        },
    }, emitted[0]);

    try std.testing.expectEqual(Operation{
        .branch = .{
            .address = @ptrCast(&emitted[2]),
        },
    }, emitted[1]);

    try std.testing.expectEqual(Operation{
        .tst = .{ .source = 7, .mask = 3 },
    }, emitted[2]);
}

test "conditional native branch paths dispatch to same continuation" {
    const continuation = [_]Operation{
        .{ .tst = .{ .source = 9, .mask = 9 } },
        .ret,
    };

    var template = [_]Operation{
        .{ .branchConditional = .{ .condition = 0, .address = undefined } },

        .{ .tst = .{ .source = 1, .mask = 1 } },
        .{ .load = .{ .register = 5, .base = TestArch.pcRegister, .offset = 0 } },
        .{ .branchRegister = 5 },

        .{ .tst = .{ .source = 2, .mask = 2 } },
        .{ .load = .{ .register = 5, .base = TestArch.pcRegister, .offset = 0 } },
        .{ .branchRegister = 5 },
    };

    template[0] = .{ .branchConditional = .{
        .condition = 0,
        .address = @ptrCast(&template[4]),
    } };

    const method = [_]TestCode{
        .{ .threadedFn = &template },
        .{ .threadedFn = &continuation },
    };

    var cnp: CopyAndPatch(TestCode, TestArch, TestJitBuffer) = undefined;
    try cnp.init();
    defer cnp.deinit();

    try cnp.jitCode(&method);

    const emitted = cnp.buffer.slice();

    try std.testing.expectEqual(@as(usize, 8), emitted.len);

    try std.testing.expectEqual(Operation{
        .branchConditional = .{
            .condition = 0,
            .address = @ptrCast(&emitted[4]),
        },
    }, emitted[0]);

    try std.testing.expectEqual(Operation{
        .tst = .{ .source = 1, .mask = 1 },
    }, emitted[1]);

    try std.testing.expectEqual(Operation{
        .addConstant = .{
            .source = TestArch.pcRegister,
            .target = TestArch.pcRegister,
            .addend = 0,
        },
    }, emitted[2]);

    try std.testing.expectEqual(Operation{
        .branch = .{
            .address = @ptrCast(&emitted[7]),
        },
    }, emitted[3]);

    try std.testing.expectEqual(Operation{
        .tst = .{ .source = 2, .mask = 2 },
    }, emitted[4]);

    try std.testing.expectEqual(Operation{
        .addConstant = .{
            .source = TestArch.pcRegister,
            .target = TestArch.pcRegister,
            .addend = 0,
        },
    }, emitted[5]);

    try std.testing.expectEqual(Operation{
        .branch = .{
            .address = @ptrCast(&emitted[7]),
        },
    }, emitted[6]);

    try std.testing.expectEqual(Operation{
        .tst = .{ .source = 9, .mask = 9 },
    }, emitted[7]);
}

test "aarch64: copy-and-patch actual pushLiteral threadedFn" {
    if (builtin.cpu.arch != .aarch64) return error.SkipZigTest;
    if (builtin.mode == .Debug) return error.SkipZigTest;

    const Code = zag.execute.Code;
    const Aarch64 = @import("aarch64.zig");
    const NativeJitBuffer = @import("jit_buffer.zig").JitBuffer;

    const method = [_]Code{
        Code.primOf(zag.controlWords.pushLiteral.threadedFn),
        .{ .object = zag.object.Nil() },
        Code.primOf(zag.controlWords.drop.threadedFn),
        Code.endCode,
    };

    var cnp: CopyAndPatch(Code, Aarch64, NativeJitBuffer) = undefined;
    try cnp.init();
    defer cnp.deinit();

    try cnp.jitCode(&method);

    var decoder = Aarch64.decoder(@ptrCast(cnp.buffer.memory.ptr));
    const end = @intFromPtr(cnp.buffer.memory.ptr) + cnp.buffer.pos;
    var saw_pc_add = false;
    var saw_branch = false;

    while (@intFromPtr(decoder.getAddress()) < end) {
        const instruction = decoder.nextInstruction();
        switch (instruction.operation) {
            .addConstant => |arith| {
                if (arith.source == Aarch64.pcRegister and
                    arith.target == Aarch64.pcRegister and
                    arith.addend == 32)
                {
                    saw_pc_add = true;
                }
            },
            .branch => saw_branch = true,
            else => {},
        }
    }

    try std.testing.expect(saw_pc_add);
    try std.testing.expect(saw_branch);
}

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

const std = @import("std");
const builtin = @import("builtin");
const debug = std.debug;
const assert = debug.assert;

const zag = @import("zag");
const Context = zag.Context;
const Process = zag.Process;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;
const CompiledMethod = zag.execute.CompiledMethod;

const MockArch = @import("cnp/mockArch.zig").MockArch;
const jit_ir = @import("jit_ir.zig");
const Operation = jit_ir.Operation;
const RegisterContents = jit_ir.RegisterContents;
const PatchTable = @import("patchTable.zig").PatchTable;
