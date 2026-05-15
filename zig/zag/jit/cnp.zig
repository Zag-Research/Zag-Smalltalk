/// Example Usage:
/// var jit: CopyAndPatch(currentArch) = undefined;
/// try jit.init();
/// defer jit.deinit();
/// const code = try jit.jitMethod(method);
/// method.jitted = @ptrCast(@alignCast(code.ptr));
pub fn CopyAndPatch(Code: anytype, Arch: anytype, JitBuffer: anytype) type {
    const Address = Arch.Address;
    const NativePatchType = PatchTable(Address, Operation, mapSize, patchSize);
    const ThreadedPatchType = PatchTable([*]Code, Operation, threadedMapSize, threadedPatchSize);

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
            self.threaded_patch.externalReference(@ptrCast(@constCast(code)));
            while (self.threaded_patch.getPending()) |cp| {
                try self.abstractInterpret(cp);
            }
            self.buffer.makeExecutable();
        }

        fn abstractInterpret(self: *Self, initial_cp: [*]const Code) !void {
            self.resetAbstractState(@intFromPtr(initial_cp + 1));
            self.native_patch.clearMap();
            
            var decoder = Arch.decoder(initial_cp[0].threadedFn);
            self.define(&self.threaded_patch, decoder.getAddress());
            
            nextInstruction: while (true) {
                var inst: Operation = decoder.nextInstruction();
                instSw: switch (inst) {
                    .ret, .raw => break,
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
                    .store, .tst, .branchConditional => {
                        // @TODO: Missing
                    },
                    .branchRegister => |register| {
                        sw: switch (self.reg_type[register]) {
                            .codeAddress => {
                                // this isn't right
                                inst = .{ .addConstant = .{ .source = Arch.pcRegister, .target = Arch.pcRegister, .addend = self.reg_value[Arch.pcRegister] - @intFromPtr(initial_cp) } };
                                Arch.emit(inst, &self.buffer);
                                const target: [*]Code = @ptrFromInt(self.reg_value[register]);
                                inst = .{ .branch = .{ .address = undefined } };
                                _ = self.threaded_patch.reference(target, @ptrCast(self.buffer.getAddress()), inst);
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
                        _ = self.native_patch.reference(@ptrCast(@alignCast(@constCast(branch.address))), self.buffer.getAddress(), inst);
                        continue :instSw .endBranch;
                    },
                    .endBranch => {
                        Arch.emit(inst, &self.buffer);
                        if (self.native_patch.getPending()) |addr| {
                            decoder.goto(addr);
                            self.define(&self.native_patch, decoder.getAddress());
                            continue :nextInstruction;
                        } else break;
                    },
                }

                Arch.emit(inst, &self.buffer);
            }
        }

        fn define(self: *Self, patchTable: anytype, address: anytype) void {
            var iter = patchTable.definition(@ptrCast(@constCast(address)), @ptrCast(self.buffer.getAddress()));
            while (iter.next()) |patch| {
                _ = patch;
            }
        }

        fn resetAbstractState(self: *Self, pc: u64) void {
            self.reg_type = Arch.registerTypes();
            self.reg_value = [_]u64{0} ** Arch.nRegisters; // don't need to, but may be useful for debugging
            self.reg_value[Arch.pcRegister] = pc;
        }
    };
}

const std = @import("std");
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
pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

const PatchTable = @import("patchTable.zig").PatchTable;
const jit_ir = @import("jit_ir.zig");
// const Address = jit_ir.Address;
const Operation = jit_ir.Operation;
const RegisterContents = jit_ir.RegisterContents;

const threadedMapSize = 100;
const threadedPatchSize = 200;
const mapSize = 1000;
const patchSize = 2000;
const maxMethodJitSize = 32768;

test "copyNPatch" {
    // this command (with the magic corrected) can be used to test CopyAndPatch
    // zig test --dep zag -Mroot=cnp.zig --dep options -Mzag=../zag.zig -Moptions=.../.zig-cache/c/.../options.zig
    // to find the right path, use the most recent of:
    // find ../../.zig-cache/c -name options.zig -ls
    const Code = union(enum) {
        object: i64,
        threadedFn: [*]const Operation,
        codePtr: *@This(),
    };

    const JitBuffer = struct {
        buffer: [10]Operation = undefined,
        pos: usize = 0,
        const Self = @This();
        const Address = [*]Operation;
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

    const tf1 = [_]Operation{ .{ .tst = .{ .source = 5, .mask = 7 } }, .ret };
    const m1 = [_]Code{.{ .threadedFn = &tf1 }};
    const Arch = @import("cnp/mockArch.zig").MockArch(JitBuffer.Address);

    var cnp: CopyAndPatch(Code, Arch, JitBuffer) = undefined;
    try cnp.init();
    defer cnp.deinit();

    try cnp.jitCode(&m1);
    // std.debug.print("buffer: {any}\n",.{cnp.buffer.slice()});
}
