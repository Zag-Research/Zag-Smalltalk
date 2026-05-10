/// Example Usage:
/// var jit: CopyAndPatch(currentArch) = undefined;
/// try jit.init();
/// defer jit.deinit();
/// const code = try jit.jitMethod(method);
/// method.jitted = @ptrCast(@alignCast(code.ptr));
pub fn CopyAndPatch(Code: anytype, arch: anytype, JitBuffer: anytype, Address: anytype) type {
    const nativePatchType = PatchTable(Address, Operation, mapSize, patchSize);
    const threadedPatchType = PatchTable([*]const Code, Operation, threadedMapSize, threadedPatchSize);
    return struct {
        buffer: JitBuffer,
        regType: [arch.nRegisters]RegisterContents,
        regValue: [arch.nRegisters]u64,
        nativePatch: nativePatchType,
        threadedPatch: threadedPatchType,
        const Self = @This();

        pub fn init(self: *Self) !void {
            self.buffer = try JitBuffer.init(maxMethodJitSize);
            self.nativePatch.init();
            self.threadedPatch.init();
        }

        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }

        pub fn jitMethod(self: *Self, method: *const CompiledMethod) !void {
            return self.jitCode(method.codeSlice());
        }

        fn jitCode(self: *Self, code: []const Code) !void {
            self.buffer.makeWritable();
            self.threadedPatch.externalReference(@ptrCast(code));
            while (self.threadedPatch.getPending()) |cp| {
                try self.abstractInterpret(cp);
            }
            self.buffer.makeExecutable();
        }

        fn abstractInterpret(self: *Self, initial_cp: [*]const Code) !void {
            self.resetAbstractState(@intFromPtr(initial_cp + 1));
            var decoder = arch.decoder(initial_cp[0].threadedFn);
            self.define(&self.threadedPatch,decoder.getAddress());
            self.nativePatch.clearMap();
            nextInstruction: while (true) {
                var inst: Operation = decoder.nextInstruction();
                instSw: switch (inst) {
                    .ret, .raw => break,
                    .move => |move| {
                        self.regType[move.destination] = self.regType[move.source];
                        self.regValue[move.destination] = self.regValue[move.source];
                    },
                    .load => |ldst| {
                        switch (self.regType[ldst.base]) {
                            .pc => {
                                self.regType[ldst.register] = .codeAddress;
                                self.regValue[ldst.register] = self.regValue[ldst.base] + ldst.offset;
                                continue;
                            },
                            .context => {
                                switch (ldst.offset) {
                                    @offsetOf(Context,"prevCtxt") => self.regType[ldst.register] = .context,
                                    @offsetOf(Context,"tpc") => self.regType[ldst.register] = .unknownPc,
                                    @offsetOf(Context,"npc") => self.regType[ldst.register] = .executableAddress,
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    },
                    .addConstant => |arith| {
                        switch (self.regType[arith.source]) {
                            .pc, .codeAddress => {
                                self.regType[arith.target] = self.regType[arith.source];
                                self.regValue[arith.target] = self.regValue[arith.source] + arith.addend;
                                continue :nextInstruction;
                            },
                            else => {},
                        }
                    },
                    .add => |arith| {
                        if (self.regType[arith.source] == self.regType[arith.addend]) {
                            self.regType[arith.target] = self.regType[arith.source];
                            self.regValue[arith.target] = self.regValue[arith.source] + self.regValue[arith.addend];
                            continue :nextInstruction;
                        }
                    },
                    .store, .tst, .branchConditional => {
                        // @TODO: Missing
                    },
                    .branchRegister => |register| {
                        sw: switch (self.regType[register]) {
                            .codeAddress => {
                                // this isn't right
                                inst = .{.addConstant = .{.source = arch.pcRegister, .target = arch.pcRegister, .addend = self.regValue[arch.pcRegister] - @intFromPtr(initial_cp)}};
                                arch.emit(inst, self.buffer);
                                const target: [*]Code = @ptrFromInt(self.regValue[register]);
                                inst = .{.branch = .{.address = undefined}};
                                self.threadedPatch.reference(target, self.buffer.getAddress(), inst);
                                continue :sw .executableAddress;
                            },
                            .executableAddress => {
                                // for debugging, should check that the 5 registers have the right types
                                assert(self.regType[arch.pcRegister] == .pc or self.regType[arch.pcRegister] == .unknownPc);
                                assert(self.regType[arch.spRegister] == .sp);
                                assert(self.regType[arch.processRegister] == .process);
                                assert(self.regType[arch.contextRegister] == .context);
                                assert(self.regType[arch.extraRegister] == .extra);
                                continue :instSw .endBranch;
                            },
                            else => @panic("branchRegister to non-executable"),
                        }
                    },
                    .branch => |branch| {
                        self.nativePatch.reference(branch.address, self.buffer.currentOffset(), inst);
                        continue :instSw .endBranch;
                    },
                    .endBranch => {
                        arch.emit(inst, self.buffer);
                        if (self.nativePatch.getPending()) |addr| {
                            decoder.goto(addr);
                            self.define(&self.nativePatch,decoder.getAddress());
                            continue :nextInstruction;
                        } else
                            break;
                    },
                }

                arch.emit(inst, &self.buffer);
            }
        }

        fn define(self: *Self, patchTable: anytype, address: anytype) void {
            var iter = patchTable.definition(@ptrCast(address), self.buffer.getAddress());
            while (iter.next()) |patch| {
                _ = patch;
            }
        }

        fn resetAbstractState(self: *Self, pc: u64) void {
            self.regType = arch.registerTypes();
            self.regValue = [_]u64{0} ** arch.nRegisters; // don't need to, but may be useful for debugging
            self.regValue[arch.pcRegister] = pc;
        }
    };
}

const zag = @import("zag");

const debug = @import("std").debug;
const assert = debug.assert;
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
    const Code = union(enum) {
        object: i64,
        threadedFn: [*]const Operation,
        codePtr: *@This(),
    };
    const JitBuffer = struct {
        buffer: [10]Operation = undefined,
        pos: usize = 0,
        const Self = @This();
        fn init(_: usize) !Self {
            return .{};
        }
        pub fn deinit(_: *Self) void {}
        pub fn makeWritable(_: *Self) void {}
        pub fn makeExecutable(_: *Self) void {}
        pub fn append(self: *Self, values: []Operation) void {
            for (values) |op| {
                self.buffer[self.pos] = op;
                self.pos = self.pos + 1;
            }
        }
        pub fn getAddress(self: *Self) [*]Operation {
            return @as([*]Operation, @ptrCast(&self.buffer)) + self.pos;
        }
        pub fn slice(self: *Self) []Operation {
            return self.buffer[0..self.pos];
        }
    };
    const tf1 = [_]Operation{.ret};
    const m1 = [_]Code{.{.threadedFn = &tf1}};
    var cnp: CopyAndPatch(Code, @import("cnp/mockArch.zig"), JitBuffer, [*]Operation) = undefined;
    try cnp.init();
    defer cnp.deinit();
    try cnp.jitCode(&m1);
}
