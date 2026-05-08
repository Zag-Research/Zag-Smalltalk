/// Example Usage:
/// var jit: CopyAndPatch(currentArch) = undefined;
/// try jit.init();
/// defer jit.deinit();
/// const code = try jit.jitMethod(method);
/// method.jitted = @ptrCast(@alignCast(code.ptr));
pub fn CopyAndPatch(arch: anytype) type {
    return struct {
        buffer: JitBuffer,
        regType: [arch.nRegisters]RegisterContents,
        regValue: [arch.nRegisters]u64,
        nativePatch: PatchTable(Address, Operation, mapSize, patchSize),
        threadedPatch: PatchTable(PC, void, threadedMapSize, 0),
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
            self.buffer.makeWritable();
            self.threadedPatch.externalReference(method.initPC());
            while (self.threadedPatch.getPending()) |pc| {
                try self.abstractInterpret(pc);
            }
            self.buffer.makeExecutable();
        }

        fn abstractInterpret(self: *Self, initial_pc: PC) !void {
            self.resetAbstractState(initial_pc.next().asCodePtr());
            var address = Address.fromPtr(initial_pc.prim());
            const iter = self.nativePatch.definition(address, self.buffer.currentOffset());
            while (iter.next) |patch| {
                // patch the buffer
                _ = patch;
            }
            nextInstruction: while (true) {
                var inst = arch.getInstruction(address);
                instSw: switch (inst) {
                    .stop => break,
                    .move => |move| {
                        self.regType[move.destination] = self.regType[move.source];
                        self.regValue[move.destination] = self.regValue[move.source];
                    },
                    .load => |ldst| {
                        switch (self.regType[ldst.base]) {
                            .pc => {
                                self.regType[ldst.register] = .codeAddress;
                                self.regValue[ldst.register] = self.regValue[ldst.base] + ldst.offset;
                                address = arch.skip(inst, address);
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
                                address = arch.skip(inst, address);
                                continue :nextInstruction;
                            },
                            else => {},
                        }
                    },
                    .add => |arith| {
                        switch (self.regType[arith.source]) {
                            self.regType[arith.addend] => {
                                self.regType[arith.target] = self.regType[arith.source];
                                self.regValue[arith.target] = self.regValue[arith.source] + self.regValue[arith.addend];
                                address = arch.skip(inst, address);
                                continue :nextInstruction;
                            },
                            .unknown => {},
                            else => {},
                        }
                    },
                    .store, .tst, .branchConditional => {
                        // @TODO: Missing
                    },
                    .branchRegister => |register| {
                        sw: switch (self.regType[register]) {
                            .codeAddress => {
                                // this isn't right
                                const target = PC.init(@ptrFromInt(self.regValue[register]));
                                self.threadedPatch.externalReference(target);
                                inst = .{.branch = .{.address = undefined}};
                                self.nativePatch.globalReference(self.regValue[register.register], self.buffer.address(), inst);
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
                            address = addr;
                            self.nativePatch.definition(address, self.jit.currentAddress());
                            continue :nextInstruction;
                        } else
                            return;
                    },
                }

                arch.emit(inst, &self.buffer);
                address = arch.skip(inst, address);
            }
        }

        fn resetAbstractState(self: *Self, pc: u64) void {
            self.regType = arch.registerTypes();
            self.regValue = [_]u64{0} ** arch.nRegisters; // don't need to, but may be useful for debugging
            self.regValue[arch.pcRegister] = @intFromPtr(pc);
        }
    };
}

const zag = @import("zag");

const assert = @import("std").debug.assert;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;
const CompiledMethod = zag.execute.CompiledMethod;

const JitBuffer = @import("jit_buffer.zig").JitBuffer;
const PatchTable = @import("patchTable.zig").PatchTable;
const jit_ir = @import("jit_ir.zig");
const Address = jit_ir.Address;
const Operation = jit_ir.Operation;
const RegisterContents = jit_ir.RegisterContents;

const threadedMapSize = 200;
const mapSize = 1000;
const patchSize = 2000;
const maxMethodJitSize = 32768;

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

test "copyNPatch" {
    _ = CopyAndPatch(@import("aarch64.zig"));
}
