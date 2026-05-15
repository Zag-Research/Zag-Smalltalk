/// Example Usage:
/// var jit: CopyAndPatch(currentArch) = undefined;
/// try jit.init();
/// defer jit.deinit();
/// const code = try jit.jitMethod(method);
/// method.jitted = @ptrCast(@alignCast(code.ptr));
pub fn CopyAndPatch(Code: anytype, Arch: anytype, JitBuffer: anytype) type {
    const Address = Arch.Address;
    const nativePatchType = PatchTable(Address, Operation, mapSize, patchSize);
    const threadedPatchType = PatchTable([*]Code, Operation, threadedMapSize, threadedPatchSize);
    
    return struct {
        buffer: JitBuffer,
        regType: [Arch.nRegisters]RegisterContents,
        regValue: [Arch.nRegisters]u64,
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
            self.threadedPatch.externalReference(@constCast(@ptrCast(code)));
            while (self.threadedPatch.getPending()) |cp| {
                try self.abstractInterpret(cp);
            }
            self.buffer.makeExecutable();
        }

        fn abstractInterpret(self: *Self, initial_cp: [*]const Code) !void {
            self.resetAbstractState(@intFromPtr(initial_cp + 1));
            var decoder = Arch.decoder(initial_cp[0].threadedFn);
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
                                inst = .{.addConstant = .{.source = Arch.pcRegister, .target = Arch.pcRegister, .addend = self.regValue[Arch.pcRegister] - @intFromPtr(initial_cp)}};
                                Arch.emit(inst, &self.buffer);
                                const target: [*]Code = @ptrFromInt(self.regValue[register]);
                                inst = .{.branch = .{.address = undefined}};
                                _ = self.threadedPatch.reference(target, @ptrCast(self.buffer.getAddress()), inst);
                                continue :sw .executableAddress;
                            },
                            .executableAddress => {
                                // for debugging, should check that the 5 registers have the right types
                                assert(self.regType[Arch.pcRegister] == .pc or self.regType[Arch.pcRegister] == .unknownPc);
                                assert(self.regType[Arch.spRegister] == .sp);
                                assert(self.regType[Arch.processRegister] == .process);
                                assert(self.regType[Arch.contextRegister] == .context);
                                assert(self.regType[Arch.extraRegister] == .extra);
                                continue :instSw .endBranch;
                            },
                            else => @panic("branchRegister to non-executable"),
                        }
                    },
                    .branch => |branch| {
                        _ = self.nativePatch.reference(@constCast(@alignCast(@ptrCast(branch.address))), self.buffer.getAddress(), inst);
                        continue :instSw .endBranch;
                    },
                    .endBranch => {
                        Arch.emit(inst, &self.buffer);
                        if (self.nativePatch.getPending()) |addr| {
                            decoder.goto(addr);
                            self.define(&self.nativePatch,decoder.getAddress());
                            continue :nextInstruction;
                        } else
                            break;
                    },
                }

                Arch.emit(inst, &self.buffer);
            }
        }

        fn define(self: *Self, patchTable: anytype, address: anytype) void {
            var iter = patchTable.definition(@constCast(@ptrCast(address)), @ptrCast(self.buffer.getAddress()));
            while (iter.next()) |patch| {
                _ = patch;
            }
        }

        fn resetAbstractState(self: *Self, pc: u64) void {
            self.regType = Arch.registerTypes();
            self.regValue = [_]u64{0} ** Arch.nRegisters; // don't need to, but may be useful for debugging
            self.regValue[Arch.pcRegister] = pc;
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
    
    const tf1 = [_]Operation{.{.tst = .{ .source = 5, .mask = 7}}, .ret};
    const m1 = [_]Code{.{.threadedFn = &tf1}};
    const Arch = @import("cnp/mockArch.zig").MockArch(JitBuffer.Address);
    
    var cnp: CopyAndPatch(Code, Arch, JitBuffer) = undefined;
    try cnp.init();
    defer cnp.deinit();
    
    try cnp.jitCode(&m1);
    // std.debug.print("buffer: {any}\n",.{cnp.buffer.slice()});
}
