const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;
const Code = zag.execute.Code;
const CompiledMethod = zag.execute.CompiledMethod;
const JitBuffer = @import("jit_buffer.zig").JitBuffer;

const mapSize = 1000;
const maxMethodJitSize = 32000;

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

const RegisterContents = enum{
    pc, sp, processP, contextP, extra,
    codeAddress,
    unknown,
    randInt, randFloat,
    object, code,
};
const Operations = union(enum) {
    move: Move,
    tst: u64,
    load: LoadStore,
    store: LoadStore,
    branch: Branch,
    branchRegister: Register,
    branchConditional: BranchConditional,
    add: Arithmetic,
    addConstant: ArithmeticConstant,
    const Arithmetic  = struct {
        target: Register,
        source: Register,
        addend: Register,
    };
    const ArithmeticConstant  = struct {
        target: Register,
        source: Register,
        addend: u64,
    };
    const BranchConditional = struct {
        condition: Condition,
        address: Address,
    };
    const Condition = u8;
    const Register = u8;
    const Branch = struct {
        address: u64,
    };
    const LoadStore = struct {
        register: Register,
        base: Register,
        offset: u16,
    };
    const Move = struct {
        source: Register,
        destination: Register,
    };
};
const Address = struct {
    address: *u8,
};
// use as:
// var jit = CopyAndPatch(currentArch);
// jit.init();
// defer jit.deinit();
// jit.jitMethod(method);
// method.jitted = jit.asThreadedFn();
//
pub fn CopyAndPatch(arch: anytype) type {
    const Buffer = JitBuffer(arch);
    return struct {
        buffer: Buffer,
        regs: arch.RegisterSet,
        regValue: [arch.nRegisters]u64,
        nativePatch: PatchTable(Address, Operation, mapSize, patchSize),
        threadedPatch: PatchTable(PC, void, mapSize, patchSize),
        const Self = @This();

        pub fn init(self: *Self) !void {
            var jitBuffer = try JitBuffer.init(maxMethodJitSize); // need to adjust accordingly
            self.buffer = jitBuffer;
            self.regType = arch.registerSet();
            self.nativePatch.init();
            self.threadedPatch.init();
        }
        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }
        pub fn jitMethod(self: *Self, method: *const zag.execute.CompiledMethod) !void {
            self.buffer.makeWritable();
            self.threadedPatch.externalReference(method.initPC());
            while (self.threadedPatch.getPending()) |pc| {
                self.abstractInterpret(pc);
            }
            self.buffer.makeExecutable(); // should also release unused pages
        }
        fn abStractInterpret(self: *Self, initialPc: PC) {
            var address = Address.from(initialPc.prim());
            // check for a send and replace with sendNative
            self.regValue[0] = initialPc.next();
            self.nativePatch.define(address, self.jit.currentAddress());
            while (true) {
                const inst = arch.getInstruction(address);
                switch (inst) {
                    .move => |move| self.regs[move.destination] = self.regs[move.source],
                    .branch => |branch| self.nativePatch.reference(branch.address, self.buffer.address(), inst),
                    .load => |ldst| {
                        switch (ldst.base) {
                            .pc => {
                                self.regType[ldst.register] = .codeAddress;
                                self.regValue[ldst.register] = self.regValue[ldst.base] + offset;
                                address = arch.skip(inst, address);
                                continue;
                            },
                            else => {},
                        }
                    },
                    .branchRegister => |register| {
                        const inst: Operation = .{.branch = .{.address = undefined}};
                        self.threadedPatch.reference(self.regValue[register.register], self.buffer.address(), {});
                        arch.emit(inst, self.buffer);
                    },
                    .addConstant => |arith| {
                        switch (self.regType[arith.source]) {
                            .pc => {
                                self.regType[arith.target] = .pc;
                                self.regValue[arith.target] = self.regValue[arith.source] + arith.constant;
                                continue;
                            }
                        }
                    },
                }
                arch.emit(inst, self.buffer);
                address = arch.skip(inst, address);
            }
        }
    };
}
