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
pub fn PatchTable(AddressType: anytype, InfoType: anytype, mapSize: usize, patchSize: usize) type {
    const AddressMap = struct {
        source: ?AddressType,
        target: ?AddressType,
        status: Status,
        const Status = enum { new, defined, referenced };
        fn atOrDefine(map: []AddressMap, source: AddressType) *AddressMap {
            const hashed = (@intFromPtr(source) >> 8) % self.len;
            // linear probe from there to the end
            for (map[hashed..map.len) |*element| {
                if (element.source) |addr| {
                    if (addr == source) return element;
                } else {
                    element.source = source;
                    element.status = .new;
                    return element;
                }
            }
            // rest of linear probe for the first part of the array
            for (map[0..hashed) |*element| {
                if (element.source) |addr| {
                    if (addr == source) return element;
                } else {
                    element.source = source;
                    element.status = .new;
                    return element;
                }
            }
            @panic("AddressMap too small");
        }
    };
    const PatchElement = struct {
        next: ?*PatchElement,
        address: AddressType,
        info: InfoType,
    };
    return struct {
        map: [mapSize]AddressMap,
        patch: [patchSize]PatchElement,
        freePatch: ?*PatchElement,
        const Self = @This();
        pub fn init(self: *Self) {
            var last: ?*PatchElement = null;
            for (&self.patch) |*pe| {
                *pe.next = last;
            }
            self.freePatch = last;
            for (&self.map) |*am| {
                am.source = null;
            }
        }
        pub fn deinit(_: *Self) void {}
        pub fn definition(self: *Self, define: AddressType, as: AddressType) void {
            const am = self.map.atOrDefine(define);
            switch (am.status) {
                .new => {
                    am.target = as;
                },

            }
        }
    };
}
pub fn JitMethod(arch: anytype) type {
    const Buffer = JitBuffer(arch);
    return struct {
        buffer: Buffer,
        regs: arch.RegisterSet,
        nativePatch: PatchTable(Address, Operation, mapSize, patchSize),
        threadedPatch: PatchTable(ThreadedFn, void, mapSize, patchSize),
        const Self = @This();

        pub fn init(self: *Self) !void {
            var jitBuffer = try JitBuffer.init(maxMethodJitSize); // need to adjust accordingly
            self.buffer = jitBuffer;
            self.regs = arch.registerSet();
            self.nativePatch.init();
            self.threadedPatch.init();
        }
        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }
        pub fn copyAndPatch(self: *Self, method: *const zag.execute.CompiledMethod) !void {
            self.buffer.makeWritable();
            var pc = method.initPC();
            self.threadedPatch.externalReference(pc.prim());
            pc = pc.next();
            while (self.threadedPatch.getPending()) |tf| {
                self.abstractInterpret(tf);
            }
            self.buffer.makeExecutable(); // should also release unused pages
        }
        fn abStractInterpret(tf: ThreadedFn) {
            var address = Address.from(tf);
            self.nativePatch.define(address, self.jit.currentAddress());
            while (true) {
                const inst = arch.getInstruction(address);
                switch (inst) {
                    .move => |move| self.regs[move.destination] = self.regs[move.source],
                    .branch => |branch| self.nativePatch.reference(branch.address, self.buffer.address(), inst),
                }
                arch.emit(inst, self.buffer);
            }
        }
    };
}
