const std = @import("std");
const builtin = @import("builtin");
const zag = @import("zag");
const assert = std.debug.assert;
const config = zag.config;
const InMemory = zag.InMemory;
const object = zag.object;
const Object = object.Object;
const execute = zag.execute;
const Context = zag.context;
const Process = zag.process;
const heap = zag.heap;
const globalArena = zag.globalArena;
const symbolX = zag.symbol;
const utilities = zag.utilities;
const threadedFn = zag.threadedFn;
const llvm = zag.llvm;
const crc = std.hash.Crc32;

const references = execute.embedded.references;
fn version() void {
    std.debug.print("Zag Smalltalk {s} using {} object encoding\n", .{ config.git_version, config.objectEncoding });
}

var addresses: [500]usize = undefined;
var n_addresses: usize = 0;
fn lessThanInt(context: void, a: usize, b: usize) bool {
    _ = context;
    return a < b;
}
fn compareInt(key: usize, item: usize) std.math.Order {
    return std.math.order(key, item);
}
fn setup_addresses() void {
    for (0..500) |tf|
        switch (@as(threadedFn.Enum, @enumFromInt(tf))) {
            ._end => break,
            else => |tag| {
                const addr = threadedFn.threadedFn(tag);
                addresses[n_addresses] = @intFromPtr(addr);
                n_addresses += 1;
            },
        };
    std.mem.sort(usize, addresses[0..n_addresses], {}, lessThanInt);
}
fn sizeof(addr: usize) usize {
    if (std.sort.binarySearch(
        usize,
        addresses[0..n_addresses],
        addr,
        compareInt,
    )) |result| return if (result < n_addresses - 1) addresses[result + 1] - addresses[result] else 400;
    return 0;
}

pub fn getFuncBoundsWindows(func_ptr: *const anyopaque) !struct { start: usize, size: usize } {
    const process = c.GetCurrentProcess();

    // Initialize symbol handler for current process
    _ = c.SymInitialize(process, null, 1);

    // Allocate memory for SYMBOL_INFO + name buffer
    var buffer: [@sizeOf(c.SYMBOL_INFO) + 256]u8 align(@alignOf(c.SYMBOL_INFO)) = undefined;
    const symbol: *c.SYMBOL_INFO = @ptrCast(&buffer);
    symbol.SizeOfStruct = @sizeOf(c.SYMBOL_INFO);
    symbol.MaxNameLen = 255;

    var displacement: u64 = 0;
    const addr = @intFromPtr(func_ptr);

    if (c.SymFromAddr(process, addr, &displacement, symbol) != 0) {
        return .{
            .start = symbol.Address,
            .size = symbol.Size,
        };
    }

    return error.SymbolNotFound;
}

pub fn getFuncBoundsLinux(func_ptr: *const anyopaque) !struct { start: usize, size: usize } {
    const target_addr = @intFromPtr(func_ptr);

    var file = try std.fs.cwd().openFile("/proc/self/exe", .{});
    defer file.close();

    var header = try std.elf.Header.read(file);
    var shdr_it = header.sectionHeaderIterator(file);

    var symtab_shdr: ?std.elf.Elf64_Shdr = null;

    while (try shdr_it.next()) |shdr| {
        if (shdr.sh_type == std.elf.SHT_SYMTAB) {
            symtab_shdr = shdr;
            break;
        }
    }

    const symtab = symtab_shdr orelse return error.SymbolTableNotFound;
    const num_syms = symtab.sh_size / @sizeOf(std.elf.Elf64_Sym);

    var sym: std.elf.Elf64_Sym = undefined;
    var i: usize = 0;

    // To account for PIE/ASLR, calculate base slide or check relative offsets
    while (i < num_syms) : (i += 1) {
        _ = try file.seekTo(symtab.sh_offset + i * @sizeOf(std.elf.Elf64_Sym));
        _ = try file.readAll(std.mem.asBytes(&sym));

        // STT_FUNC check
        if ((sym.st_info & 0x0f) == std.elf.STT_FUNC and sym.st_size > 0) {
            // Check if target_addr matches symbol address (or symbol + slide)
            if (sym.st_value == target_addr) {
                return .{
                    .start = sym.st_value,
                    .size = sym.st_size,
                };
            }
        }
    }

    return error.SymbolNotFound;
}

pub fn getFuncBoundsMacOS(func_ptr: *const anyopaque) !struct { start: usize, size: usize } {
    const target_addr = @intFromPtr(func_ptr);

    // Get ASLR slide for main image (index 0)
    const slide = c._dyld_get_image_vmaddr_slide(0);
    const mh = c._dyld_get_image_header(0);
    if (mh == null) return error.ImageNotFound;

    // Traverse load commands to find LC_SYMTAB
    var header_ptr: [*]const u8 = @ptrCast(mh);
    header_ptr += @sizeOf(c.mach_header_64);

    var symtab_cmd: ?*const c.symtab_command = null;
    var i: u32 = 0;

    while (i < mh.*.ncmds) : (i += 1) {
        const cmd: *const c.load_command = @ptrCast(@alignCast(header_ptr));
        if (cmd.cmd == c.LC_SYMTAB) {
            symtab_cmd = @ptrCast(@alignCast(cmd));
            break;
        }
        header_ptr += cmd.cmdsize;
    }

    const st = symtab_cmd orelse return error.SymtabNotFound;
    const syms: [*]const c.nlist_64 = @ptrFromInt(@intFromPtr(mh) + st.symoff);

    var target_sym_addr: ?usize = null;
    var next_closest_addr: usize = std.math.maxInt(usize);

    // Scan nlist table to find exact symbol and the next closest function symbol in memory
    var idx: u32 = 0;
    while (idx < st.nsyms) : (idx += 1) {
        const sym = syms[idx];
        if (sym.n_value == 0) continue;

        const sym_addr = sym.n_value + @as(u64, @intCast(slide));
        if (sym_addr == target_addr) {
            target_sym_addr = sym_addr;
        } else if (sym_addr > target_addr and sym_addr < next_closest_addr) {
            next_closest_addr = sym_addr;
        }
    }

    if (target_sym_addr) |start| {
        if (next_closest_addr != std.math.maxInt(usize)) {
            return .{
                .start = start,
                .size = next_closest_addr - start,
            };
        }
    }

    return error.SymbolNotFound;
}

pub fn getFunctionBounds(func_ptr: anytype) !struct { start: usize, size: usize } {
    const ptr: *const anyopaque = @ptrCast(func_ptr);

    return switch (builtin.os.tag) {
        .windows => try getFuncBoundsWindows(ptr),
        .linux => try getFuncBoundsLinux(ptr),
        .macos => try getFuncBoundsMacOS(ptr),
        else => @compileError("Unsupported OS for runtime symbol inspection"),
    };
}
pub fn getFunctionSizeFromSymbol(func_ptr: anytype) !?usize {
    if (getFunctionBounds(func_ptr)) |result| {
        return result.size;
    } else |_| return null;
}
pub fn getFunctionSizeFromSymbol_notUsed(func_ptr: anytype) !?usize {
    const target_addr = @intFromPtr(func_ptr);

    var self_debug_info = try std.debug.getSelfDebugInfo(std.heap.page_allocator);
    defer self_debug_info.deinit();

    // Query symbol information for the function address
    if (try self_debug_info.getModuleForAddress(target_addr)) |module| {
        if (try module.getSymbolAtAddress(std.heap.page_allocator, target_addr)) |symbol| {
            // Calculate size if start and end line/address bounds are present
            // Or inspect ELF symbol st_size directly via module
            _ = symbol;
        }
    }
    return null;
}

const c = @cImport({
    @cInclude("capstone/capstone.h");
    //@cInclude("capstone/arm64.h");   // For Capstone v4
    @cInclude("capstone/aarch64.h"); // For Capstone v5/v6
    switch (builtin.os.tag) {
        .windows => {
            @cInclude("windows.h");
            @cInclude("dbghelp.h");
        },
        .macos => {
            @cInclude("mach-o/dyld.h");
            @cInclude("mach-o/nlist.h");
        },
        else => {},
    }
});
const PrintFlags = packed struct {
    address: bool = true,
    bytes: bool = true,
    mnemonic: bool = true,
    operands: bool = true,
};
const noPrint = PrintFlags{ .address = false, .bytes = false, .mnemonic = false, .operands = false };
pub fn printInstruction(print: PrintFlags, insn: *const c.cs_insn) void {
    if (print == noPrint) return;

    // Format output: 0xADDR: [BYTES] MNEMONIC OPERANDS
    if (print.address) std.debug.print("0x{x:0>12}:", .{insn.address});
    if (print.bytes) std.debug.print(" {x}", .{insn.bytes[0..insn.size]});
    if (print.mnemonic) std.debug.print("  {s}", .{std.mem.sliceTo(&insn.mnemonic, 0)});
    if (print.operands) {
        // Convert null-terminated C char arrays to Zig string slices
        const op_str = std.mem.sliceTo(&insn.op_str, 0);
        if (op_str.len > 0) std.debug.print("\t{s}", .{op_str});
    }
    std.debug.print("\n", .{});
}
pub fn getFunctionSize(print: PrintFlags, allocator: std.mem.Allocator, func_ptr: anytype) !usize {
    const arch = builtin.cpu.arch;

    // 1. Initialize Capstone for x86_64 or AArch64
    var handle: c.csh = 0;
    const err = switch (arch) {
        .x86_64 => c.cs_open(c.CS_ARCH_X86, c.CS_MODE_64, &handle),
        .aarch64 => c.cs_open(c.CS_ARCH_AARCH64, c.CS_MODE_ARM, &handle),
        else => return error.UnsupportedArchitecture,
    };
    if (err != c.CS_ERR_OK) return error.CapstoneInitFailed;
    defer _ = c.cs_close(&handle);

    // Enable detailed instruction parsing
    _ = c.cs_option(handle, c.CS_OPT_DETAIL, c.CS_OPT_ON);

    const start_addr = @intFromPtr(func_ptr);

    // --- Zig 0.15 ArrayList Changes ---
    var worklist: std.ArrayList(u64) = .empty;
    defer worklist.deinit(allocator);

    var visited_blocks = std.AutoHashMap(u64, void).init(allocator);
    defer visited_blocks.deinit();

    try worklist.append(allocator, start_addr);
    var max_addr: u64 = start_addr;

    const insn = c.cs_malloc(handle);
    defer c.cs_free(insn, 1);

    // 2. Traversal Loop
    while (worklist.pop()) |block_start| {
        if (visited_blocks.contains(block_start)) continue;
        try visited_blocks.put(block_start, {});

        var curr_addr: u64 = block_start;

        while (true) {
            var code_ptr: [*c]const u8 = @ptrFromInt(curr_addr);
            var code_size: usize = 16;

            if (!c.cs_disasm_iter(handle, &code_ptr, &code_size, &curr_addr, insn)) {
                break;
            }
            printInstruction(print, insn);
            const insn_end = insn.*.address + insn.*.size;
            if (insn_end > max_addr) max_addr = insn_end;

            const info = inspectInstruction(insn, arch);

            if (info.is_return) break;

            if (info.is_unconditional_jump) {
                if (info.target_address) |target| {
                    if (isInternalTarget(start_addr, target)) {
                        try insertSorted(allocator, &worklist, target);
                    }
                }
                break; // External tail call or indirect jump
            }

            if (info.is_conditional_jump) {
                if (info.target_address) |target| {
                    if (isInternalTarget(start_addr, target)) {
                        try insertSorted(allocator, &worklist, target);
                    }
                }
                continue; // Fallthrough linearly
            }
        }
    }

    return max_addr - start_addr;
}
fn compareU64(key: u64, item: u64) std.math.Order {
    return std.math.order(key, item);
}

pub fn insertSorted(allocator: std.mem.Allocator, list: *std.ArrayList(u64), value: u64) !void {
    // Finds the index of the first element >= value in O(log N)
    const index = std.sort.lowerBound(u64, list.items, value, compareU64);
    try list.insert(allocator, index, value); // O(N) array shift
}

const BranchInfo = struct {
    is_return: bool = false,
    is_unconditional_jump: bool = false,
    is_conditional_jump: bool = false,
    target_address: ?u64 = null,
};

fn inspectInstruction(insn: *c.cs_insn, arch: std.Target.Cpu.Arch) BranchInfo {
    var info = BranchInfo{};

    switch (arch) {
        .x86_64 => {
            if (insn.*.id == c.X86_INS_RET) {
                info.is_return = true;
            } else if (insn.*.id == c.X86_INS_JMP) {
                info.is_unconditional_jump = true;
                info.target_address = getX86ImmTarget(insn, 0);
            } else if (isX86ConditionalJump(insn.*.id)) {
                info.is_conditional_jump = true;
                info.target_address = getX86ImmTarget(insn, 0);
            }
        },
        .aarch64 => {
            if (insn.*.id == c.AARCH64_INS_RET) {
                info.is_return = true;
            } else if (insn.*.id == c.AARCH64_INS_BR) {
                info.is_unconditional_jump = true;
            } else if (insn.*.id == c.AARCH64_INS_B) {
                if (insn.*.detail != null) {
                    const arm = insn.*.detail.*.unnamed_0.aarch64;
                    if (arm.cc == c.AArch64CC_Invalid or arm.cc == c.AArch64CC_AL) {
                        info.is_unconditional_jump = true;
                    } else {
                        info.is_conditional_jump = true;
                    }
                    info.target_address = getArm64ImmTarget(insn, 0);
                }
            } else if (insn.*.id == c.AARCH64_INS_CBZ or insn.*.id == c.AARCH64_INS_CBNZ) {
                info.is_conditional_jump = true;
                info.target_address = getArm64ImmTarget(insn, 1);
            } else if (insn.*.id == c.AARCH64_INS_TBZ or insn.*.id == c.AARCH64_INS_TBNZ) {
                info.is_conditional_jump = true;
                info.target_address = getArm64ImmTarget(insn, 2);
            }
        },
        else => {},
    }

    return info;
}

fn isX86ConditionalJump(id: c_uint) bool {
    return switch (id) {
        c.X86_INS_JE, c.X86_INS_JNE, c.X86_INS_JG, c.X86_INS_JGE, c.X86_INS_JL, c.X86_INS_JLE, c.X86_INS_JA, c.X86_INS_JAE, c.X86_INS_JB, c.X86_INS_JBE, c.X86_INS_JS, c.X86_INS_JNS => true,
        else => false,
    };
}

fn getX86ImmTarget(insn: *c.cs_insn, op_idx: usize) ?u64 {
    if (insn.*.detail != null and insn.*.detail.*.unnamed_0.x86.op_count > op_idx) {
        const op = insn.*.detail.*.unnamed_0.x86.operands[op_idx];
        if (op.type == c.X86_OP_IMM) return @bitCast(op.unnamed_0.imm);
    }
    return null;
}

fn getArm64ImmTarget(insn: *c.cs_insn, op_idx: usize) ?u64 {
    if (insn.*.detail != null and insn.*.detail.*.unnamed_0.aarch64.op_count > op_idx) {
        const op = insn.*.detail.*.unnamed_0.aarch64.operands[op_idx];
        if (op.type == c.AARCH64_OP_IMM) return @bitCast(op.unnamed_0.imm);
    }
    return null;
}

fn isInternalTarget(start_addr: u64, target: u64) bool {
    if (target == @intFromPtr(&zag.dispatch.fail)) return false;
    if (target == @intFromPtr(&std.debug.defaultPanic)) return false;
    return (target >= start_addr and target < start_addr + 0x1000);
    // or (target < start_addr and start_addr - target < 0x1000);
}
fn smalltalkThreadedFns(dump: bool) void {
    std.debug.print("zagThreadesFns\n", .{});
    for ( //[_]i32{12}
        0..500
        //
    ) |tf|
        switch (@as(threadedFn.Enum, @enumFromInt(tf))) {
            ._end => break,
            else => |tag| {
                const addr = threadedFn.threadedFn(tag);
                if (dump) {
                    std.debug.print("{s} ({d:>3}):\n", .{ @tagName(tag), tf });
                    _ = getFunctionSize(PrintFlags{ // .bytes = false, .address = false
                    }, std.heap.page_allocator, addr) catch 0;
                } else {
                    const size = getFunctionSize(noPrint, std.heap.page_allocator, addr) catch 0;
                    std.debug.print("{d:>3}: @{x:0>12} ({d:>4}) {s}\n", .{ tf, @intFromPtr(addr), size, @tagName(tag) });
                }
            },
        };
}
pub fn main() !void {
    version();
    setup_addresses();
    smalltalkThreadedFns(false);
}
