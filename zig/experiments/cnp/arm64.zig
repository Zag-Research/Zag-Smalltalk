const std = @import("std");

/// ARM64 instruction encoding/decoding helpers for JIT compilation.
pub const Arm64 = struct {
    /// Check if instruction is BR/BLR Xn (indirect branch / tail call).
    pub fn isBranchRegister(inst: u32) bool {
        const masked = inst & 0xFFFFFC1F;
        return masked == 0xD61F0000 or masked == 0xD63F0000;
    }

    pub fn isAdr(inst: u32) bool {
        return (inst & 0x9F000000) == 0x10000000;
    }

    pub fn isAdrp(inst: u32) bool {
        return (inst & 0x9F000000) == 0x90000000;
    }

    pub fn isLdrLiteral64(inst: u32) bool {
        return (inst & 0x3B000000) == 0x58000000;
    }

    pub fn isBImm(inst: u32) bool {
        return (inst & 0xFC000000) == 0x14000000;
    }

    pub fn isBlImm(inst: u32) bool {
        return (inst & 0xFC000000) == 0x94000000;
    }

    fn signExtend(value: i64, bits: u8) i64 {
        const shift: u6 = @intCast(64 - bits);
        return (value << shift) >> shift;
    }

    pub fn decodeAdrImm(inst: u32) i64 {
        const immlo = (inst >> 29) & 0x3;
        const immhi = (inst >> 5) & 0x7FFFF;
        var imm: i64 = @intCast((immhi << 2) | immlo);
        imm = signExtend(imm, 21);
        return imm;
    }

    pub fn encodeAdrImm(inst: u32, imm: i64) u32 {
        const mask: u32 = (0x3 << 29) | (0x7FFFF << 5);
        const immlo: u32 = @intCast(imm & 0x3);
        const immhi: u32 = @intCast((imm >> 2) & 0x7FFFF);
        return (inst & ~mask) | (immlo << 29) | (immhi << 5);
    }

    pub fn decodeLdrLiteralImm(inst: u32) i64 {
        const imm19 = (inst >> 5) & 0x7FFFF;
        var imm: i64 = @intCast(imm19);
        imm = signExtend(imm, 19) << 2;
        return imm;
    }

    pub fn encodeLdrLiteralImm(inst: u32, imm: i64) u32 {
        const imm19: u32 = @intCast((imm >> 2) & 0x7FFFF);
        const mask: u32 = 0x7FFFF << 5;
        return (inst & ~mask) | (imm19 << 5);
    }

    pub fn decodeBImm(inst: u32) i64 {
        const imm26 = inst & 0x03FFFFFF;
        var imm: i64 = @intCast(imm26);
        imm = signExtend(imm, 26) << 2;
        return imm;
    }

    pub fn encodeBImm(inst: u32, imm: i64) u32 {
        const imm26: u32 = @intCast((imm >> 2) & 0x03FFFFFF);
        return (inst & 0xFC000000) | imm26;
    }

    pub fn encodeBranch(offset: i32) u32 {
        const imm26: u32 = @as(u32, @bitCast(@divTrunc(offset, 4))) & 0x03FFFFFF;
        return 0x14000000 | imm26;
    }

    pub fn isRet(inst: u32) bool {
        return inst == 0xD65F03C0;
    }
};
