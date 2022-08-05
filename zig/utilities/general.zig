pub fn inversePhi(comptime T: type) T {
    return switch (T) { // these are all 2^size/phi
        u32 => 2654435769, // was 2717763491
        u16 => 40503,
        u8 => 158,
        else => 11400714819323197440,
    };
}
