const zag = @import("../zag.zig");
const c = zag.object.ClassIndex;
const compileRaw = zag.execute.compileRaw;
const si = c.SmallInteger;
pub const ZERO = compileRaw(.{ si, 0 });
pub const False = compileRaw(.{c.False});
pub const True = compileRaw(.{c.True});
pub const Nil = compileRaw(.{c.UndefinedObject});
pub const SmallIntegerCache = compileRaw(.{
    si, -5,
    si, -4,
    si, -3,
    si, -2,
    si, -1,
    si, 0,
    si, 1,
    si, 2,
    si, 3,
    si, 4,
    si, 5,
    si, 6,
    si, 7,
    si, 8,
    si, 9,
    si, 10,
    si, 11,
    si, 12,
    si, 13,
    si, 14,
    si, 15,
    si, 16,
    si, 17,
    si, 18,
    si, 19,
    si, 20,
    si, 21,
    si, 22,
    si, 23,
    si, 24,
    si, 25,
    si, 26,
    si, 27,
    si, 28,
    si, 29,
    si, 30,
    si, 31,
    si, 32,
    si, 33,
    si, 34,
    si, 35,
    si, 36,
    si, 37,
    si, 38,
    si, 39,
});
const SICacheMin = -5;
pub const SICacheMax = 39;
