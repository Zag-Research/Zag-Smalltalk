const zag = @import("../zag.zig");
const c = zag.object.ClassIndex;
const compileRaw = zag.execute.compileRaw;
pub const ZERO = compileRaw(.{ c.SmallInteger, 0 });
pub const False = compileRaw(.{c.False});
pub const True = compileRaw(.{c.True});
pub const Nil = compileRaw(.{c.UndefinedObject});
