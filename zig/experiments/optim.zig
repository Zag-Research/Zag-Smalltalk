pub const ClassIndex = enum(u16) {
    none = 0,
    Object,
    SmallInteger,
    UndefinedObject,
    False,
    True,
    Float,
    Symbol,
    Character,
    BlockClosure,
    _,
};
pub const Object = packed struct {
    u: u64,
};
    export fn immediate_class(self: Object) ClassIndex {
        if (self.u&4!=0) return .Float;
        if (self.u&2!=0) {
            if (self.u&1!=0) return .Float;
            return @enumFromInt((self.u>>4)&0xffff);
        }
        if (self.u&1!=0) return .SmallInteger;
        return .Object;
    }
    export fn immediate_class2(self: Object) ClassIndex {
        switch (self.u&7) {
            0 => return .Object,
            1 => return .SmallInteger,
            2 => return @enumFromInt((self.u>>4)&0xffff),
            else => return .Float,
        }
    }
pub const Group = enum(u16) {
    immediates = 0xfff0,
    smallIntMin,
    smallIntNeg_2,
    smallIntNeg_3,
    smallIntNeg_4,
    smallInt0,
    smallIntPos_6,
    smallIntPos_7,
    smallIntMax,
    numericThunk,
    immediateThunk,
    heapThunk,
    nonLocalThunk,
    nonLocalClosure,
    heapClosure,
    heap,
    _,
};
pub const ZObject = packed struct(u64) {
    h0: u16, // align(8),
    h1: u16,
    classIndex: ClassIndex,
    tag: Group,
    inline fn which_class(self: ZObject, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .numericThunk, .immediateThunk, .heapThunk, .nonLocalThunk, .nonLocalClosure, .heapClosure => .BlockClosure,
            .immediates => self.classIndex,
            .heap => if (full) .Character else .Object,
            .smallIntMin, .smallIntNeg_2, .smallIntNeg_3, .smallIntNeg_4, .smallInt0, .smallIntPos_6, .smallIntPos_7, .smallIntMax => .SmallInteger,
            else => .Float
        };
    }
};
    export fn immediate_classZ(self: ZObject) ClassIndex {
        return self.which_class(false);
    }
