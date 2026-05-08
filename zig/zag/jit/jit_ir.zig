pub const RegisterContents = enum {
    pc,
    sp,
    processP,
    contextP,
    extra,
    codeAddress,
    executableAddress,
    unknown,
    unknownPc,
    randInt,
    randFloat,
    object,
    code,
};

pub const Address = packed struct {
    address: *const u8,

    pub fn fromPtr(ptr: anytype) Address {
        return .{ .address = @ptrCast(ptr) };
    }
};

pub const Operation = union(enum) {
    stop,
    endBranch,
    move: Move,
    tst: u64,
    load: LoadStore,
    store: LoadStore,
    branch: Branch,
    branchRegister: Register,
    branchConditional: BranchConditional,
    add: Arithmetic,
    addConstant: ArithmeticConstant,

    pub const Arithmetic = struct {
        target: Register,
        source: Register,
        addend: Register,
    };

    pub const ArithmeticConstant = struct {
        target: Register,
        source: Register,
        addend: u64,
    };

    pub const BranchConditional = struct {
        condition: Condition,
        address: Address,
    };

    pub const Condition = u8;
    pub const Register = u8;

    pub const Branch = struct {
        address: Address,
    };

    pub const LoadStore = struct {
        register: Register,
        base: Register,
        offset: u16,
    };

    pub const Move = struct {
        source: Register,
        destination: Register,
    };
};
