pub const RegisterContents = enum {
    pc,
    sp,
    process,
    context,
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

pub const Address = [*]const u8;

pub fn Instruction(AddressType: type, RawType: type) type {
    return struct {
        address: AddressType,
        raw: RawType,
        operation: Operation,
    };
}

pub const Operation = union(enum) {
    raw: u32,
    ret,
    endBranch,
    move: Move,
    tst: Test,
    load: LoadStore,
    loadPostIndex: LoadStore,
    store: LoadStore,
    storePostIndex: LoadStore,
    branch: Branch,
    branchLink: Branch,
    branchRegister: Register,
    branchConditional: BranchConditional,
    conditionalSelect: ConditionalSelect,
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

    pub const Test = struct {
        source: Register,
        mask: u64,
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

    pub const ConditionalSelect = struct {
        target: Register,
        source: Register,
        alternative: Register,
        condition: Condition,
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
