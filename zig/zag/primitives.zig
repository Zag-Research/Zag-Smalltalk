// all primitives defined in Pharo image
// SmallInteger - 1 2 3 4 5 6 7 8 9 10 11 12 13
// SmallInteger and LargePositiveInteger - 14 15 16 17
// Integer @ - 18
// BlockClosure - 19
// LargeInteger - 20 21 22 23 24 25 26 29 30 31 32 33
// Float related - 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
// at: - 60
// at:put: - 61
// size - 62
// at: ^Character - 63
// at:put: Character - 64
// ReadStream next - 65
// WriteStream nextPut: - 66
// ReadStream atEnd - 67
// special - 68 69
// basicNew - 70
// basicNew: - 71
// instVarAt: - 73
// instVarAt:put: - 74
// misc - 75 76 77 78 79 80
// perform: - 83
// perform:withArguments: - 84
// semaphore/process - 85 86 87 88 89
// snapShot - 97
// perform:withArguments:inSuperClass: - 100
// replaceFrom:to:with:startingAt: - 105
// 108
// == 110
// class/species - 111
// SmalltalkImage - 112 113 114 115 116 119 121 124 125 128 129 130 131
// 132 135 136 138 139 140 142 143 144 145 148 149 158 159 165 166 167 168 169 170 171 175 176 177 178 180 188 195 196 197 198 199
// BlockClosure - 206 207 208 209
// Context - 210 211 212
// Cog - 214 215
// timer related - 230 231 232 233 235 240 242
// AST - 243 246 249
// SmallFloat64 - 541 542 543 544 545 546 547 548 549 550 551 552 553 554 555 556 557 558 559
// SmalltalkImage unbindExternalPrimitives - 570
// VirtualMachine - 254 571 572 573
// Native arrays - 600 601 602 603 604 605 606 607 608 609 610 611 612 613 614 615 616 617 618 619 620 621 622 623 624 625 626 627 628 629
// ExternalAddress - 630 631 632 633 634 635 636 637 638 639 640 641 642 643 644 645 646 647 648 649 650 651 652 653 654 655 656 657 658 659
// ASTSmallInteger asCharacter - 2000
// ASTStdIOStream unicodeRead - 2001
// ASTStdIOStream nextPut: - 2002
// ASTStdIOStream nextPutAll: - 2003
// ASTStdIOStream nextPutError: - 2004
// ASTStdIOStream nextPutErrorAll: - 2005
// ASTBehavour basicIdentityHash - 2075
// ASTFloat basicIdentityHash - 2171

pub const inlines = struct {
    pub usingnamespace @import("primitives/Object.zig").inlines;
    pub usingnamespace @import("primitives/Smallinteger.zig").inlines;
    pub usingnamespace @import("primitives/Behavior.zig").inlines;
    pub usingnamespace @import("primitives/BlockClosure.zig").inlines;
    pub usingnamespace @import("primitives/Boolean.zig").inlines;
};
pub const embedded = struct {
    pub const Object = struct {
        pub usingnamespace @import("primitives/Object.zig").embedded;
    };
    pub usingnamespace @import("primitives/Smallinteger.zig").embedded;
    pub const Behavior = struct {
        pub usingnamespace @import("primitives/Behavior.zig").embedded;
    };
    pub const BlockClosure = struct {
        pub usingnamespace @import("primitives/BlockClosure.zig").embedded;
    };
    pub const Boolean = struct {
        pub usingnamespace @import("primitives/Boolean.zig").embedded;
    };
    pub usingnamespace @import("execute.zig").controlPrimitives;
};
pub const primitives = struct {
    pub usingnamespace @import("primitives/Object.zig").primitives;
    pub usingnamespace @import("primitives/Smallinteger.zig").primitives;
    pub usingnamespace @import("primitives/Behavior.zig").primitives;
    pub usingnamespace @import("primitives/BlockClosure.zig").primitives;
    pub usingnamespace @import("primitives/Boolean.zig").primitives;
};
pub fn init() void {
    @import("primitives/Object.zig").init();
    @import("primitives/Smallinteger.zig").init();
    @import("primitives/Behavior.zig").init();
    @import("primitives/BlockClosure.zig").init();
    @import("primitives/Boolean.zig").init();
    //    @import("execute.zig").init();
}

test "primitives" {
    @import("std").debug.print(" - testing primitives ", .{});
    _ = @import("primitives/Smallinteger.zig").inlines;
    _ = @import("primitives/Object.zig").inlines;
    _ = @import("primitives/Behavior.zig").inlines;
    _ = @import("primitives/BlockClosure.zig").inlines;
    _ = @import("primitives/Boolean.zig").inlines;
}
