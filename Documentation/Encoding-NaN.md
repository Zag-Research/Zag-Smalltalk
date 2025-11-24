### NaN Encoding
The IEEE 754 64-bit binary number is encoded as follows:
	![IEEE 754 Binary-64](images/Pasted%20image%2020210311212924.png)
When the 11 mantissa bits are all 1s and at least one of the bottom 51 bits is non-zero, then the value is considered Not a Number (NaN), and the low 52 bits are otherwise ignored as a floating point number.^[Bit 51 could also be 1 to make a quiet (non-signaling) NaN, but it doesn't seem necessary.]

So we have 52 bits to play with, as long as at least one bit is non-zero. This lets us encode 2^52 possible values (see the comment at [SpiderMonkey](https://github.com/ricardoquesada/Spidermonkey/blob/4a75ea2543408bd1b2c515aa95901523eeef7858/js/src/gdb/mozilla/jsval.py) now [located here](https://spidermonkey.dev/)). They further point out that on many architectures only the bottom 48 bits are valid as memory addresses, and when used as such, the high 16 bits must be the same as bit 47.

There are several ways to do NaN tagging/encoding. You can choose integers, pointers, or doubles to be naturally encoded and all the others be encoded with some shifting/adding. While integers and pointers are probably more common in most Smalltalk images, leaving doubles as naturally encoded means that vector instructions and/or GPUs could act directly on memory.

So this leaves us with the following encoding based on the **S**ign+**E**xponent and **F**raction bits:

| S+E             | F      | F      | F      | Type                      |
| --------------- | ------ | ------ | ------ | ------------------------- |
| `0000`          | `0000` | `0000` | `0000` | double  +0                |
| `0000`-`7FEF`   | `xxxx` | `xxxx` | `xxxx` | double (positive)         |
| `7FF0`          | `0000` | `0000` | `0000` | +inf                      |
| `7FF0`          | `aaaa` | `aaaa` | `aaaa` | `ThunkReturnLocal`        |
| `7FF1`          | `aaaa` | `aaaa` | `aaaa` | `ThunkReturnInstance`     |
| `7FF2`          | `aaaa` | `aaaa` | `aaaa` | `ThunkReturnSmallInteger` |
| `7FF3`          | `aaaa` | `aaaa` | `aaaa` | `ThunkReturnImmediate`    |
| `7FF4`          | `aaaa` | `aaaa` | `aaaa` | `ThunkLocal`              |
| `7FF5`          | `aaaa` | `aaaa` | `aaaa` | `BlockAssignLocal`        |
| `7FF6`          | `aaaa` | `aaaa` | `aaaa` | `ThunkInstance`           |
| `7FF7`          | `aaaa` | `aaaa` | `aaaa` | `BlockAssignInstance`     |
| `7FF8`          | `aaaa` | `aaaa` | `aaaa` | `ThunkHeap`               |
| `7FF9`          | `xxxx` | `xxxx` | `xxxx` | `ThunkImmediate`          |
| `7FFA`          | `0000` | `hhhh` | `hhhh` | `Symbol`                  |
| `7FFB`          | `0000` | `0000` | `0000` | `False`                   |
| `7FFC`          | `0000` | `0000` | `0001` | `True`                    |
| `7FFD`          | `0000` | `00uu` | `uuuu` | `Character`               |
| `7FFE`          | `cccc` | `hhhh` | `hhhh` | `Signature`               |
| `7FFF`          | `aaaa` | `aaaa` | `aaaa` | heap object               |
| `8000`          | `0000` | `0000` | `0000` | double     -0             |
| `8000`-`FFEF`   | `xxxx` | `xxxx` | `xxxx` | double (negative)         |
| `FFF0`          | `0000` | `0000` | `0000` | -inf                      |
| `FFF0` - `FFF7` | `xxxx` | `xxxx` | `xxxx` | NaN (unused)              |
| `FFF8` + `ii`   | `iiii` | `iiii` | `iiii` | `SmallInteger` values     |

So, interpreted as a u64, any value that is less than or equal to -inf is a double. Else, the top 4 bits of the mantissa are a class grouping. For group B, the next 16 bits are a class number so the first 8 classes have (and all classes can have) a compressed representation. 
Groups 0 through A have the low 48 bits being the address of an object.

#### Class numbers
1. `ThunkReturnLocal`: *There is no encoding for this class in NaN encoding*.
2. `ThunkReturnInstance`: *There is no encoding for this class in NaN encoding - a limited version. is provided by `ThunkReturnImmediate`*.
3.  `ThunkReturnSmallInteger`: *There is no encoding for this class in NaN encoding - a limited version. is provided by `ThunkReturnImmediate`.*
4. `ThunkReturnImmediate`: non-local return of one of 8 constant values. The low 48 bits (with the low 3 bits forced to zero) are the address of the Context. The only possible values (encoded in the low 3 bits) are: `[^self]`, `[^true]`, `[^false]`, `[^nil]`, `[^-1]`, `[^0]`, `[^1]`, `[^ firstInstanceVariable]`.
5. `ThunkReturnCharacter`: *There is no encoding for this class in NaN encoding.*
6. `ThunkReturnFloat`: *There is no encoding for this class in NaN encoding.*
7. `ThunkLocal`: *There is no encoding for this class in NaN encoding.*
8. `BlockAssignLocal`: *There is no encoding for this class in NaN encoding.*
9. `ThunkInstance`: *There is no encoding for this class in NaN encoding.*
10. `BlockAssignInstance`: *There is no encoding for this class in NaN encoding.*
11. `PICPointer`: *There is no encoding for this class in NaN encoding.*
12. `ThunkHeap`: This encodes a thunk that evaluates to a heap object.
13. `ThunkImmediate`: This encodes  a thunk that evaluates to an immediate value. NaN encoding encodes the type of the immediate in the top bit. If the top bit is set, this encodes 47-bit `SmallInteger`s. Otherwise it encodes all of the other immediate values.
14. `ThunkFloat`: *There is no encoding for this class in NaN encoding.*
15. `LLVM`: *There is no encoding for this class in NaN encoding.*
16.  `SmallInteger` - this is reserved for the bit patterns that encode small integers. This isn't encoded in the tag. The low 50 bits of the "hash code" make up the value, so this provides 50-bit integers (-562,949,953,421,312 to 562,949,953,421,311).
17. `Symbol`: See [Symbol](Symbol.md) for detailed information on the format.
18. `False`: This encodes the singleton value `false`. The `False` and `True` classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation).
19. `True`: This encodes the singleton value `true`.
20. `Character`: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.
21. to 31 unused

### Thunks and Closures
Full block closures are relatively expensive. Even though many will typically be discarded quickly, they take dozens of instructions to create. They are allocated on the stack (because most have LIFO behaviour) which puts pressure on the stack which may force the stack to overflow more quickly and need to be spilled to the heap, and some will put pressure on the heap directly - both causing garbage collections to be more frequent. There are many common blocks that don't actually need access to method local variables, `self` or parameters. These can be encoded as immediate values with special subclasses of BlockClosure and obviate the need for heap allocation. 
1. `ThunkImmediate` acts as a niladic BlockClosure that evaluates to a limited range of numeric values, encoded in the hash bits. Hence this supports 32/45-bit SmallIntegers.
2. `ThunkFloat` similarly supports 32/45-bit float values.
3. an immediate thunk acts as a niladic BlockClosure that evaluates to any immediate. For modified Spur format, this only supports  the first 8K classes and 32-bit hash values, but this includes all the common values. Examples: `[#foo]`, `[true]`, `[nil]`, `[$x]`.
4. a heap thunk is similar, but it evaluates to a heap object.
5. a non-local thunk simply does a non-local return of one of 8 constant values. The low 48 bits (with the low 3 bits forced to zero) are the address of the Context. The only possible values (encoded in the low 3 bits) are: `[^self]`, `[^true]`, `[^false]`, `[^nil]`, `[^-1]`, `[^0]`, `[^1]`, `[^2]`.
6. all remaining closures are full block closures and are memory objects, They are allocated on the stack (because most will disappear when their containing method returns) but they may need to be moved to a heap. They contain the following fields in order (omitting any unused fields):
	1. the address of the CompiledMethod object that contains various values, and the threaded code implementation (if this is the only field the block has no closure or other variable fields, so the block can be statically allocated - otherwise it needs to be stack allocated (which could be moved to a heap);
	2. the address of the Context if there are any non-local returns (if a closure that references a Context is forced to the heap, that will force that Context to be promoted to the heap, which will force the Context that that refers to, etc. - essentially dumping the whole stack to the heap);
	3. the address of the value holding block if there were multiple blocks in a method and mutable values needed by this block were allocated in another block (there could conceivably be multiples if there are blocks within blocks);
	4. the values of `self`, parameters, or locals that are only used in the block after being initialized in the method, as well as (if this is a local-holding block) any mutable locals used by this or other blocks.

When a `[`*some-value*`]` closure is required and *some-value* is a literal, self, or a parameter to the method (i.e. something that can't be assigned to), runtime code returns either a `ThunkImmediate`/`ThunkFloat` (if the value is immediate or numeric and fits), a `ThunkHeap` when the value is a heap object, or if the value doesn't fit any of these constraints it will fall back to a full closure with 2 fields: the "return one field" CompiledMethod reference and the value. This applies to `self` or any other runtime value.

There are pre-defined CompiledMethods for some common closures:
1. value:  `[some-value]` - use when value isn't covered by numeric, immediate or heap thunks. CompiledMethod reference and the value are the only things in the closure 
2. id: `[:x|x]` - 
3. return id: `[:x| ^ x]`
4. return value: `[^ value]` - when the value is outside the non-local thunk group. The value is the only thing in the closure other than the method address and the `Context` pointer.

More information on closures can be found at [[Execution#BlockClosures]].
