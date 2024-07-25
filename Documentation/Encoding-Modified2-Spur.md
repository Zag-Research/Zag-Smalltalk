### Modified Spur Encoding
Spur is the encoding used by [OpenSmalltalkVM](https://github.com/OpenSmalltalk).

Spur uses the [following format](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/):
![[Pasted image 20240115082827.png]]
This provides 61-bit SmallInteger and immediate floats that have an exponent range equivalent to 32-bit floats (approximately 1e-77 to 1e77) with the full 64-bit float mantissa. Determining which of the 4 types is a simple bit-test. Decoding the float is a test for the zero case and then a shift, add a 64-bit constant, and a rotate.

We extend this slightly, by using all 8 possible tag values:
0: Pointer or `nil`. This is compatible with native 8-byte-aligned pointers, no no conversion is required.
1: immediate values for the classes `SmallInteger`, `Character`, `Symbol`, `True`, `False` as well as . The next 5 bits are the class number, and the top 56 bits are the information (the integer value or the character Unicode value or the symbol hash code). In a few cases, the top 48 bits provide a 48-bit address allowing capture of heap objects, including contexts.
2-7: Float. By using 6 tags we can encode all 64-bit floats less than 2e154. Any value larger than that will be heap allocated. For the vast majority of applications this range will allow all values except `+inf`, `-inf`, and `nan` to be coded as immediate values. Because those values may occur, we save the heap allocation by recognizing them and using a reference to a statically allocated value. Decoding doesn't need to handle zero specially, and is simply: subtract 2, and rotate right 4 bits. Encoding is similarly: rotate left 4 bits; if the resulting low 3 bits are less than 6, add 2; otherwise immediate encoding is not possible.

| Rest     |          |     |     |     |          |          | Tag      | Type                 |
| -------- | -------- | --- | --- | --- | -------- | -------- | -------- | -------------------- |
| 0        | 0        | 0   | 0   | 0   | 0        | 0        | 0        | `nil`                |
| aaaaaaaa | ...      | ... | ... | ... | aaaaaaaa | aaaaaaaa | aaaaa000 | pointer              |
| aaaaaaaa | ...      | ... | ... | ... | aaaaaaaa | 00000000 | 00001001 | ThunkHeap            |
| aaaaaaaa | ...      | ... | ... | ... | aaaaaaaa | eeeeeeee | -        | -                    |
| aaaaaaaa | ...      | ... | ... | ... | aaaaaaaa | cccccccc | 00101001 | ThunkReturnCharacter |
| xxxxxxxx | ...      | ... | ... | ... | xxxxxxxx | xxxxxxxx | 00110001 | SmallInteger         |
| xxxxxxxx | ...      | ... | ... | ... | xxxxxxxx | xxxxxxxx | ccccc001 | intermediates        |
| eeeeeeee | nnnnnnnn | ... | ... | ... | ...      | nnnnnnnn | nnnns010 | double               |
| eeeeeeee | nnnnnnnn | ... | ... | ... | ...      | nnnnnnnn | -        | double               |
| eeeeeeee | nnnnnnnn | ... | ... | ... | ...      | nnnnnnnn | nnnns111 | double               |

Because we are using all 8 possible values of the tag field, where the test in Spur for "is a `SmallInteger`" was simply an `and`, using our encoding it requires an `and` followed by a `cmp`. However testing for a `Float` is simply an `and` with 6.

Testing for a pointer-containing object (for the garbage collector) takes the low byte, rotates right 3 and if the result is less than 0b00100110 then it contains a pointer.

Getting the class looks at the low 3 bits: 0 it's `nil` or a heap object and need to look at the header, 1 it's an immediate and the next 5 bits are the class, 3-7 it's a `Float`.
### Immediates

All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. They also mostly have very few instances, so aren't likely to be usefully optimized. The currently identified ones that do  are `nil`, `true`, `false`, Characters, and Symbols. By restricting to only 5 bits for the class, we significantly increase the range of useful values we can encode into immediate values.

| high 32 bits |          |          |          | Tag      | Class           |
| ------------ | -------- | -------- | -------- | -------- | --------------- |
| 00000000     | 00000000 | 00000000 | 00000000 | 00111001 | UndefinedObject |
| 00000000     | 00000000 | 00000000 | 00000000 | 01000001 | False           |
| 00000000     | 00000000 | 00000000 | 00000000 | 01001001 | True            |
| 00000000     | dddddddd | dddddddd | dddddddd | 01010001 | Symbol          |
| 00000000     | dddddddd | dddddddd | dddddddd | 01011001 | Character       |

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 5 bits and the hash code is 24-56 bits. Most immediate classes use the hash24 value which is the 24 bits after the tag. A few contain a pointer in the top 48 bits with some other data in the other 8 bits. The encodings for `True`, and `False` are extremely wasteful of space (because there is only one instance of each, so the hash code is irrelevant), but the efficiency of dispatch and code generation depend on them being immediate values and having separate classes.

#### Class numbers
1. `ThunkHeap`: This encodes a thunk that evaluates to a heap object. The address of the heap object is in the high 48 bits.
2. `ThunkReturnSelf`: This and the following 3 classes encode thunks that do non-local returns of a value, where the address of the `Context` is in the high 48 bits. This class simply returns the `self` object.
3. `ThunkReturnISmallInteger`: This encodes a non-local return of a 8-bit integer, encoded in the extra field.
4. `ThunkReturnImmediate`: This encode a non-local return of an immediate value (`nil`, `false`, or `true`) with the class of the value (and 3 bits of hash) encoded in the extra field.
5. `ThunkReturnCharacter`: This encodes a non-local return of an 8-bit character, with the character encoded in the extra field.
6. `SmallInteger`: this encodes small integers. In this encoding, the high 56 bits of the word make up the value, so this provides 56-bit integers (-36,028,797,018,963,968 to 36,028,797,018,963,967). This allows numerous optimizations of `SmallInteger` operations (see [[Optimizations]]).
7. `UndefinedObject`: This is reserved for the singleton value `nil` which is represented as all zero bits.
8. `False`: The `False` and `True` classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation). This encodes the singleton value `false`.
9. `True`: This encodes the singleton value `true`
10. `Symbol`: See [Symbols](Symbols.md) for detailed information on the format.
11. `Character`: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.
12. `Float`: this is reserved  for the bit patterns that encode double-precision IEEE floating point.
13. `Object`: this is reserved for the master superclass. This is also the value returned by `immediate_class` for all heap and thread-local objects. This is an address of an in-memory object.
14. `ThunkImmediate`: This encodes  a thunk that evaluates to an immediate value. A sign-extended copy of the top 56 bits is returned. This encodes 48-bit `SmallInteger`s, and all of the other immediate values.
15. `ThunkFloat`: This encodes  a thunk that evaluates to a `Float` value. A copy of the top 52 bits, concatenated to 8 zero bits and the next 4 bits. This encodes any floating-point number we can otherwise encode as long as the bottom 8 bits are zero. Values that can't be encoded that way would use `ThunkHeap` to return an object.

### Thunks and Closures
Full block closures are relatively expensive because most need to be heap allocated. Even though they will typically be discarded quickly, they take dozens of instructions to create, and put pressure on the heap - causing garbage collections to be more frequent. There are many common blocks that don't actually need access to method local variables, `self` or parameters. These can be encoded as immediate values with special subclasses of BlockClosure and obviate the need for heap allocation. 
1. a SmallInteger thunk acts as a niladic BlockClosure that returns a limited range of numeric values, encoded in the hash bits. Hence this supports 32/45-bit SmallIntegers.
2. a Float thunk similarly supports 32/45-bit float values.
3. an immediate thunk acts as a niladic BlockClosure that returns any immediate. For modified Spur format, this only supports  the first 8K classes and 32-bit hash values, but this includes all the common values. Examples: `[#foo]`, `[true]`, `[nil]`, `[$x]`.
4. a heap thunk is similar, but it returns a heap object.
5. a non-local thunk simply does a non-local return of one of 8 constant values. The low 48 bits (with the low 3 bits forced to zero) are the address of the Context. The only possible values (encoded in the low 3 bits) are: `[^self]`, `[^true]`, `[^false]`, `[^nil]`, `[^-1]`, `[^0]`, `[^1]`, `[^2]`.
6. all remaining closures are full block closures and are memory objects (they may have been moved to a heap or still reside on the stack), and contain the following fields in order (omitting any unused fields):
	1. the address of the CompiledMethod object that contains various values, and the threaded code implementation (if this is the only field the block has no closure or other variable fields, so the block can be statically allocated - otherwise it needs to be stack allocated (which could be moved to a heap);
	2. the address of the Context if there are any non-local returns (if a closure that references a Context is forced to the heap, that will force that Context to be promoted to the heap);
	3. the address of the value holding block if there were multiple blocks in a method and mutable values needed by this block were allocated in another block (there could conceivably be multiples if there are blocks within blocks);
	4. the values of `self` and any parameters or read-only locals, that are referenced just in this block (this also includes locals that are used solely in the block after being initialized in the method), as well as (if this is a local-holding block) any mutable locals used by this or other blocks.

When a `[`some-value`]` closure is required and some-value is a literal, self, or a parameter to the method (i.e. something that can't be assigned to), runtime code returns either a numeric or immediate thunk (if the value is numeric/immediate and fits), a heap thunk when the value is a heap object, with the low 48 bits referencing the object, or, if the value doesn't fit any of these constraints, then it will fall back to a full closure with 2 fields: the CompiledMethod reference and the value. This applies to `self` or any other runtime value.
