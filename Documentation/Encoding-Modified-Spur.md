### Modified Spur Encoding
Spur is the encoding used by [OpenSmalltalkVM](https://github.com/OpenSmalltalk).

This is evolved from an earlier attempt [[Encoding-Modified-Spur-Old]]

Spur uses the [following format](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/):
![[Pasted image 20240115082827.png]]
This provides 61-bit SmallInteger and immediate floats that have an exponent range equivalent to 32-bit floats (approximately 1e-77 to 1e77) with the full 64-bit float mantissa. Determining which of the 4 types is a simple bit-test. Decoding the float is a test for the zero case and then a shift, add a 64-bit constant, and a rotate.

We extend this slightly, by using all 8 possible tag values:
0: Pointer or `nil`. This is compatible with native 8-byte-aligned pointers, so no conversion is required.
1: immediate values for the classes `SmallInteger`, `Character`, `Symbol`, `True`, `False` as well as several forms of immediate `BlockClosure`s that take 0 or 1 parameters. The next 5 bits are the class number, and the top 56 bits are the information (the integer value or the character Unicode value or the symbol hash code). In some cases, the top 48 bits provide a 48-bit address allowing capture of heap objects, including contexts.
2-7: `Float`. By using 6 tags we can encode all 64-bit floats less than 2.68e154. Any value larger than that will be heap allocated. For the vast majority of applications this range will allow all values except `+inf`, `-inf`, and `nan` to be coded as immediate values. Because those values may occur, we save the heap allocation by recognizing them and using a reference to a statically allocated value. Decoding doesn't need to handle zero specially, and is simply: subtract 2, and rotate right 4 bits. Encoding is similarly: rotate left 4 bits and add 2; if the result anded with 6 is zero, immediate encoding is not possible, so a reference to a static (`+inf`, `-inf`, or `nan`) or heap-allocated memory object is used.

| High 8 bits | Next 24 bits |            |            |            | Tag        | Type               |
| ----------- | ------------ | ---------- | ---------- | ---------- | ---------- | ------------------ |
| `00000000`  | ...          | `00000000` | `00000000` | `00000000` | `00000000` | `nil`              |
| `00000000`  | ...          | ...        | `aaaaaaaa` | `aaaaaaaa` | `aaaaa000` | pointer            |
| `aaaaaaaa`  | ...          | ...        | `aaaaaaaa` | `00000000` | `00001001` | `ThunkHeap`        |
| `aaaaaaaa`  | ...          | ...        | `aaaaaaaa` | `eeeeeeee` | -          | other thunks       |
| `aaaaaaaa`  | ...          | ...        | `aaaaaaaa` | `iiiiiiii` | `01001001` | `ThunkGetInstance` |
| `00000000`  | ...          | `00000000` | `00000000` | `00000000` | `01010001` | `false`            |
| `00000000`  | ...          | `00000000` | `00000000` | `00000000` | `01011001` | `true`             |
| `xxxxxxxx`  | ...          | ...        | `xxxxxxxx` | `xxxxxxxx` | `01100001` | `SmallInteger`     |
| `00000000`  | ...          | hhhhhhhh   | `hhhhhhhh` | `hhhhhhhh` | `01101001` | `Symbol`           |
| `00000000`  | ...          | hhhhhhhh   | `hhhhhhhh` | `hhhhhhhh` | `01110001` | `Character`        |
| char7 or 0  | ...          | ...        | char2      | char1      | `01111001` | `ShortString`      |
| `xxxxxxxx`  | ...          | ...        | `xxxxxxxx` | `ccccc001` | `10000001` | `ThunkImmediate`   |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | `mmmmmmmm` | `mmmms010` | `10001001` | `ThunkFloat`       |
| `xxxxxxxx`  | ...          | ...        | ...        | ...        | `10010001` | reserved           |
| `xxxxxxxx`  | ...          | ...        | ...        | ...        | -          | reserved           |
| `xxxxxxxx`  | ...          | ...        | ...        | ...        | `11111001` | reserved           |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | ...        | `mmmmmmmm` | `mmmms010` | `Float`            |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | ...        | `mmmmmmmm` | -          | `Float`            |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | ...        | `mmmmmmmm` | `mmmms111` | `Float`            |

Because we are using all 8 possible values of the tag field, where the test in Spur for "is a `SmallInteger`" was simply an `and`, using our encoding it requires comparing the low byte with a constant. However testing for a `Float` is simply an `and` with 6.

Getting the class looks at the low 3 bits: 0 it's `nil` or it's a heap object and need to look at the header, 1 it's an immediate and the next 5 bits are the class, 2-7 it's a `Float`.

Testing for a pointer-containing object (for the garbage collector): if the value is not `nil`, take the low byte, rotate right 3 and if the result is less than `0b00101010` then it contains a pointer (i.e. it is one of the first 11 tags above). Any of the other values can have a hash value calculated by anding out the top 2 bits and the bottom 8 bits, and then adding in `0b01100001` to make a `SmallInteger`, or shifting left 10, then right 2, and then adding in `0b01100001`. 
### Immediates

Any selection of zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. They also mostly have very few instances, so aren't likely to be usefully optimized. The currently identified ones that do  are `True`, `False`, `Character`, and `Symbol`, as well as `SmallInteger`. By restricting to only 5 bits for the class, we significantly increase the range of useful values we can encode into immediate values.

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 5 bits and the hash code is 24-56 bits. Some immediate classes use the hash24 value which is the 24 bits after the tag. Others contain a pointer in the top 48 bits with some other data in the other 8 bits.
#### Class numbers
1. `ThunkHeap`: This encodes a thunk (a `BlockClosure` that takes no parameters) that evaluates to a heap object. The address of the heap object is in the high 48 bits. The extra field is ignored.
2. `ThunkReturnLocal`: This and the following 3 classes encode thunks that do non-local returns of a value, where the address of the `Context` is in the high 48 bits. This class returns a value from the `Context` (a local, parameter, or the `self` object). The local index is encoded in the extra field.
3. `ThunkReturnInstance`: This encodes a non-local return of an instance variable. The variable index is encoded in the extra field. The `self` field of the `Context` is the referenced object.
4. `ThunkReturnSmallInteger`: This encodes a non-local return of a 8-bit signed integer, encoded in the extra field.
5. `ThunkReturnImmediate`: This encode a non-local return of an immediate value (`nil`, `false`, or `true`). Simply returns the extra field, so 0 is `nil`, `01011001` is `true`, etc.
6. `ThunkReturnCharacter`: This encodes a non-local return of an 8-bit character, with the character encoded in the extra field. This doesn't encode all characters, but it encodes all the ASCII characters.
7. `BlockAssignLocal`: This takes 1 parameter and assigns the value to a local in the `Context`. That value is also the result. The local index is encoded in the extra field.
8. `BlockAssignInstance`: This takes 1 parameter and assigns the value to an instance variable of the object referred to in the high 48 bits. That value is also the result. The variable index is encoded in the extra field.
9. `ThunkGetInstance`: This evaluates to the value of an instance variable of the object referred to in the high 48 bits. The variable index is encoded in the extra field.
10. `False`: This encodes the singleton value `false`. The `False` and `True` classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation).
11. `True`: This encodes the singleton value `true`.
12. `SmallInteger`: this encodes small integers. In this encoding, the high 56 bits of the word make up the value, so this provides 56-bit integers (-36,028,797,018,963,968 to 36,028,797,018,963,967). This allows numerous optimizations of `SmallInteger` operations (see [[Optimizations]]).
13. `Symbol`: See [Symbols](Symbols.md) for detailed information on the format.
14. `Character`: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.
15. `ShortString`: immutable ASCII string of 0-7 characters; null terminated.
16. `ThunkImmediate`: This encodes  a thunk that evaluates to an immediate value. A sign-extended copy of the top 56 bits is returned. This encodes 48-bit `SmallInteger`s, and all of the other immediate values.
17. `ThunkFloat`: This encodes  a thunk that evaluates to a `Float` value. A copy of the top 52 bits, concatenated to 8 zero bits and the next 4 bits. This encodes any floating-point number we can otherwise encode as long as the bottom 8 bits are zero (this include any reasonable integral value as well as common fractional values such as 0.5, 0.25). Values that can't be encoded that way would use `ThunkHeap` to return an object.
18. to 23 unused
23. `UndefinedObject`: This is reserved for the singleton value `nil` which is represented as all zero bits. 
24. `Float`: this is reserved  for the bit patterns that encode double-precision IEEE floating point.
25. `Object`: this is reserved for the master superclass. This is also the value returned by `immediate_class` for all heap and thread-local objects. This is an address of an in-memory object.
26. `BlockClosure`: this is reserved for block closures. All the Thunk... are subclasses of this.
27. `BlockClosuerValue`: this is reserved for the BlockClosure that simply evaluates to its field.
28. `Context`: this is reserved for method contexts.
29. `Array`: this is reserved for the fundamental `Array` class.
30. `String`: this is reserved for ASCII strings
31. `Utf8String`: this is reserved for utf-8 strings

### Thunks and Closures
Full block closures are relatively expensive. Even though many will typically be discarded quickly, they take dozens of instructions to create. They are allocated on the stack (because most have LIFO behaviour) which puts pressure on the stack which may force the stack to overflow more quickly and need to be spilled to the heap, and some will put pressure on the heap directly - both causing garbage collections to be more frequent. There are many common blocks that don't actually need access to method local variables, `self` or parameters. These can be encoded as immediate values with special subclasses of BlockClosure and obviate the need for heap allocation. 
1. a `ThunkImmediate` acts as a niladic BlockClosure that evaluates to 56 bits of immediate value, which includes 48-bit integers and all other immediate values.
2. a `ThunkFloat` similarly supports float values with 48-bits of significance in the mantissa.
3. the non-local thunks (`ThunkReturnLocal`, `ThunkReturnInstance`, `ThunkReturnImmediate`, `ThunkReturnCharacter`) do a non-local return of one of their values.
4. the object referencing thunks (`ThunkHeap`, `ThunkGetInstance`) evaluate to their values.
5. the object modifying `BlockClosure`s (`BlockAssignLocal`, `BlockAssignInstance`) take a parameter and assign to a variable and evaluate to that parameter value.
6. all remaining closures are full block closures and are memory objects, They are allocated on the stack (because most will disappear when their containing method returns) but they may need to be moved to a heap. They contain the following fields in order (omitting any unused fields):
	1. the address of the CompiledMethod object that contains various values, and the threaded code implementation (if this is the only field the block has no closure or other variable fields, so the block can be statically allocated - otherwise it needs to be stack allocated (which could be moved to a heap);
	2. the address of the Context if there are any non-local returns (if a closure that references a Context is forced to the heap, that will force that Context to be promoted to the heap, which will force the Context that that refers to, etc. - essentially spilling the whole stack to the heap);
	3. the address of the value holding blocks if there were multiple blocks in a method and mutable values needed by this block were allocated in another block (there could conceivably be multiples if there are blocks within blocks);
	4. the values of `self`, parameters, or locals that are only used in the block after being initialized in the method, as well as (if this is a local-holding block) any mutable locals used by this or other blocks.

When a `[`*some-value*`]` closure is required and *some-value* is a literal, self, or a parameter to the method (i.e. something that can't be assigned to), runtime code returns either a `ThunkImmediate`/`ThunkFloat` (if the value is immediate or numeric and fits), a `ThunkHeap` when the value is a heap object, and object referencing thunk for a local or instance variable, or if the value doesn't fit any of these constraints it will fall back to a full closure with 2 fields: the "return one field" CompiledMethod reference and the value. This applies to `self` or any other runtime value.

There are pre-defined CompiledMethods for some common closures:
1. value:  `[some-value]` - use when value isn't covered by numeric, immediate or heap thunks. CompiledMethod reference and the value are the only things in the closure 
2. id: `[:x|x]` - 
3. return id: `[:x| ^ x]`
4. return value: `[^ value]` - when the value is outside the non-local thunk group. The value is the only thing in the closure other than the method address and the `Context` pointer.

More information on closures can be found at [[Execution#BlockClosures]].