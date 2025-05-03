### Zag Tagged Encoding
This is an encoding that tags the low bits of a word to represent various immediate values. It was originally a modification of Spur encoding.

Spur is the encoding used by [OpenSmalltalkVM](https://github.com/OpenSmalltalk) and was designed by Elliot Miranda and Clement Bera. They tagged only a few kinds of immediate values because their philosophy is that most objects should be in memory (i.e. heap) whereas our philosophy is that as many objects as possible should be immediate values. This reduces memory traffic, allows for more efficient dispatch, and encodes a significant set of block closures to not require any memory allocation.

This is evolved from an earlier attempt [[Encoding-Modified-Spur-Old]] and a departure from [[Encoding-NaN]] which was promising (there is still a switch to enable it, but it's not been kept up to date, and there are optimizations that depend on this encoding instead - so it's essentially a dead end).

Spur uses the [following format](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/):
![[Pasted image 20240115082827.png]]
This provides 61-bit SmallInteger and immediate floats that have an exponent range equivalent to 32-bit floats (approximately 1e-77 to 1e77) with the full 64-bit float mantissa. Determining which of the 4 types is a simple bit-test. Decoding the float is a test for the zero case and then a shift, add a 64-bit constant, and a rotate.

We extend this slightly, by using all 8 possible tag values:
- 0: Pointer or `nil`. This is compatible with native 8-byte-aligned pointers, so no conversion is required. This compatibility extends to `nil` being equivalent to `null` in C, C++, Rust, or Zig.
- 1: immediate values for the classes `SmallInteger`, `Character`, `Symbol`, `True`, `False` as well as several forms of immediate `BlockClosure`s that take 0 or 1 parameters. The next 5 bits are the class number, and the top 56 bits are the information (the integer value or the character Unicode value or the symbol hash code). In some cases, the top 48 bits provide a 48-bit address allowing capture of heap objects, including contexts.
- 2-7: `Float`. By using 6 tags we can encode all 64-bit floats less than 2.68e154. Any value larger than that will be heap allocated. For the vast majority of applications this range will allow all values except `+inf`, `-inf`, and `nan` to be coded as immediate values. Because those values may occur, we save the heap allocation by recognizing them and using a reference to a statically allocated value. Decoding doesn't need to handle zero specially, and is simply: subtract 2, and rotate right 4 bits. Encoding is similarly: rotate left 4 bits and add 2; if the result anded with 6 is zero, immediate encoding is not possible, so a reference to a static (`+inf`, `-inf`, or `nan`) or heap-allocated memory object is used. These are both several instructions shorter than Spur and involve no conditional code on decode.

| High 16 bits       |            |            | Tag        | Type                        |
| ------------------ | ---------- | ---------- | ---------- | --------------------------- |
| `0000000000000000` | `00000000` | `00000000` | `00000000` | `nil`                       |
| `0000000000000000` | `aaaaaaaa` | `aaaaaaaa` | `aaaaa000` | (heap) pointer              |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `llllllll` | `00001001` | `ThunkReturnLocal`          |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `iiiiiiii` | `00010001` | `ThunkReturnInstance`       |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `nnnnnnnn` | `00011001` | `ThunkReturnSmallInteger`   |
| ...                | ...        | `00000000` | `00100001` | reserved for `Context`      |
| ...                | ...        | `00000000` | `00101001` | reserved for `BlockClosure` |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `iiiiiiii` | `00110001` | `ThunkReturnImmediate`      |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `cccccccc` | `00111001` | `ThunkReturnCharacter`      |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `seeemmmm` | `01000001` | `ThunkReturnFloat`          |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `llllllll` | `01001001` | `ThunkLocal`                |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `iiiiiiii` | `01010001` | `BlockAssignLocal`          |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `iiiiiiii` | `01011001` | `ThunkInstance`             |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `iiiiiiii` | `01100001` | `BlockAssignInstance`       |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `00000000` | `01101001` | `PICPointer`                |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `00000000` | `01110001` | `ThunkHeap`                 |
| `xxxxxxxxxxxxxxxx` | `xxxxxxxx` | `cccccttt` | `01111001` | `ThunkImmediate`            |
| `eeeeeeeemmmmmmmm` | `mmmmmmmm` | `mmmmseee` | `10000001` | `ThunkFloat`                |
| `xxxxxxxxxxxxxxxx` | `xxxxxxxx` | `xxxxxxxx` | `10001001` | `SmallInteger`              |
| `0000000000000000` | `00000000` | `00000000` | `10010001` | `False`                     |
| `0000000000000000` | `00000000` | `00000000` | `10011001` | `True`                      |
| `0000000000000000` | `hhhhhhhh` | `hhhhhhhh` | `10100001` | `Symbol`                    |
| `0000000000000000` | `uuuuuuuu` | `uuuuuuuu` | `10101001` | `Character`                 |
| `aaaaaaaaaaaaaaaa` | `aaaaaaaa` | `tttttttt` | `10110001` | `LLVM`                      |
| ...                | ...        | ...        | `10111001` | reserved                    |
| ...                | ...        | ...        | -          | reserved                    |
| ...                | ...        | ...        | `11111001` | reserved                    |
| `eeeeeeeemmmmmmmm` | `mmmmmmmm` | `mmmmmmmm` | `mmmms010` | `Float`                     |
| `eeeeeeeemmmmmmmm` | `mmmmmmmm` | `mmmmmmmm` | -          | `Float`                     |
| `eeeeeeeemmmmmmmm` | `mmmmmmmm` | `mmmmmmmm` | `mmmms111` | `Float`                     |

Because we are using all 8 possible values of the tag field, where the test in Spur for "is a `SmallInteger`" was simply **`and 1`** not being 0, using our encoding it requires comparing the low byte with a constant. However testing for a `Float` is simply **`and 6`** not being 0, and a test for a heap object is **`and 7`** being 0.

Getting the class looks at the low 3 bits: 0 it's `nil` or it's a heap object and need to look at the header, 1 it's an immediate and the next 5 bits are the class, 2-7 it's a `Float`.

Testing for a pointer-containing object (for the garbage collector): take the low byte, rotate right 3 and if the result is less than `0b00101110` then it contains a pointer (i.e. it is one of the first 13 tags above or a pointer). (Note there is a false-positive for the two reserved tags, but they are never found where an object could be.)

`basicHash` for any of the non-pointer-containing values  is the high 32 bits `xor` with the low 32 bits, encoded as a `SmallInteger`. The hash value for any pointer value is accessed from the object header, encoded as a `SmallInteger`.
### Immediates

Any selection of zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. They also mostly have very few instances, so aren't likely to be usefully optimized. The currently identified ones that do  are `True`, `False`, `Character`, and `Symbol`, as well as `SmallInteger`. By restricting to only 5 bits for the class, we significantly increase the range of useful values we can encode into immediate values.

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 5 bits and the hash code is 24-56 bits. Some immediate classes use the hash24 value which is the 24 bits after the tag. Others contain a pointer in the top 48 bits with some other data in the other 8 bits.
#### Class numbers
1. `ThunkReturnLocal`: This and the following 4 classes encode thunks that do non-local returns of a value, where the address of the `Context` is in the high 48 bits. This class returns a value from the `Context` (a local, parameter, or the `self` object). The local index is encoded in the extra field.
2. `ThunkReturnInstance`: This encodes a non-local return of an instance variable. The variable index is encoded in the extra field. The `self` field of the `Context` is the referenced object.
3. `ThunkReturnSmallInteger`: This encodes a non-local return of an 8-bit signed integer, encoded in the extra field.
4. reserved for `Context`:  A `HeapHeader` with class `Context` looks like this immediate, which indicates the head of a `Context` object on the stack. This and the next 2 are specially chosen so that an actual object header can appear on the stack and be identifiable (i.e. the header can look like a valid object). When creating a `ContextData` object on the stack, the stack is scanned looking for one of these 2 values, which will delimit the caller's stack.
5. reserved for `BlockClosure`:  A `HeapHeader` with class `BlockClosure` looks like this immediate, which indicates the head of a `BlockClosure` object on the stack.
6. `ThunkReturnImmediate`: This encodes a non-local return of an immediate value (`nil`, `false`, or `true`). Simply returns the extra field, so 0 is `nil`, `01101001` is `true`, etc.
7. `ThunkReturnCharacter`: This encodes a non-local return of an 8-bit character, with the character encoded in the extra field. This doesn't encode all characters, but it encodes all the ASCII characters.
8. `ThunkReturnFloat`: This encodes a non-local return of a limited floating-point value, with the value encoded in the extra field. This only encodes 256 values, but it encodes all the integral values from 0 to 8, all the powers of 2 up to 128 and their inverses, powers of 2 times 10 up to 320, 1/8 to 7/8, `nan` and `inf`. And it has negative and positives for all of them.
9. `ThunkLocal`: This evaluates to the value of a local variable in the `Context` referred to in the high 48 bits. The variable index is encoded in the extra field.
10. `BlockAssignLocal`: This takes 1 parameter and assigns the value to a local variable in the `Context`. That value is also the result. The local index is encoded in the extra field. If the local variable number is 0, no assignment is done, and it simply does a non-local return of the parameter - i.e. this is a continuation.
11. `ThunkInstance`: This evaluates to the value of an instance variable of the object referred to in the high 48 bits. The variable index is encoded in the extra field.
12. `BlockAssignInstance`: This takes 1 parameter and assigns the value to an instance variable of the object referred to in the high 48 bits. That value is also the result. The variable index is encoded in the extra field.
13. `PICPointer`:  This encodes a pointer to a [[Execution#Sends and Polymorphic Inline Caches|Polymorphic Inline Cache]] array. The address of the heap object is in the high 48 bits. The extra field is ignored, although it could encode the size of the array if we decided to make it variable.
14. `ThunkHeap`: This encodes a thunk (a `BlockClosure` that takes no parameters) that evaluates to a heap object. The address of the heap object is in the high 48 bits. The extra field is ignored. This value is also used internally by primitives.
15. `ThunkImmediate`: This encodes  a thunk that evaluates to an immediate value. A sign-extended copy of the top 56 bits is returned. This encodes 48-bit `SmallInteger`s, and all of the other immediate values, as well as `nil`.
16. `ThunkFloat`: This encodes  a thunk that evaluates to a `Float` value. A copy of the top 52 bits, concatenated to 8 zero bits and the next 4 bits. This encodes any floating-point number we can otherwise encode as long as the bottom 8 bits are zero (this include 45-bit integral values as well as values with common fractional parts such as 0.5, 0.25, 0.75). Values that can't be encoded that way would use `ThunkHeap` to return an object.
17. `SmallInteger`: this encodes small integers. In this encoding, the high 56 bits of the word make up the value, so this provides 56-bit integers (-36,028,797,018,963,968 to 36,028,797,018,963,967). This allows numerous optimizations of `SmallInteger` operations (see [[Optimizations]]).
18. `False`: This encodes the singleton value `false`. The `False` and `True` classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation).
19. `True`: This encodes the singleton value `true`.
20. `Symbol`: See [Symbol](Symbol.md) for detailed information on the format.
21. `Character`: The hash code contains the full Unicode value for the character/code-point. This allows orders of magnitude more possible character values than the 294,645 allocated code points as of [Unicode](https://www.unicode.org/versions/stats/)16 and even the 1,112,064 possible Unicode code points.
22. `LLVM`: Interface object to LLVM library. The 8 bit tag differentiates different kinds of LLVM JIT pointers.
23. to 31 unused

The additional classes that are hard-coded (because they are referenced by Zig code) are:
32. `UndefinedObject`: the singleton value `nil` which is represented as all zero bits.
33. `Context`: method context. Note this must be #33 so that it relates to the reserved class #4
34. `Float`: the bit patterns that encode double-precision IEEE floating point.
35. `ProtoObject`: the master superclass. This is also the value returned by `immediate_class` for all heap and thread-local objects. This is an address of an in-memory object.
36. `Object`: the superclass of all normal objects
37. `Array`: the fundamental `Array` class.
38. `String`: ASCII strings (which are mutable)
39. `Utf8String`: immutable [UTF-8](https://datatracker.ietf.org/doc/html/rfc3629) strings; a subclass of `String`.
40. `DoubleWordArray`: array of 64-bit integers.
41. `BlockClosure`: this is reserved for block closures. All the Thunk... and Block... in the first 31 classes are subclasses of this. Note this must be #41 so that it relates to the reserved class #5
42. `Process`: an object that contains all the information of a process, including the stack and nursery heap areas.
43. `Class`:
44. `CompiledMethod`:
45. `Dispatch`:
46. `Association`:
47. `Exception`:
48. `Error`:
49. `ContextData`: 
50. `SelectorException`:
51. `PrimitiveFailed`:
52. `BlockClosureValue`: the BlockClosure that simply evaluates to its field.

### Thunks and Closures
Full block closures are relatively expensive. Even though many will typically be discarded quickly, they take dozens of instructions to create. They are allocated on the stack (because most have LIFO behaviour) which puts pressure on the stack which may force the stack to overflow more quickly and need to be spilled to the heap, and some will put pressure on the heap directly - both causing garbage collections to be more frequent. There are many common blocks that don't actually need access to method local variables, `self` or parameters. These can be encoded as immediate values with special subclasses of BlockClosure and obviate the need for heap allocation. 
1. a `ThunkImmediate` acts as a niladic BlockClosure that evaluates to 56 bits of immediate value, which includes 48-bit integers and all other immediate values as well as `nil`.
2. a `ThunkFloat` similarly supports float values with 48-bits of significance in the mantissa.
3. the non-local thunks (`ThunkReturnLocal`, `ThunkReturnInstance`, `ThunkReturnImmediate`, `ThunkReturnCharacter`, `ThunkReturnFloat`) do a non-local return of one of their values.
4. the object referencing thunks (`ThunkHeap`, `ThunkLocal`, `ThunkInstance`) evaluate to their values.
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
