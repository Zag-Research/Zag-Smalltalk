### Mix Tagged Encoding
This is an encoding that tags the low bits of a word to represent various immediate values. It was originally a modification of [[Encoding-Zag]] which was itself inspired by [[Encoding-Spur]]. Zag encoding is a very good encoding, but it uses 6 of the possible 8 encodings of the low 3 bits to encode floating point values, which slows down the recognition of integer values (also it only supports 56 bit SmallIntegers). When reading the [[Float-Self-Tagging.pdf]] paper, which has a table that documents the most used ranges of floating point numbers, I noticed that the four patterns of the top 4 bits of the exponent (0000, 0111, 1000, and 1111) could be recognized as 2 values (000 and 111) if the top bit is ignored. This means that we are left with 6 other potential encodings. 

This continues our philosophy that as many objects as possible should be immediate values. This reduces memory traffic, allows for more efficient dispatch, and encodes a significant set of block closures to not require any memory allocation.

We extend this slightly, by using 5 of the 8 possible tag values:
- 2: `SmallInteger`. Recognized by `u & 2 != 0`. This provides 62-bit `SmallInteger` values in the high bits. (note this test also matches 3,6,7 - which are not used)
- 4 and 5: `Float`. Recognized by u  & 6 == 4. By using 2 tags we can encode all 64-bit floats in the ranges 0..3.8e-270, 5.9e-39..6.8e38, and 1.1e270..1.8e308 as well as NaN and Inf. Any other values will be heap allocated. For the vast majority of applications this range will allow all values to be coded as immediate values. Encoding is a left rotate by 5 bits and subtract 3, then a check if the bottom 3 bits are 4 or 5. These are both several instructions shorter than Spur and involve no conditional code on decode.
- 0: Pointer or `nil`. Recognized by u  & 7 == 0. This is compatible with native 8-byte-aligned pointers, so no conversion is required. This compatibility extends to `nil` being equivalent to `null` in C, C++, Rust, or Zig. If the high 5 bits non-zero then this is an immediate pointer to a `BlockClosure`  that takes 0 or 1 parameters (and this doesn't even need to be removed for AARCH64).
- 1: immediate values for immutable classes. Recognized by u  & 7 == 1.  For the classes `Character`, `Symbol`, `True`, `False`. The high 5 bits are the class number, and the middle 56 bits are the information (the character Unicode value or the symbol hash code).


| High 16 bits       |                   | Tag        | Type                   |
| ------------------ | ----------------- | ---------- | ---------------------- |
| `0000000000000000` | `00000000...0000` | `00000000` | `nil`                  |
| `0000000000000000` | `aaaaaaaa...aaaa` | `aaaaa000` | (heap) pointer         |
| `00001lllllllllll` | `aaaaaaaa...aaaa` | `aaaaa000` | `ThunkReturnLocal`     |
| `00010iiiiiiiiiii` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkReturnInstance`  |
| `00011nnnnnnnnnnn` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkReturnObject`    |
| `00100iiiiiiiiiii` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkReturnImmediate` |
| `00101000cccccccc` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkReturnCharacter` |
| `00110000seeemmmm` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkReturnFloat`     |
| `00111000llllllll` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkLocal`           |
| `01000000llllllll` | `aaaaaaaa...aaaa` | `aaaaa001` | `BlockAssignLocal`     |
| `01001iiiiiiiiiii` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkInstance`        |
| `01010iiiiiiiiiii` | `aaaaaaaa...aaaa` | `aaaaa001` | `BlockAssignInstance`  |
| `0101100000000000` | `aaaaaaaa...aaaa` | `aaaaa001` | `ThunkHeap`            |
| `01100ttttttttttt` | `aaaaaaaa...aaaa` | `aaaaa001` | `LLVM`                 |
| `01101cccccxxxxxx` | `xxxxxxxx...xxxx` | `xxttt001` | `ThunkImmediate`       |
| `01110seeeeeeeeee` | `emmmmmmm...mmmm` | `mmmmm001` | `ThunkFloat`           |
| `1000000000000000` | `00...hhhhh...hh` | `hhhhh001` | `Symbol`               |
| `10001000cccccccc` | `cc...hhhhh...hh` | `hhhhh001` | `Signature`            |
| `1001000000000000` | `00000000...0000` | `10010001` | `False`                |
| `1001100000000000` | `00000000...0000` | `10011001` | `True`                 |
| `1010000000000000` | `0...uuuuuuuuuuu` | `uuuuu001` | `Character`            |
| `iiiiiiiiiiiiiiii` | `iiiiiiii...iiii` | `iiiii010` | `SmallInteger`         |
| `xxxxxxxxxxxxxxxx` | `xxxxxxxx...xxxx` | `xxxxx011` | unused                 |
| `ffffffffffffffff` | `ffffffff...ffff` | `fffff100` | `Float`                |
| `ffffffffffffffff` | `ffffffff...ffff` | `fffff101` | `Float`                |
| `xxxxxxxxxxxxxxxx` | `xxxxxxxx...xxxx` | `xxxxx110` | unused                 |
| `xxxxxxxxxxxxxxxx` | `xxxxxxxx...xxxx` | `xxxxx111` | unused                 |

Getting the class looks at the low 3 bits:
- 0: it's `nil` or it's a heap object and need to look at the header and the class is in the high 5 bits, unless they are zero, in which case the class is fetched from the object header ,
- 1: it's an immediate and the high 5 bits are the class,
- 2: it's a `SmallInteger`
- 4, 5: it's a `Float`
- 3,6,7: reserved as false-positives for `SmallInteger`

`basicHash` for any of the non-pointer-containing values  is the high 32 bits `xor` with the low 32 bits, encoded as a `SmallInteger`. The hash value for any pointer value is accessed from the object header, encoded as a `SmallInteger`.
### Immediates

Any selection of zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. They also mostly have very few instances, so aren't likely to be usefully optimized. The currently identified ones that do  are `True`, `False`, `Character`, and `Symbol`, as well as `SmallInteger`. By restricting to only 5 bits for the class, we significantly increase the range of useful values we can encode into immediate values.

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 5 bits and the hash code is 24-56 bits. Some immediate classes use the hash24 value which is the 24 bits after the tag. Others contain a pointer in the low 48 bits with some other data in the other 11 bits.
#### Class numbers
For Mixed immediate encoding any address (`Context` or `ContextData`) is encoded in the low 48 bits. The extra field is the 11 bits below the class, which limits the immediate form to 2048 possible extra values. For non-immediate versions, the bits of the object form the address of an in-memory object, of which the first word is the address and the second word is the extra value.
1. `ThunkReturnLocal`: This and the following 4 classes encode thunks that do non-local returns of a value. The address is the address of the `Context`. This class returns a value from the `Context` (a local, parameter, or the `self` object - 0 is unused, 1 is `self`, 2 is the first parameter or local, etc.). The local index is encoded in the extra field .
2. `ThunkReturnInstance`: This encodes a non-local return of an instance variable. The variable index is encoded in the extra field. The `self` field of the `Context` is the referenced object.
3. `ThunkReturnObject`: This encodes a non-local return of an object. In the immediate version, this is an 11-bit signed integer, encoded in the extra field.
4. `ThunkReturnImmediate`: This encodes a non-local return of an immediate value. Simply returns the high 5 bits of the extra field, 53 0-bits, and the low 6 bits of the extra field, so 0 is `nil`, `01101000001` is `true`, etc.  This is never used for in-memory closures as it is subsumed by `ThunkReturnObject`.
5. `ThunkLocal`: This evaluates to the value of a local variable in the `ContextData` referred to in the low 48 bits. The variable index is encoded in the extra field - see `ThunkReturnLocal`.
6. `BlockAssignLocal`: This takes 1 parameter and assigns the value to a local variable in the `ContextData`. That value is also the result. The local index is encoded in the extra field. If the local variable number is 0, no assignment is done, and it simply does a non-local return of the parameter - i.e. the pointer is treated as a `Context` and this is a continuation.
7. `ThunkInstance`: This evaluates to the value of an instance variable of the object referred to in the low 48 bits. The variable index is encoded in the extra field.
8. `BlockAssignInstance`: This takes 1 parameter and assigns the value to an instance variable of the object referred to in the low 48 bits. That value is also the result. The variable index is encoded in the extra field.
9. `ThunkHeap`: This encodes a thunk (a `BlockClosure` that takes no parameters) that evaluates to a heap object. The address of the heap object is in the low 48 bits. The extra field is ignored.
10. `ThunkImmediate`: This encodes  a thunk that evaluates to an immediate value. This is never used for in-memory closures as it is subsumed by `ThunkInstance`. The 56 bit extra field is interpreted as:
	- 0 & 1: The high 5 bits of the field, 8 zero bits, and the low 51 bits are returned. This encodes `nil` and all the immediate values (symbols, characters, booleans). Although this could, in theory encode an address, it must not
	- 2 (& 3): A sign-extended copy of the field is returned. This encodes 54-bit `SmallInteger`s
11. `Symbol`: See [Symbol](Symbol.md) for detailed information on the format.
12. `False`: This encodes the singleton value `false`. The `False` and `True` classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation).
13. `True`: This encodes the singleton value `true`.
14. `Character`: The hash code contains the full Unicode value for the character/code-point. This allows orders of magnitude more possible character values than the 294,645 allocated code points as of [Unicode](https://www.unicode.org/versions/stats/)16 and even the 1,112,064 possible Unicode code points.
15. `Signature`: This is a superset of [Symbol](Symbol.md) with a `ClassIndex` and a possible 8-bit primitive number.
16. `ThunkReturnCharacter`: This encodes a non-local return of an immediate 11-bit `Character` encoded in the extra field. This doesn't encode all characters, but it encompasses the first 2,048 code points, which are primarily used for Latin-based languages, European languages, Middle Eastern languages, and common symbols. This is never used for in-memory closures as it is subsumed by `ThunkReturnObject`.
17. `ThunkReturnFloat`: This encodes a non-local return of a limited floating-point value, with the value encoded in the extra field. This only encodes 256... values, but it encodes all the integral values from 0 to 8, all the powers of 2 up to 128 and their inverses, powers of 2 times 10 up to 320, 1/8 to 7/8, `nan` and `inf`. And it has negative and positives for all of them. This is never used for in-memory closures as it is subsumed by `ThunkReturnObject`.
18. `ThunkFloat`: This encodes  a thunk that evaluates to an immediate `Float` value.... This is never used for in-memory closures as it is subsumed by `ThunkInstance`.
19. `LLVM`: Interface object to LLVM library. The 11 bit tag differentiates different kinds of LLVM JIT pointers. Although these include addresses, they are not object references and not looked at by the garbage collector.
20. `SmallInteger`: this encodes small integers. In this encoding, the high 62 bits of the word make up the value, so this provides 62-bit integers (-2,305,843,009,213,693,952 to 2,305,843,009,213,693,951). This allows numerous optimizations of `SmallInteger` operations (see [[Optimizations]]). **placeholder**
21. to 31 unused

The additional classes that are hard-coded (because they are referenced by Zig code) are:
32. `UndefinedObject`: the singleton value `nil` which is represented as all zero bits. **placeholder**
33. `Context`: method context. Note this must be #33 so that it relates to the reserved class #4
34. `Float`: the bit patterns that encode double-precision IEEE floating point. **placeholder**
35. `ProtoObject`: the master superclass. This is an address of an in-memory object.
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

When a `[`*some-value*`]` closure is required and *some-value* is a literal, self, or a parameter to the method (i.e. something that can't be assigned to), runtime code returns either a `ThunkImmediate`/`ThunkFloat` (if the value is immediate or numeric and fits), a `ThunkHeap` when the value is a heap object, and object referencing thunk for a local or instance variable, or if the value doesn't fit any of these constraints it will fall back to a `ThunkInstance` pointing to a stack-allocated `ThunkInstance` object with 1 field: the value. This applies to `self` or any other runtime value.

There are pre-defined CompiledMethods for some common closures:
1. id: `[:x|x]` - 
2. return id: `[:x| ^ x]`
3. return value: `[^ value]` - when the value is outside the non-local thunk group. The value is the only thing in the closure other than the method address and the `Context` pointer.

More information on closures can be found at [[Execution#BlockClosures]].
