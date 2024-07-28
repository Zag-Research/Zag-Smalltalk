### Modified Spur Encoding
Spur is the encoding used by [OpenSmalltalkVM](https://github.com/OpenSmalltalk).

This is evolved from an earlier attempt [[Encoding-Modified-Spur-Old]]

Spur uses the [following format](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/):
![[Pasted image 20240115082827.png]]
This provides 61-bit SmallInteger and immediate floats that have an exponent range equivalent to 32-bit floats (approximately 1e-77 to 1e77) with the full 64-bit float mantissa. Determining which of the 4 types is a simple bit-test. Decoding the float is a test for the zero case and then a shift, add a 64-bit constant, and a rotate.

We extend this slightly, by using all 8 possible tag values:
0: Pointer or `nil`. This is compatible with native 8-byte-aligned pointers, no no conversion is required.
1: immediate values for the classes `SmallInteger`, `Character`, `Symbol`, `True`, `False` as well as several forms of thunk (`BlockClosure`s that take no parameters). The next 5 bits are the class number, and the top 56 bits are the information (the integer value or the character Unicode value or the symbol hash code). In a few cases, the top 48 bits provide a 48-bit address allowing capture of heap objects, including contexts.
2-7: `Float`. By using 6 tags we can encode all 64-bit floats less than 2.68e154. Any value larger than that will be heap allocated. For the vast majority of applications this range will allow all values except `+inf`, `-inf`, and `nan` to be coded as immediate values. Because those values may occur, we save the heap allocation by recognizing them and using a reference to a statically allocated value. Decoding doesn't need to handle zero specially, and is simply: subtract 2, and rotate right 4 bits. Encoding is similarly: rotate left 4 bits and add 2; if the result anded with 6 is zero, immediate encoding is not possible.

| High 8 bits | Next 24 bits |            |            |            | Tag        | Type                   |
| ----------- | ------------ | ---------- | ---------- | ---------- | ---------- | ---------------------- |
| `00000000`  | ...          | `00000000` | `00000000` | `00000000` | `00000000` | `nil`                  |
| `aaaaaaaa`  | ...          | ...        | `aaaaaaaa` | `aaaaaaaa` | `aaaaa000` | pointer                |
| `aaaaaaaa`  | ...          | ...        | `aaaaaaaa` | `00000000` | `00001001` | `ThunkHeap`            |
| `aaaaaaaa`  | ...          | ...        | `aaaaaaaa` | `eeeeeeee` | -          | -                      |
| `aaaaaaaa`  | ...          | ...        | `aaaaaaaa` | `cccccccc` | `00101001` | `ThunkReturnCharacter` |
| `00000000`  | ...          | `00000000` | `00000000` | `00000000` | `00111001` | `true`                 |
| `00000000`  | ...          | `00000000` | `00000000` | `00000000` | `01000001` | `false`                |
| `xxxxxxxx`  | ...          | ...        | `xxxxxxxx` | `xxxxxxxx` | `01001001` | `SmallInteger`         |
| `00000000`  | ...          | hhhhhhhh   | `hhhhhhhh` | `hhhhhhhh` | `01010001` | `Symbol`               |
| `00000000`  | ...          | hhhhhhhh   | `hhhhhhhh` | `hhhhhhhh` | `01011001` | `Character`            |
| `xxxxxxxx`  | ...          | ...        | `xxxxxxxx` | `ccccc001` | `01100001` | `ThunkImmediate`       |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | `mmmmmmmm` | `mmmms010` | `01101001` | `ThunkFloat`           |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | ...        | `mmmmmmmm` | `mmmms010` | `Float`                |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | ...        | `mmmmmmmm` | -          | `Float`                |
| `eeeeeeee`  | `mmmmmmmm`   | ...        | ...        | `mmmmmmmm` | `mmmms111` | `Float`                |

Because we are using all 8 possible values of the tag field, where the test in Spur for "is a `SmallInteger`" was simply an `and`, using our encoding it requires comparing the low byte with a constant. However testing for a `Float` is simply an `and` with 6.

Testing for a pointer-containing object (for the garbage collector) if the value is not `nil`, takes the low byte, rotates right 3 and if the result is less than 0b00100110 then it contains a pointer.

Getting the class looks at the low 3 bits: 0 it's `nil` or it's a heap object and need to look at the header, 1 it's an immediate and the next 5 bits are the class, 2-7 it's a `Float`.
### Immediates

All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. They also mostly have very few instances, so aren't likely to be usefully optimized. The currently identified ones that do  are `true`, `false`, Characters, and Symbols. By restricting to only 5 bits for the class, we significantly increase the range of useful values we can encode into immediate values.

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 5 bits and the hash code is 24-56 bits. Most immediate classes use the hash24 value which is the 24 bits after the tag. A few contain a pointer in the top 48 bits with some other data in the other 8 bits.
#### Class numbers
1. `ThunkHeap`: This encodes a thunk that evaluates to a heap object. The address of the heap object is in the high 48 bits. (A non-zero extra field could be encoded to access an instance variable, if that is deemed to be a useful optimization.)
2. `ThunkReturnLocal`: This and the following 3 classes encode thunks that do non-local returns of a value, where the address of the `Context` is in the high 48 bits. This class returns a value from the `Context` (a local, parameter, or the `self` object). The local offset is encoded in the extra field.
3. `ThunkReturnSmallInteger`: This encodes a non-local return of a 8-bit signed integer, encoded in the extra field.
4. `ThunkReturnImmediate`: This encode a non-local return of an immediate value (`nil`, `false`, or `true`). Simply returns the extra field, so 0 is `nil`, `00111001` is `true`, etc.
5. `ThunkReturnCharacter`: This encodes a non-local return of an 8-bit character, with the character encoded in the extra field. This doesn't encode all characters, but it encodes all the ASCII characters.
6. `UndefinedObject`: This is reserved for the singleton value `nil` which is represented as all zero bits. 
7. `True`: This encodes the singleton value `true`.
8. `False`: This encodes the singleton value `false`. The `False` and `True` classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation).
9. `SmallInteger`: this encodes small integers. In this encoding, the high 56 bits of the word make up the value, so this provides 56-bit integers (-36,028,797,018,963,968 to 36,028,797,018,963,967). This allows numerous optimizations of `SmallInteger` operations (see [[Optimizations]]).
10. `Symbol`: See [Symbols](Symbols.md) for detailed information on the format.
11. `Character`: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.
12. `ThunkImmediate`: This encodes  a thunk that evaluates to an immediate value. A sign-extended copy of the top 56 bits is returned. This encodes 48-bit `SmallInteger`s, and all of the other immediate values.
13. `ThunkFloat`: This encodes  a thunk that evaluates to a `Float` value. A copy of the top 52 bits, concatenated to 8 zero bits and the next 4 bits. This encodes any floating-point number we can otherwise encode as long as the bottom 8 bits are zero (this include any reasonable integral value as well as common fractional values such as 0.5, 0.25). Values that can't be encoded that way would use `ThunkHeap` to return an object.
14. `Float`: this is reserved  for the bit patterns that encode double-precision IEEE floating point.
15. `Object`: this is reserved for the master superclass. This is also the value returned by `immediate_class` for all heap and thread-local objects. This is an address of an in-memory object.

### Thunks and Closures
Full block closures are relatively expensive. Even though many will typically be discarded quickly, they take dozens of instructions to create. They are allocated on the stack (because most have LIFO behaviour) which puts pressure on the stack which may force the stack to overflow more quickly and need to be spilled to the heap, and some will put pressure on the heap directly - both causing garbage collections to be more frequent. There are many common blocks that don't actually need access to method local variables, `self` or parameters. These can be encoded as immediate values with special subclasses of BlockClosure and obviate the need for heap allocation. 
1. a `ThunkImmediate` acts as a niladic BlockClosure that evaluates to 56 bits of immediate value, which includes 48-bit integers and all other immediate values.
2. a `ThunkFloat` similarly supports float values with 48-bits of significance in the mantissa.
3. the non-local thunks (`ThunkHeap`, `ThunkReturnLocal`, `ThunkReturnImmediate`, `ThunkReturnCharacter`) do a non-local return of one of their values.
4. all remaining closures are full block closures and are memory objects, They are allocated on the stack (because most will disappear when their containing method returns) but they may need to be moved to a heap. They contain the following fields in order (omitting any unused fields):
	1. the address of the CompiledMethod object that contains various values, and the threaded code implementation (if this is the only field the block has no closure or other variable fields, so the block can be statically allocated - otherwise it needs to be stack allocated (which could be moved to a heap);
	2. the address of the Context if there are any non-local returns (if a closure that references a Context is forced to the heap, that will force that Context to be promoted to the heap, which will force the Context that that refers to, etc. - essentially dumping the whole stack to the heap);
	3. the address of the value holding block if there were multiple blocks in a method and mutable values needed by this block were allocated in another block (there could conceivably be multiples if there are blocks within blocks);
	4. the values of `self`, parameters, or locals that are only used in the block after being initialized in the method, as well as (if this is a local-holding block) any mutable locals used by this or other blocks.

When a `[`*some-value*`]` closure is required and *some-value* is a literal, self, or a parameter to the method (i.e. something that can't be assigned to), runtime code returns either a `ThunkImmediate`/`ThunkFloat` (if the value is immediate or numeric and fits), a `ThunkHeap` when the value is a heap object, or if the value doesn't fit any of these constraints it will fall back to a full closure with 2 fields: the "return one field" CompiledMethod reference and the value. This applies to `self` or any other runtime value.
