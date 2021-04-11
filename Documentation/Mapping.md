## Mapping
[Virtual Machine Warmup Blows Hot and Cold](https://youtu.be/vLl4GteL9Mw)
[mmap man page](https://www.man7.org/linux/man-pages/man2/mmap.2.html)
[Rusty Runtimes: Building Languages In Rust](https://youtu.be/U3upi-y2pCk)
[Unsafe Rust](https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html)

### Object encoding
The IEEE 754 64-bit binary number is encoded as follows:
	![IEEE 754 Binary-64](Pasted%20image%2020210311212924.png)

When the 11 mantissa bits are all 1s and at least one of the bottom 51 bits is non-zero, then the value is considered Not a Number (NaN), and the low 51 bits are otherwise ignored as a floating point number.^[Bit 51 could also be 1 to make a quiet (non-signaling) NaN, but it doesn't seem necessary.]

So we have 52 bits to play with, as long as the number is non-zero. This lets us encode 2^52 possible values (see the comment at [SpiderMonkey](https://github.com/ricardoquesada/Spidermonkey/blob/4a75ea2543408bd1b2c515aa95901523eeef7858/js/src/gdb/mozilla/jsval.py)). They further point out that on many architectures only the bottom 48 bits are valid as memory addresses, and when used as such, the high 16 bits must be the same as bit 47.

There are several ways to do NaN tagging/encoding. You can choose integers, pointers, or doubles to be naturally encoded and all the others be encoded with some shifting/adding. While integers and pointers are probably more common in most Smalltalk images, leaving doubles as naturally encoded means that vector instructions and/or GPUs could act directly on memory.

So this leaves us with the following encoding based on the **S**ign+**M**antissa and **F**raction bits:

| S+M       | F    | F    | F    | Type            |
| --------- | ---- | ---- | ---- | --------------- |
| 0000      | 0000 | 0000 | 0000 | double  +0      |
| 0000-7FEF | xxxx | xxxx | xxxx | double          |
| 7FF0      | 0000 | 0000 | 0000 | +inf            |
| 7FF8      | 0000 | 0000 | 0001 | NaN (generated) |
| 7FF0-F    | xxxx | xxxx | xxxt | tagged literals |
| 8000      | 0000 | 0000 | 0000 | double     -0   |
| 8000-FFEF | xxxx | xxxx | xxxx | double          |
| FFF0      | 0000 | 0000 | 0000 | -inf            |
| FFF8-F    | xxxx | xxxx | xxxx | NaN (unused)    |

So, interpreted as an i64, any value that is less than or equal to +inf is a double. Else, the bottom 3 bits are a class tag, so the first 7 classes have a compressed representation.^[note that we don't encode 0 or 1 as literal values, so there is no conflict with the generated NaN value or +inf].

### Literals
All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. The currently identified ones are `nil`, `true`, `false`, Integers, Floats, Characters, and Symbols.

Literals are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 3 bits and the hash code is 49 bits. The encodings for UndefinedObject, True, and False are extremely wasteful of space, but the efficiency of dispatch and code generation depend on them being literal values and having separate classes

#### Tag values
0. Heap object addresses: This is an address of a heap object, so sign-extending the address is all that is required. Since all heap objects are 8-byte aligned, this gives us 52-bit addresses, which is beyond current architectures. Note that the address can't be 0, or it would look like +inf.
1. BlockClosure: These are the address of a heap closure object. By coding separately from other objects, we don't have to create a class entry for each closure. The hash field in the object header is the symbol index for `value`, `value:`, or whatever the value selector is for this particular block. So a simple match against the message selector works for value messages, and if it doesn't match, it does a dispatch against the BlockClosure class. Note that the generated NaN value is also possible here, but it will have a 0 address so a simple check will dispatch that as a Float.
2. False: The False and True classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation). This only encodes the single value `false`.
3. True: This only encodes the single value `true`
4. UndefinedObject: This encodes the value `nil` with all zero hash code. It also encodes a set of `specialReturn` values that can be returned from any message send for the purpose of unwinding the stack. For this, the hash value will be the address of the stack for the enclosing method.
5. SmallInteger: For integers the "hash code" is the value, so this provides 49-bit integers. While other encodings could give an additional bit, decoding would be slower and therefore is not worth doing.
6. Symbol: The hash code contains 2 portions: the low 28 bits are an index into the symbol table, 1 bit to code alternate versions of a symbol (for primitive failure dispatch), and the next 5 bits are the arity of the symbol. This supports millions of symbols (a typical Pharo image has about 90,000 symbols). For Symbols, the hash-code field is an index into a table that points to the an internal representation of the Symbol, including the characters that make it up, but all that really is necessary is in the hash-code. It is used to hash into the selector table on a method dispatch, and is obviously necessary for testing equality. This means that several methods that access the string-ness of the symbol need to be specially coded, and means that unreferenced symbols cannot be reclaimed in the basic garbage collection process. However this seems worth it so that dispatch can proceed without having to follow a pointer. So a separate mechanism must be added to collect unused symbols and the associated Strings. Also an internal data structure (either a [balanced search](https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree) tree or a hash tabled (hashed on the string, not the hash code)) must be used for `String>>#asSymbol`.
7. Character: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.
8. Float: This isn't encoded in the tag, but rather with all the values outside the range of literals.

### Object in Memory
We are following some of the basic ideas from the [SPUR](http://www.mirandabanda.org/cogblog/2013/09/05/a-spur-gear-for-cog/) encoding for objects on the heap, used by the [OpenSmalltalk VM](https://github.com/OpenSmalltalk).

There are a few significant changes:
1. We are using a pure generational copying collector. This means that we need forwarding pointers during collection. We encode this with the sign-bit of the header word, so a negative object is a forward. Rather than masking the value we store the negation of the pointer as a a forward.
2. `become:` will be implemented with similar forwarding flagging. `become:` will replace both objects headers with forwarding pointer to an object that just contains the original object references and revised header words. When the objects are collected, the references will be updated.
3. References from old-generation to new generation will use forwarding as well (the new object will be copied to the older space, and leave a forwarding pointer behind - note if there is no space for this copy, this could force a collection on an older generation without collecting newer generations)

First we have the object format tag. The bits in the tag are:

0. has instance variables
1. unused (possibly "pointer-free")
2. is weak
3. indexable
4. indexable, no pointers (no requirement for scan on garbage collection)

where the low 4 bits are only interpreted when bit 4=0. So any value <8 has only instance variables; any value <16 has pointers. Only the following values currently have meaning:
- 1: non-indexable objects with inst vars (Point et al) 
- 5: weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
- 8: indexable objects with no inst vars (Array et al)
- 9: indexable objects with inst vars (MethodContext AdditionalMethodState et al)
- 12: weak indexable objects with inst vars (WeakArray et al)
- 16: 64-bit indexable - non-pointers (Array with only literals and Floats, DoubleWordArray,) Arrays are initially created as format 16, but change to format 8 if a closure or other heap reference is stored. During garbage collection, if no reference is found during the scan, it reverts to format 16.
- 18-19: 32-bit indexable - low bit encodes unused bytes at end (WordArray, IntegerArray, FloatArray, WideString)
- 20-23: 16-bit indexable - low 2 bits encode unused bytes at end (DoubleByteArray)
- 24-31: byte indexable - low 3 bits encode unused bytes at end (ByteArray, Stringe)

This is the first field in the header-word for an object:

| Bits | What         | Characteristics                                              |
| ---- | ------------ | ------------------------------------------------------------ |
| 1    | isForward    | if set, negation of long-word is address of forwarded object |
| 15   | numSlots     | number of long-words beyond the header                       |
| 1    | isImmutable  |                                                              |
| 1    | unused       |                                                              |
| 1    | unused       |                                                              |
| 5    | format       | (see above)                                                  |
| 20   | identityHash |                                                              |
| 20   | classIndex   | LSB                                                          |

Unless format=9, there aren't **both** indexable elements and instance variables. This means unless the number of words of allocation is more than 32766, it can be encoded in the header length field.

If the length field=32767, the header word is followed by a word with the index allocation. In this case the total number of words allocated to the object is 2 plus the value of the index allocation word.

If the format=9, the header word is followed by a word with the index allocation. In this case the total number of words allocated to the object is 2 plus the value of the length field (for the instance variables which can't be 32K) plus the value of the index allocation word, with the instance variables immediately following the index allocation word, followed by the indexed elements.

#### Examples

A simple object like Point with 2 instance variables would look like:

| Value               | Description         |
| ------------------- | ------------------- |
| 0002 01hh hhhc cccc | length=2, format=1  |
| xxxx xxxx xxxx xxxx | instance variable 1 (x) |
| xxxx xxxx xxxx xxxx | instance variable 2 (y) |



So an Array of 5 elements would look like:

| Value               | Description                  | 
| ------------------- | ---------------------------- |
| 0005 08hh hhh0 000D | length=5, format=8, class=13 |
| xxxx xxxx xxxx xxxx | index 1                      |
| xxxx xxxx xxxx xxxx | index 2                      |
| xxxx xxxx xxxx xxxx | index 3                      |
| xxxx xxxx xxxx xxxx | index 4                      |
| xxxx xxxx xxxx xxxx | index 5                      |

And an Array of 2^20 elements would look like:

| Value               | Description                      |
| ------------------- | -------------------------------- |
| 7FFF 08hh hhh0 000D | length=32767, format=8, class=13 |
| 0000 0000 0010 0000 | 2^20                             |
| xxxx xxxx xxxx xxxx | index 1                          |
| xxxx xxxx xxxx xxxx | index 2                          |
| xxxx xxxx xxxx xxxx | index 3                          |
| ...                 | intermediate values              |
| xxxx xxxx xxxx xxxx | index 2^20                       | 

And a format 9 object with 2 instance variables and 3 indexable elements would look like:

| Value               | Description         |
| ------------------- | ------------------- |
| 0002 09hh hhhc cccc | length=2, format=9  |
| 0000 0000 0000 0003 | 3                   |
| xxxx xxxx xxxx xxxx | instance variable 1 |
| xxxx xxxx xxxx xxxx | instance variable 2 |
| xxxx xxxx xxxx xxxx | index 1             |
| xxxx xxxx xxxx xxxx | index 2             |
| xxxx xxxx xxxx xxxx | index 3             | 


![Stats from Pharo Image](Pasted%20image%2020210320170341.png)

## Web resources
#### Rust
- [primitive pointers](https://doc.rust-lang.org/std/primitive.pointer.html) and [nightly](https://doc.rust-lang.org/nightly/std/primitive.pointer.html)
- [tranmute](https://doc.rust-lang.org/nightly/std/mem/fn.transmute.html)
- [std::ptr](https://doc.rust-lang.org/nightly/std/ptr/index.html)
- [libc](https://docs.rs/libc/0.2.68/libc/)
- [labguage doc](https://doc.rust-lang.org/nightly/reference/expressions/if-expr.html)
- [conditional compilation](https://doc.rust-lang.org/reference/conditional-compilation.html)

#### Garbage collection
- [Boehm MS vs Copying](https://hboehm.info/gc/complexity.html)
- [Cornell course copying](http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm)
- [uta copying](https://lambda.uta.edu/cse5317/notes/node48.html)