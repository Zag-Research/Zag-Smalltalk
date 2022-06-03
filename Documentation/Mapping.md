## Mapping
[Virtual Machine Warmup Blows Hot and Cold](https://youtu.be/vLl4GteL9Mw)
[mmap man page](https://www.man7.org/linux/man-pages/man2/mmap.2.html)
[Rusty Runtimes: Building Languages In Rust](https://youtu.be/U3upi-y2pCk)
[Unsafe Rust](https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html)

### Object encoding
The IEEE 754 64-bit binary number is encoded as follows:
	![IEEE 754 Binary-64](../images/Pasted%20image%2020210311212924.png)

When the 11 mantissa bits are all 1s and at least one of the bottom 51 bits is non-zero, then the value is considered Not a Number (NaN), and the low 51 bits are otherwise ignored as a floating point number.^[Bit 51 could also be 1 to make a quiet (non-signaling) NaN, but it doesn't seem necessary.]

So we have 52 bits to play with, as long as the number is non-zero. This lets us encode 2^52 possible values (see the comment at [SpiderMonkey](https://github.com/ricardoquesada/Spidermonkey/blob/4a75ea2543408bd1b2c515aa95901523eeef7858/js/src/gdb/mozilla/jsval.py)). They further point out that on many architectures only the bottom 48 bits are valid as memory addresses, and when used as such, the high 16 bits must be the same as bit 47.

There are several ways to do NaN tagging/encoding. You can choose integers, pointers, or doubles to be naturally encoded and all the others be encoded with some shifting/adding. While integers and pointers are probably more common in most Smalltalk images, leaving doubles as naturally encoded means that vector instructions and/or GPUs could act directly on memory.

So this leaves us with the following encoding based on the **S**ign+**E**xponent and **F**raction bits:

| S+E       | F    | F    | F    | Type            |
| --------- | ---- | ---- | ---- | --------------- |
| 0000      | 0000 | 0000 | 0000 | double  +0      |
| 0000-7FEF | xxxx | xxxx | xxxx | double (positive)         |
| 7FF0      | 0000 | 0000 | 0000 | +inf            |
| 7FF0-F    | xxxx | xxxx | xxxx | NaN (unused)    |
| 8000      | 0000 | 0000 | 0000 | double     -0   |
| 8000-FFEF | xxxx | xxxx | xxxx | double (negative)         |
| FFF0      | 0000 | 0000 | 0000 | -inf            |
| FFF0      | xxxx | xxxx | xxxx | unused |
| FFF1      | 0000 | 0100 | 0002 | UndefinedObject |
| FFF2      | 0000 | 0001 | 0000 | False |
| FFF3      | 0000 | 0010 | 0001 | True |
| FFF4      | xxxx | xxxx | xxxx | Context |
| FFF5      | 05aa | xxxx | xxxx | Symbol |
| FFF6      | 0600 | xxxx | xxxx | Character |
| FFF7      | xxxx | xxxx | xxxx | heap object |
| FFF8-F      | xxxx | xxxx | xxxx | SmallInteger |
| FFF8      | 0000 | 0000 | 0000 | SmallInteger minVal|
| FFFC      | 0000 | 0000 | 0000 | SmallInteger 0|
| FFFF      | FFFF | FFFF | FFFF | SmallInteger maxVal|

So, interpreted as a u64, any value that is less than or equal to -inf is a double. **Not correct:** Else, the top 3 bits of the fraction  are a class tag, so the first 7 classes have a compressed representation.

### Literals
All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. The currently identified ones that do  are `nil`, `true`, `false`, Integers, Floats, Characters, and Symbols.

Literals are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 4 bits and the hash code is 48 bits. The encodings for UndefinedObject, True, and False are extremely wasteful of space (because there is only one instance of each, so the hash code is irrelevant), but the efficiency of dispatch and code generation depend on them being literal values and having separate classes.

#### Tag values
1. UndefinedObject: This encodes the value `nil` with all zero hash code.
2. False: The False and True classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation). This only encodes the single value `false`.
3. True: This only encodes the single value `true`
4. Context: A `thisContext` value
5. Symbol: See [Symbols](Symbols.md) for detailed information on the format.
6. Character: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.
7. Heap object addresses: This is an address of a heap object, so sign-extending the address is all that is required. This gives us 49-bit addresses, which is beyond current architectures.
8. SmallInteger: This isn't encoded in the tag. For integers the low 51 bits of the"hash code" make up the value, so this provides 51-bit integers (-1,125,899,906,842,624 to 1,125,899,906,842,623). The negative integers are first, with a 0 in the high bit, followed by the positive integers with a 1 in the high bit. This allows numerous optimizations of SmallInteger operations (see [[Optimizations]]).
9. Float: This isn't encoded in the tag, but rather with all the values outside the range of literals (where the S+M is less than 0xFFF).

### Object in Memory
We are following some of the basic ideas from the [SPUR](http://www.mirandabanda.org/cogblog/2013/09/05/a-spur-gear-for-cog/) encoding for objects on the heap, used by the [OpenSmalltalk VM](https://github.com/OpenSmalltalk).

There are a few significant changes:
1. We are using a pure generational copying collector. This means that we need forwarding pointers during collection. We encode this with the sign-bit of the header word, so a negative object is a forward. Rather than masking the value we store the negation of the pointer as the forward.
2. `become:` will be implemented with similar forwarding flagging. `become:` will replace both objects headers with forwarding pointer to an object that just contains the original object references and revised header words. When the objects are collected, the references will be updated.
3. References from old-generation to new generation will use forwarding as well (the new object will be copied to the older space, and leave a forwarding pointer behind - note if there is no space for this copy, this could force a collection on an older generation without collecting newer generations)

First we have the object format tag. The low 5 bits code the following:
- bit 0: = 1 means the object contains instance variables
- bit 1: = 2 means the object is indexable
- bit 2: = 4 means the pointers in the object are weak references
- bit 3: = 8 means none of the objects are pointers (i.e. they are all literals)
- bit 4: = 16 means none of values in the object are objects and the low 4 bits encode the length of the values

Therefore, only the following values currently have meaning:
- 1: non-indexable objects with inst vars (Association et al) 
- 2: indexable <32k objects with no inst vars
- 3: indexable objects with inst vars (MethodContext AdditionalMethodState et al)
- 7: weak indexable objects with inst vars (WeakArray et al) also (Ephemeron)
- 9: non-indexable objects with inst vars - no pointers (Point et al)
- 10: indexable <32k objects with no inst vars - no pointer (Array et al)
- 11: indexable objects with inst vars - no pointers
- 17: 64-bit indexable - non-pointers (DoubleWordArray,DoubleArray,)
- 18-19: 32-bit indexable - low bit encodes unused half-words at end (WordArray, IntegerArray, FloatArray, WideString)
- 20-23: 16-bit indexable - low 2 bits encode unused quarter-words at end (DoubleByteArray)
- 24-31: byte indexable - low 3 bits encode unused bytes at end (ByteArray, String)

Everything >= 8 has no pointers, so GC doesn't look at them. Things are initially created as their pointer-free version (9,10,11), but change to their pointer-containing version (1,2,3) if a pointer is stored in them. During garbage collection, if no reference is found during the scan, they revert to the pointer-free version (i.e. bit 3 is set).

For 1, 2, 9, 10, the size is determined by the length field. The only difference between 1 and 2 is whether `at:`, `size`, etc. should work or give an error. Similarly 9 and 10.

For 3, 7, 11 the length field is the number of instVars (possibly 0) which are followed by a word containing the size of the indexable portion, which follows. Weak objects are rare enough that we don't bother to handle cases with no instance variables or no indexable values separately.

For 17-31 the length field is the number of words of the array, unless it is 32767, in which case the values are preceded by a size which is the number of additional words. They are assumed to never be interpreted as objects, so update never considers promoting to a pointer-containing format. Also the objects are initialized with zeros, rather than the `nil` for all < 16.

This is the header-word for an object:

| Bits | What         | Characteristics                                              |
| ---- | ------------ | ------------------------------------------------------------ |
| 1    | isForward    | if set, negation of long-word is address of forwarded object |
| 15   | length       | number of long-words beyond the header                       |
| 1    | unused       | \                                                           |
| 1    | isGlobal     |  !                                                            |
| 1    | isImmutable  |  !                                                           |
| 1    | isRawData    |  !                                                            |
| 1    | isPointerFree|  + format(see above)                                                  |
| 1    | isWeak       |  !                                                            |
| 1    | isIndexable  |  !                                                           |
| 1    | hasInstVars  | /                                                           |
| 24   | identityHash |                                                              |
| 16   | classIndex   | LSB                                                          |

Unless format=3,7,11, there aren't **both** indexable elements and instance variables. This means unless the number of words of allocation is more than 32766, it can be encoded in the header length field.

For formats >= 17, if the length field=32767, the header word is followed by a word with the index allocation. In this case the total number of words allocated to the object is 2 plus the value of the index allocation word.

If the format=3,7,11, the instance variables are followed by a word with the index allocation. In this case the total number of words allocated to the object is 2 plus the value of the length field (for the instance variables which can't be 32K) plus the value of the index allocation word, with the instance variables immediately following the header, followed by the index allocation word, followed by the indexed elements. 3 and 11 are used (with number of instance variables = 0) in place of 2 and 10 if there are more than 32766 indexable elements.

The remaining format bits encode:
- bit 5: = 32 means that the object is immutable, so any assignments will signal an exception
- bit 6: = 64 means that the object is in a global heap. If a value is being assigned that is a heap object but is not global, that value will have to be promoted to the global heap first because otherwise other threads could not access it. Note that this doesn't differentiate between nursery and young generations, so copying from nursery to young generations will have to start the scan at the beginning of the young generation - rather than the current end of heap as would be used for other promotion copies.
- bit 7: - 128 is currently unused

For BlockClosure, the high 8 bits of the identityHash is the number of parameters for the block. The methods for `value`, `value:`, etc. will check this matches and then dispatch to the block code.

#### Examples `need update`

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

And a format 24 object with 2 instance variables and 3 indexable elements would look like:

| Value               | Description         |
| ------------------- | ------------------- |
| 0001 1Bhh hhhc cccc | length=1, format=27 |
| 6548 6c6c 006f 0000 | Hello               |


![Stats from Pharo Image](../images/Pasted%20image%2020210320170341.png)

