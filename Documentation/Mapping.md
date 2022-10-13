## Mapping
[Virtual Machine Warmup Blows Hot and Cold](https://youtu.be/vLl4GteL9Mw)
[mmap man page](https://www.man7.org/linux/man-pages/man2/mmap.2.html)
[Rusty Runtimes: Building Languages In Rust](https://youtu.be/U3upi-y2pCk)
[Unsafe Rust](https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html)

### Object encoding
The IEEE 754 64-bit binary number is encoded as follows:
	![IEEE 754 Binary-64](../images/Pasted%20image%2020210311212924.png)

When the 11 mantissa bits are all 1s and at least one of the bottom 51 bits is non-zero, then the value is considered Not a Number (NaN), and the low 52 bits are otherwise ignored as a floating point number.^[Bit 51 could also be 1 to make a quiet (non-signaling) NaN, but it doesn't seem necessary.]

So we have 52 bits to play with, as long as the number is non-zero. This lets us encode 2^52 possible values (see the comment at [SpiderMonkey](https://github.com/ricardoquesada/Spidermonkey/blob/4a75ea2543408bd1b2c515aa95901523eeef7858/js/src/gdb/mozilla/jsval.py)). They further point out that on many architectures only the bottom 48 bits are valid as memory addresses, and when used as such, the high 16 bits must be the same as bit 47.

There are several ways to do NaN tagging/encoding. You can choose integers, pointers, or doubles to be naturally encoded and all the others be encoded with some shifting/adding. While integers and pointers are probably more common in most Smalltalk images, leaving doubles as naturally encoded means that vector instructions and/or GPUs could act directly on memory.

So this leaves us with the following encoding based on the **S**ign+**E**xponent and **F**raction bits:

| S+E       | F    | F    | F    | Type                          |
| --------- | ---- | ---- | ---- | ----------------------------- |
| 0000      | 0000 | 0000 | 0000 | double  +0                    |
| 0000-7FEF | xxxx | xxxx | xxxx | double (positive)             |
| 7FF0      | 0000 | 0000 | 0000 | +inf                          |
| 7FF0-F    | xxxx | xxxx | xxxx | NaN (unused)                  |
| 8000      | 0000 | 0000 | 0000 | double     -0                 |
| 8000-FFEF | xxxx | xxxx | xxxx | double (negative)             |
| FFF0      | 0000 | 0000 | 0000 | -inf                          |
| FFF0-3    | xxxx | xxxx | xxxx | NaN (unused)                  |
| FFF6      | 0000 | xxxx | xxxx | reserved (tag = unused)       |
| FFF6      | 0001 | xxxx | xxxx | reserved (tag = Object)       |
| FFF6      | 0002 | xxxx | xxxx | reserved (tag = SmallInteger) |
| FFF6      | 0003 | xxxx | xxxx | reserved (tag = Float (double))       |
| FFF6      | 0004 | 0001 | 0000 | False                         |
| FFF6      | 0005 | 0010 | 0001 | True                          |
| FFF6      | 0006 | 0100 | 0002 | UndefinedObject               |
| FFF6      | 0007 | aaxx | xxxx | Symbol                        |
| FFF6      | 0008 | 00xx | xxxx | Character                     |
| FFF7      | xxxx | xxxx | xxxx | heap object                   |
| FFF8-F    | xxxx | xxxx | xxxx | SmallInteger                  |
| FFF8      | 0000 | 0000 | 0000 | SmallInteger minVal           |
| FFFC      | 0000 | 0000 | 0000 | SmallInteger 0                |
| FFFF      | FFFF | FFFF | FFFF | SmallInteger maxVal           |

So, interpreted as a u64, any value that is less than or equal to -inf is a double. Else, the top 4 bits of the fraction are a class grouping. For group 7, the next 16 bits are a class number so the first 8 classes have (and all classes can have) a compressed representation. There is also room in the FFF0-FFF3 groups for encodings of new classes that need more than 32 auxiliary (hash) bits.

### Immediates
All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. The currently identified ones that do  are `nil`, `true`, `false`, Integers, Floats, Characters, and Symbols.

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 16 bits and the hash code is 32-51 bits. The encodings for UndefinedObject, True, and False are extremely wasteful of space (because there is only one instance of each, so the hash code is irrelevant), but the efficiency of dispatch and code generation depend on them being immediate values and having separate classes.

#### Tag values
1. Object - this is reserved for the master superclass. This is also the value returned by `immediate_class` for all heap and thread-local objects. This is an address of an in-memory object, so sign-extending the address is all that is required. This gives us 48-bit addresses, which is the maximum for current architectures. (This could be extended by 3 more bits, if required.)
2. SmallInteger - this is reserved for the bit patterns that encode small integers. This isn't encoded in the tag. For integers the low 51 bits of the"hash code" make up the value, so this provides 51-bit integers (-1,125,899,906,842,624 to 1,125,899,906,842,623). The negative integers are first, with a 0 in the high bit, followed by the positive integers with a 1 in the high bit. This allows numerous optimizations of SmallInteger operations (see [[Optimizations]]).
3. Float - this is reserved  for the bit patterns that encode double-precision IEEE floating point. This isn't encoded in the tag, but rather with all the values outside the range of literals (where the S+M is less than 0xFFF or the value -inf).
4. False: The False and True classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation). This only encodes the single value `false`.
5. True: This only encodes the single value `true`
6. UndefinedObject: This encodes the value `nil` with all zero hash code.
7. Symbol: See [Symbols](Symbols.md) for detailed information on the format.
8. Character: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.

### Object in Memory
We are following some of the basic ideas from the [SPUR](http://www.mirandabanda.org/cogblog/2013/09/05/a-spur-gear-for-cog/) encoding for objects on the heap, used by the [OpenSmalltalk VM](https://github.com/OpenSmalltalk).

There are a few significant changes:
1. We are using a pure generational copying collector. This means that we need forwarding pointers during collection. We encode this with a special value for the `length` field of the header word.
2. `become:` will be implemented with similar forwarding flagging. `become:` will replace both objects headers with forwarding pointer to an object that just contains the original object references and revised header words. When the objects are collected, the references will be updated.
3. References from old-generation to new generation will use forwarding as well (the new object will be copied to the older space, and leave a forwarding pointer behind - note if there is no space for this copy, this could force a collection on an older generation without collecting newer generations)

This coding is used for heap and thread-local objects. They are treated exactly the same, except that thread-local are not collected or moved.

First we have the object format tag. The bits code the following:
- bit 0-3: encode indexable fields
	- 0: no indexable fields
	- 1: 64-bit indexable - native words (DoubleWordArray,DoubleArray,) or Objects (Array)
	- 2-3: 32-bit indexable - low bit encodes unused half-words at end (WordArray, IntegerArray, FloatArray, WideString)
	- 4-7: 16-bit indexable - low 2 bits encode unused quarter-words at end (DoubleByteArray)
	- 8-15: byte indexable - low 3 bits encode unused bytes at end (ByteArray, String)
- bit 4: encode indexable have pointers
	-  0: no indexable pointers
	- 16: indexable pointers
- bit 5-6: encode instance variables
	-  0: no instance variable
	- 32: instance variables - no pointers
	- 64: weak (implying instance variables) - pointers - even if there aren't, because weak values are rare, and they only exist to hold pointers
	- 96: instance variables have pointers
- bit 7: = 1 says the value is immutable

Therefore, only the following values currently have meaning:
- 32,64: non-indexable objects with inst vars (Association et al) 
- 1-17: indexable objects with no inst vars
- 32-49,65-81: indexable objects with inst vars (MethodContext AdditionalMethodState et al)
- 96: weak non-indexable objects with inst vars  (Ephemeron)
- 97-113: weak indexable objects with inst vars (WeakArray et al)

Format anded with 80 = 0 declares no pointers, so GC doesn't look through them for pointers. Things are initially created as their pointer-free version but change to their pointer-containing version if a pointer is stored in them. i.e. 64 is ored if a pointer is stored into an instance variable, and 16 is ored if a pointer is stored into an indexed field (additionally, the pointee may need to be promoted to the general arena). During garbage collection, if no reference is found during the scan, they revert to the pointer-free version (i.e. bit 4 or 6 is reset).

If there are both instVars and indexable fields, the length field is the number of instVars which are followed by a word containing the size of the indexable portion, which follows. Weak objects are rare enough that we don't bother to handle cases with no instance variables or no indexable values separately.

If there aren't both  instVars and indexable fields, the size is determined by the length field. The only difference between instVars and indexables is whether `at:`, `size`, etc. should work or give an error.

If the array length is >= 2048 (whether in the length field or the additional size word), the values are preceded by a size which is the number of additional words. 

This is the header-word for an object:

| Bits | What          | Characteristics                        |
| ---- | ------------- | -------------------------------------- |
| 12   | length        | number of long-words beyond the header |
| 4    | age           | number of times object has been copied |
| 1    | isImmutable        | \                                      |
| 1    |       | !                                      |
| 1    |    | !                                      |
| 1    |      | !                                      |
| 1    |  | + format(see above)                    |
| 1    |         | !                                      |
| 1    |    | !                                      |
| 1    |    | /                                      |
| 24   | identityHash  |                                        |
| 16   | classIndex    | LSB                                    |

Unless format=3,7,11, there aren't **both** indexable elements and instance variables. This means unless the number of words of allocation is more than 8189, it can be encoded in the header length field.

For formats >= 17, if the length field=4094, the header word is followed by 3 words with the index allocation, a pointer to the data page, and a link for a list through all the large-allocation objects. In this case the total number of words allocated to the object is 3.

If the format=3,7,11, the instance variables are followed by a word with the index allocation. In this case the total number of words allocated to the object is 2 or 4 plus the value of the length field (for the instance variables which can't be 4K) plus the value of the index allocation word, with the instance variables immediately following the header, followed by the index allocation word, followed by the indexed elements. 3 and 11 are used (with number of instance variables = 0) in place of 2 and 10 if there are more than 4093 indexable elements. If the index allocation is greater than or equal to 4094, then the indexable elements are allocated on a separate data page, and replaced in the object with a pointer to that page and a link for the list of large-allocation objects..

The remaining format bit 7 encodes whether  the object is immutable, so any assignments will signal an exception.

The age field encodes the number of times the object has been copied. Stack objects (only Contexts) will always have an age of 0. Nursery heap objects have an age of 1. Every time it is copied to a teen arena, the count is incremented. When it gets to 8, it will be promoted to the global heap, so an age of greater than 7 indicates that the object is global. For global objects, the low 3 bits of the age are available for marks for the mark and sweep collection (see[[MemoryManagement]]).

For BlockClosure, the high 8 bits of the identityHash is the number of parameters for the block. The methods for `value`, `value:`, etc. will check this matches and then dispatch to the block code. `cull:`, etc. also use this to pare away the right number of parameters.

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

