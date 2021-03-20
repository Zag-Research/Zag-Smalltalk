## Mapping
[Virtual Machine Warmup Blows Hot and Cold](https://youtu.be/vLl4GteL9Mw)
[mmap man page](https://www.man7.org/linux/man-pages/man2/mmap.2.html)
[Rusty Runtimes: Building Languages In Rust](https://youtu.be/U3upi-y2pCk)
[Unsafe Rust](https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html)

### Object encoding
The IEEE 754 64-bit binary number is encoded as follows:
	![IEEE 754 Binary-64](Pasted%20image%2020210311212924.png)

When the 11 mantissa bits are all 1s and at least one of the bottom 51 bits is non-zero, then the value is considered Not a Number (NaN), and the low 51 bits are otherwise ignored as a floating point number. Bit 51 should also be 1 to make a quiet (non-signaling) NaN.

So we have 52 bits to play with, as long as the number is non-zero. This lets us encode 2^52 possible values (see the comment at [SpiderMonkey](https://github.com/ricardoquesada/Spidermonkey/blob/4a75ea2543408bd1b2c515aa95901523eeef7858/js/src/gdb/mozilla/jsval.py)). They further point out that on many architectures only the bottom 48 bits are valid as memory addresses, and when used as such, the high 16 bits must be the same as bit 47.

There are several ways to do NaN tagging/encoding. You can choose integers, pointers, or doubles to be naturally encoded and all the others be encoded with some shifting/adding. While integers and pointers are probably more common in most Smalltalk images, leaving doubles as naturally encoded means that vector instructions and/or GPUs could act directly on memory.

So this leaves us with the following encoding based on the **S**ign+**M**antissa and **F**raction bits:

| S+M    | F    | F    | F    | Type                      |
| ------ | ---- | ---- | ---- | ------------------------- |
| 0000   | 0000 | 0000 | 0000 | double  +0                |
| 0000   | xxxx | xxxx | xxxx | double                    |
| 7FF7   | xxxx | xxxx | xxxx | double                    |
| 7FF0   | 0000 | 0000 | 0000 | +inf                      |
| 7FF8   | 0000 | 0000 | 0001 | NaN (generated)           |
| 7FF8   | xxxx | xxxx | xxxx | NaN (unused)              |
| 7FF9   | xxxx | xxxx | xxxx | pointer                   |
| 7FFA/B | xhhh | hhxc | cccc | literals                  |
| 7FFC/D | xxxx | xxxx | xxxx | negative integer   50-bit |
| 7FFE   | 0000 | 0000 | 0000 | integer   0               |
| 7FFE/F | xxxx | xxxx | xxxx | positive integer   50-bit |
| 8000   | 0000 | 0000 | 0000 | double     -0             |
| 8000   | xxxx | xxxx | xxxx | double                    |
| FFEF   | xxxx | xxxx | xxxx | double                    |
| FFF0   | 0000 | 0000 | 0000 | -inf                      |
| FFF8-F | xxxx | xxxx | xxxx | NaN (unused)              |

So, interpreted as an i64, any value that is less than or equal to the generated NaN value is a double. Else, if bit 50 is set, it's an integer and subtracting the integer 0 encoding will give the value. If bit 49 is set it's a pointer. Else it's a literal and subtracting the +inf encoding will give the value^[note that we don't encode 0 or 1 as literal values, so there is no conflict with the generated NaN value or +inf].

Literals are interpreted just like the header word for heap objects. That is, they contain a class index, a hash code, and a length of zero..

### Object in Memory
We are following some of the basic ideas from the [SPUR](http://www.mirandabanda.org/cogblog/2013/09/05/a-spur-gear-for-cog/) encoding for objects on the heap, used by the [OpenSmalltalk VM](https://github.com/OpenSmalltalk).

There are a few significant changes:
1. We are using a pure generational copying collector. This means that we need forwarding pointers during collection. We encode this with the sign-bit of the header word, so a negative object is a forward. Rather than masking the value we store the complement of the pointer as a a forward.
2. `become:` will be implemented with similar forwarding flagging. `become:` will replace both objects headers with forwarding pointer to an object that just contains the original object references and revised header words. When the objects are collected, the references will be updated.
3. References from old-generation to new generation will use forwarding as well (the new object will be copied to the older space, and leave a forwarding pointer behind - note if there is no space for this copy, this could force a collection on an older generation without collecting newer generations)

First we have the object format tag:

0. ??
1. non-indexable objects with inst vars (Point et al) 
2. indexable objects with no inst vars (Array et al)
3. indexable objects with inst vars (MethodContext AdditionalMethodState et al)
4. weak indexable objects with inst vars (WeakArray et al)
5. weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
- 31 zero-sized objects (UndefinedObject True False Character et al)

- 6-8 unused
-  (?) 64-bit indexable 
- 10-11 32-bit indexable
- 12-15 16-bit indexable 
- 16-23 byte indexable 
- 24-31 compiled method

This is the first field in the header-word for an object:

| Bits | What         | Characteristics                                          |
| ---- | ------------ | -------------------------------------------------------- |
| 1    | isForward    | if set, rest of long-word is address of forwarded object |
| 1    | isMarked     |                                                          |
| 1    | isRemembered |                                                          |
| 1    | isGrey       |                                                          |
| 1    | isPinned     |                                                          |
| 1    | isImmutable  |                                                          |
| 1    | unused       |                                                          |
| 5    | format       | (see above)                                              |
| 1    | notIndexable |                                                          |
| 8    | numSlots     |                                                          |
| 21   | identityHash |                                                          |
| 22   | classIndex   | LSB                                                      |

If notIndexable is clear, then there is a second 8-byte header word that contains the allocated size (including the slots) in 8-byte words, and the maximum size in index units (8, 16, 32, or 64 bit).

If numSlots is 0 and notIndexable is set, then there cannot be any instance variables or indexable values, and format will be 31.

### Literals
All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. The currently identified ones are `nil`, `true`, `false`, Characters, and Symbols. All others will need to be heap allocated. Literal values have the identityHash and classIndex in the same location as the header word. The MSB 15 bits are set to 7FFA so that the literal will qualify as a NaN. This leaves 6 bits unused in the literal.^[note that if we were willing to limit to 32 bit integers and 19 bits for dispatch table index that integers could be dispatched the same way as all other literals]

For Characters, the character value is encoded in the hash-code field. This allows 2 million possible character values which covers the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.^[decreasing to 20 bits of hash value would not cover the potential space]

Actually, we don't need so many class values. The only literal types are UndefindObject, True, False, SmallInteger, Symbol, Character, so coding 3 bits would support all those, plus references, plus one spare, so we could index into a smaller dispatch table for those limited types, and support 48 bit integers. Then the reference dispatch could use the 22 bit class index from the header word for a secondary dispatch.

For Symbols, the hash-code field is an index into a table that points to the String representation of the Symbol, but all that really is necessary is in the hash-code. It is used to hash into the selector table on a method dispatch, and is obviously necessary for testing equality. This means that several methods that access the string-ness of the symbol need to be specially coded, and means that unreferenced symbols cannot be reclaimed in the basic garbage collection process. However this seems worth is so that dispatch can proceed without having to follow a pointer. So a separate mechanism must be added to collect unused symbols and the associated Strings.

![Stats from Pharo Image](Pasted%20image%2020210320170341.png)