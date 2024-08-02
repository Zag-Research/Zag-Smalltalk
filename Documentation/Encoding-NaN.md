### NaN Encoding
The IEEE 754 64-bit binary number is encoded as follows:
	![IEEE 754 Binary-64](images/Pasted%20image%2020210311212924.png)
When the 11 mantissa bits are all 1s and at least one of the bottom 51 bits is non-zero, then the value is considered Not a Number (NaN), and the low 52 bits are otherwise ignored as a floating point number.^[Bit 51 could also be 1 to make a quiet (non-signaling) NaN, but it doesn't seem necessary.]

So we have 52 bits to play with, as long as at least one bit is non-zero. This lets us encode 2^52 possible values (see the comment at [SpiderMonkey](https://github.com/ricardoquesada/Spidermonkey/blob/4a75ea2543408bd1b2c515aa95901523eeef7858/js/src/gdb/mozilla/jsval.py)). They further point out that on many architectures only the bottom 48 bits are valid as memory addresses, and when used as such, the high 16 bits must be the same as bit 47.

There are several ways to do NaN tagging/encoding. You can choose integers, pointers, or doubles to be naturally encoded and all the others be encoded with some shifting/adding. While integers and pointers are probably more common in most Smalltalk images, leaving doubles as naturally encoded means that vector instructions and/or GPUs could act directly on memory.

So this leaves us with the following encoding based on the **S**ign+**E**xponent and **F**raction bits:

| S+E       | F    | F    | F    | Type                                       |
| --------- | ---- | ---- | ---- | ------------------------------------------ |
| 0000      | 0000 | 0000 | 0000 | double  +0                                 |
| 0000-7FEF | xxxx | xxxx | xxxx | double (positive)                          |
| 7FF0      | 0000 | 0000 | 0000 | +inf                                       |
| 7FF0-F    | xxxx | xxxx | xxxx | NaN (unused)                               |
| 8000      | 0000 | 0000 | 0000 | double     -0                              |
| 8000-FFEF | xxxx | xxxx | xxxx | double (negative)                          |
| FFF0      | 0000 | 0000 | 0000 | -inf                                       |
| FFF0      | 0000 | xxxx | xxxx | NaN (unused)                               |
| FFF0      | 0001 | xxxx | xxxx | reserved (tag = Object)                    |
| FFF0      | 0002 | xxxx | xxxx | reserved (tag = SmallInteger)              |
| FFF0      | 0003 | FFFF | FFFF | UndefinedObject                            |
| FFF0      | 0004 | 0000 | 0000 | False                                      |
| FFF0      | 0005 | 0000 | 0001 | True                                       |
| FFF0      | 0006 | xxxx | xxxx | reserved (tag = Float (double))            |
| FFF0      | 0007 | xxxx | xxaa | Symbol                                     |
| FFF0      | 0008 | 00xx | xxxx | Character                                  |
| FFF0      | yyyy | xxxx | xxxx | (compressed representation for class yyyy) |
| FFF1-FFF4 | xxxx | xxxx | xxxx | unused                                     |
| FFF5      | xxxx | xxxx | xxxx | heap thunk                                 |
| FFF6      | xxxx | xxxx | xxxx | non-local thunk                            |
| FFF7      | xxxx | xxxx | xxxx | heap object                                |
| FFF8-F    | xxxx | xxxx | xxxx | SmallInteger                               |
| FFF8      | 0000 | 0000 | 0000 | SmallInteger minVal                        |
| FFFC      | 0000 | 0000 | 0000 | SmallInteger 0                             |
| FFFF      | FFFF | FFFF | FFFF | SmallInteger maxVal                        |

So, interpreted as a u64, any value that is less than or equal to -inf is a double. Else, the bottom 4 bits of the fraction are a class grouping. For group 0, the next 16 bits are a class number so the first 8 classes have (and all classes can have) a compressed representation. 
Groups 5 through 7 have the low 48 bits being the address of an object.
