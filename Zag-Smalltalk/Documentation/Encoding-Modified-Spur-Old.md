### Modified Spur Encoding
Spur is the encoding used by [OpenSmalltalkVM](https://github.com/OpenSmalltalk).

Spur uses the [following format](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/):
![[Pasted image 20240115082827.png]]
This provides 61-bit SmallInteger and immediate floats that have an exponent range equivalent to 32-bit floats (approximately 1e-77 to 1e77) with the full 64-bit float mantissa. Determining which of the 4 types is a simple bit-test. Decoding the float is a test for the zero case and then a shift, add a 64-bit constant, and a rotate.

We extend this slightly, by using all 8 possible tag values:
0: Pointer
1: SmallInteger
2: immediate values for the classes Character, Symbol, True, False, UndefinedObject. The next 16 bits are the class number, and the top 32 bits are the information (the character Unicode value or the symbol hash code). In a few cases, the top 45 bits plus 3 low zero bits provides a 48-bit address allowing capture of closures or contexts.
3-7: Float. By using 5 tags we can encode all 64-bit floats less than 2e77. Any value larger than that will be heap allocated. For the vast majority of applications this range will allow all values except `+inf`, `-inf`, and `nan` to be coded as immediate values. Because those values may occur, we save the heap allocation by recognizing them and using a reference to a statically allocated value. Decoding doesn't need to handle zero specially, and is simply: subtract 3, and rotate right 4 bits. Encoding is similarly: rotate left 4 bits; if the resulting low 3 bits are less than 5, add 3; otherwise immediate encoding is not possible.

| Rest     |          |     |          |     |          |          | Tag      | Type          |
| -------- | -------- | --- | -------- | --- | -------- | -------- | -------- | ------------- |
| aaaaaaaa | ...      | ... | ...      | ... | aaaaaaaa | aaaaaaaa | aaaaa000 | pointer       |
| xxxxxxxx | ...      | ... | ...      | ... | xxxxxxxx | xxxxxxxx | xxxxx001 | SmallInteger  |
| xxxxxxxx | ...      | ... | xxxxxxxx | ... | 00000ccc | cccccccc | ccccc010 | intermediates |
| aaaaaaaa | ...      | ... | ...      | ... | aaaaaccc | cccccccc | ccccc010 | intermediates |
| eeeeeeee | nnnnnnnn | ... | ...      | ... | ...      | nnnnnnnn | nnnnn011 | double        |
| eeeeeeee | nnnnnnnn | ... | ...      | ... | ...      | nnnnnnnn | nnnnn100 | double        |
| eeeeeeee | nnnnnnnn | ... | ...      | ... | ...      | nnnnnnnn | nnnnn101 | double        |
| eeeeeeee | nnnnnnnn | ... | ...      | ... | ...      | nnnnnnnn | nnnnn110 | double        |
| eeeeeeee | nnnnnnnn | ... | ...      | ... | ...      | nnnnnnnn | nnnnn111 | double        |

Because we are using all 8 possible values of the tag field, where the test in Spur for "is a SmallInteger" (or Float) was simply an `and`, using our encoding it requires an `and` followed by a `cmp`.
### Immediates

All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. They also mostly have very few instances, so aren't likely to be usefully optimized. The currently identified ones that do  are `nil`, `true`, `false`, Characters, and Symbols.

| 32 bit Data |          |          |          |       | Tag | Class           |
| ----------- | -------- | -------- | -------- | ----- | --- | --------------- |
| 00000000    | 00000000 | 00000000 | 00000000 | 00011 | 010 | UndefinedObject |
| 00000000    | 00000000 | 00000000 | 00000000 | 00100 | 010 | False           |
| 00000000    | 00000000 | 00000000 | 00000000 | 00101 | 010 | True            |
| dddddddd    | 00000000 | 00000000 | 00000000 | 00111 | 010 | Symbol          |
| dddddddd    | 00000000 | 00000000 | 00000000 | 01000 | 010 | Character       |

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 16 bits and the hash code is 32-45 bits. Most immediate classes use the hash32 value which is the top 32 bits. A few use the full 45 available bits, because extended with 3 zero bits they can be treated as a pointer. The encodings for UndefinedObject, True, and False are extremely wasteful of space (because there is only one instance of each, so the hash code is irrelevant), but the efficiency of dispatch and code generation depend on them being immediate values and having separate classes.
