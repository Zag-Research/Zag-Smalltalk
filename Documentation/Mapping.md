## Mapping
[Virtual Machine Warmup Blows Hot and Cold](https://youtu.be/vLl4GteL9Mw)
[mmap man page](https://www.man7.org/linux/man-pages/man2/mmap.2.html)
[Rusty Runtimes: Building Languages In Rust](https://youtu.be/U3upi-y2pCk)
[Unsafe Rust](https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html)

## Object encoding

We use one of 2 object encodings with different properties:
	1. NaN Encoding - which has the advantage of 64-bit floats being encoded with no change. This allows floating-point values to be directly used by other languages and potentially GPUs. This supports 50-bit SmallIntegers.
	2. Modified Spur Encoding - which probably has faster class determination (hence dispatch), and 61-bit SmallIntegers. Most-used 64-bit floats are encoded as immediates, but some very large values (more than 2e77) will be heap-allocated.
Defining a configuration flag allows choosing between these encodings. Once we can run real benchmarks, we will determine which is actually faster for particular workloads.
### NaN Encoding
The IEEE 754 64-bit binary number is encoded as follows:
	![IEEE 754 Binary-64](images/Pasted%20image%2020210311212924.png)
When the 11 mantissa bits are all 1s and at least one of the bottom 51 bits is non-zero, then the value is considered Not a Number (NaN), and the low 52 bits are otherwise ignored as a floating point number.^[Bit 51 could also be 1 to make a quiet (non-signaling) NaN, but it doesn't seem necessary.]

So we have 52 bits to play with, as long as at least one bit is non-zero. This lets us encode 2^52 possible values (see the comment at [SpiderMonkey](https://github.com/ricardoquesada/Spidermonkey/blob/4a75ea2543408bd1b2c515aa95901523eeef7858/js/src/gdb/mozilla/jsval.py)). They further point out that on many architectures only the bottom 48 bits are valid as memory addresses, and when used as such, the high 16 bits must be the same as bit 47.

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
| FFF0      | 0000 | xxxx | xxxx | NaN (unused)                  |
| FFF0      | 0001 | xxxx | xxxx | reserved (tag = Object)       |
| FFF0      | 0002 | xxxx | xxxx | reserved (tag = SmallInteger) |
| FFF0      | 0003 | FFFF | FFFF | UndefinedObject               |
| FFF0      | 0004 | 0000 | 0000 | False                         |
| FFF0      | 0005 | 0000 | 0001 | True                          |
| FFF0      | 0006 | xxxx | xxxx | reserved (tag = Float (double))|
| FFF0      | 0007 | xxxx | xxaa | Symbol                        |
| FFF0      | 0008 | 00xx | xxxx | Character                     |
| FFF0      | yyyy | xxxx | xxxx | (compressed representation for class yyyy)                     |
| FFF1-FFF4 | xxxx | xxxx | xxxx | unused                    |
| FFF5      | xxxx | xxxx | xxxx | heap thunk                    |
| FFF6      | xxxx | xxxx | xxxx | non-local thunk               |
| FFF7      | xxxx | xxxx | xxxx | heap object                   |
| FFF8-F    | xxxx | xxxx | xxxx | SmallInteger                  |
| FFF8      | 0000 | 0000 | 0000 | SmallInteger minVal           |
| FFFC      | 0000 | 0000 | 0000 | SmallInteger 0                |
| FFFF      | FFFF | FFFF | FFFF | SmallInteger maxVal           |

So, interpreted as a u64, any value that is less than or equal to -inf is a double. Else, the bottom 4 bits of the fraction are a class grouping. For group 0, the next 16 bits are a class number so the first 8 classes have (and all classes can have) a compressed representation. 
Groups 4 through 7 have the low 48 bits being the address of an object.
Groups 1 through 6 are all `BlockClosure`s - 1 through 4 being immediate blocks (see [[Mapping#Thunks and Closures]]) and E being a full closure

### Modified Spur Encoding
Spur is the encoding used by [OpenSmalltalkVM](https://github.com/OpenSmalltalk).

Spur uses the [following format](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/):
![[Pasted image 20240115082827.png]]
This provides 61-bit SmallInteger and immediate floats that have an exponent range equivalent to 32-bit floats (approximately 1e-77 to 1e77) with the full 64-bit float mantissa. Determining which of the 4 types is a simple bit-test. Decoding the float is a test for the zero case and then a shift, add a 64-bit constant, and a rotate.

We extend this slightly, by using all 8 possible tag values:
0: Pointer
1: SmallInteger
2: immediate values for the classes Character, Symbol, True, False, UndefinedObject. The next 16 bits are the class number, and the top 32 bits are the information (the character Unicode value or the symbol hash code)
3-7: Float. By using 5 tags we can encode all 64-bit floats less than 2e77. Any value larger than that will be heap allocated. For the vast majority of applications this range will allow all values except `+inf`, `-inf`, and `nan` to be coded as immediate values. Because those values may occur, we save the heap allocation by recognizing them and using a reference to a statically allocated value. Decoding doesn't need to handle zero specially, and is simply: subtract 3, and rotate right 4 bits. Encoding is similarly: rotate left 4 bits; if the resulting low 3 bits are less than 5, add 3; otherwise immediate encoding is not possible.

| Rest | | | | | | |Tag | Type                          |
| -----|---- | --- | -----|-|---|--|---|------------ |
| aaaaaaaa|...|...|...|aaaaaaaa|aaaaaaaa|aaaaa|000 | pointer                    |
| xxxxxxxx|...|...|...|xxxxxxxx|xxxxxxxx|xxxxx|001 | SmallInteger   |
| xxxxxxxx|...|...|...|xxxxxccc|cccccccc|ccccc|010 | intermediates                   |
| eeeeeeee| nnnnnnnn| ...| ...| ...| nnnnnnnn| nnnns|011 | double  |
| eeeeeeee| nnnnnnnn| ...| ...| ...| nnnnnnnn| nnnns|100 | double  |
| eeeeeeee| nnnnnnnn| ...| ...| ...| nnnnnnnn| nnnns|101 | double  |
| eeeeeeee| nnnnnnnn| ...| ...| ...| nnnnnnnn| nnnns|110 | double  |
| eeeeeeee| nnnnnnnn| ...| ...| ...| nnnnnnnn| nnnns|111 | double  |

Because we are using all 8 possible values of the tag field, where the test in Spur for "is a SmallInteger" (or Float) was simply an `and`, using our encoding it requires an `and` followed by a `cmp`.
### Immediates

All zero-sized objects could be encoded in the Object value if they had unique hash values (as otherwise two instances would be identically equal), so need not reside on the heap. About 6% of the classes in a current Pharo image have zero-sized instances, but most have no discernible unique hash values. They also mostly have very few instances, so aren't likely to be usefully optimized. The currently identified ones that do  are `nil`, `true`, `false`, Characters, and Symbols.

| 32 bit Data | | | |Tag | Class                          |
| ---------|-|---|--|---|------------ |
| dddddddd|xxxxx000|00000000|00011|010 | UndefinedObject|
| dddddddd|xxxxx000|00000000|00100|010 | False|
| dddddddd|xxxxx000|00000000|00101|010 | True|
| dddddddd|xxxxx000|00000000|00111|010 | Symbol|
| dddddddd|xxxxx000|00000000|01000|010 | Character|

Immediates are interpreted similarly to a header word for heap objects. That is, they contain a class index and a hash code. The class index is 16 bits and the hash code is 32-45 bits. Most immediate classes use the hash32 value which is the top 32 bits. A few use the full 45 available bits, because extended with 3 zero bits they can be treated as pointer. The encodings for UndefinedObject, True, and False are extremely wasteful of space (because there is only one instance of each, so the hash code is irrelevant), but the efficiency of dispatch and code generation depend on them being immediate values and having separate classes.

#### Class numbers
1. Object - this is reserved for the master superclass. This is also the value returned by `immediate_class` for all heap and thread-local objects. This is an address of an in-memory object, so sign-extending the address is all that is required (at most, for NaN encoding). This gives us 48-bit addresses, which is the maximum for current architectures. (This could be extended by 3 more bits, if required.)
2. SmallInteger - this is reserved for the bit patterns that encode small integers. This isn't encoded in the tag. In the NaN encoding, for integers the low 50 bits of the"hash code" make up the value, so this provides 50-bit integers (-1,125,899,906,842,624 to 1,125,899,906,842,623). The negative integers are first, followed by the positive integers. This allows numerous optimizations of SmallInteger operations (see [[Optimizations]]). In the (modified) Spur encoding, they are 61-bit integers, and different optimizations are possible.
3. UndefinedObject: This encodes the singleton value `nil`.
4. False: The False and True classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation). This encodes the singleton value `false`.
5. True: This encodes the singleton value `true`
6. Float - this is reserved  for the bit patterns that encode double-precision IEEE floating point. In the NaN encoding, this isn't encoded in the tag, but rather with all the values outside the range of literals (where the S+M is less than 0xFFF or the value -inf).
7. Symbol: See [Symbols](Symbols.md) for detailed information on the format.
8. Character: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.

### Thunks and Closures
Full block closures are relatively expensive because most need to be heap allocated. Even though they will typically be discarded quickly, they take dozens of instructions to create, and put pressure on the heap - causing garbage collections to be more frequent. There are many common blocks that don't actually need access to method local variables, `self` or parameters. These can be encoded as immediate values with special subclasses of BlockClosure and obviate the need for heap allocation. 
1. a SmallInteger thunk acts as a niladic BlockClosure that returns a limited range of numeric values, encoded in the hash bits. Hence this supports 32-bit SmallIntegers.
2. a Float thunk similarly supports 32-bit float values.
3. an immediate thunk acts as a niladic BlockClosure that returns any immediate. For modified Spur format, this only supports  the first 8K classes and 32-bit hash values, but this includes all the common values. Examples: `[#foo]`, `[true]`, `[nil]`, [$x].
4. a heap thunk is similar, but it returns a heap object.
5. a non-local thunk simply does a non-local return of one of 8 constant values. The low 48 bits (with the low 3 bits forced to zero) are the address of the Context. The only possible values (encoded in the low 3 bits) are: `[^self]`, `[^true]`, `[^false]`, `[^nil]`, `[^-1]`, `[^0]`, `[^1]`, `[^2]`.
6. all remaining closures are full block closures and are memory objects (they may have been moved to a heap or still reside on the stack), and contain the following fields in order (omitting any unused fields):
	1. the address of the CompiledMethod object that contains various values, and the threaded code implementation (if this is the only field the block has no closure or other variable fields, so the block can be statically allocated - otherwise it needs to be stack allocated (which could be moved to a heap);
	2. the address of the Context if there are any non-local returns (if a closure that references a Context is forced to the heap, that will force that Context to be promoted to the heap);
	3. the address of the value holding block if there were multiple blocks in a method and mutable values needed by this block were allocated in another block (there could conceivably be multiples if there are blocks within blocks);
	4. the values of `self` and any parameters or read-only locals, that are referenced just in this block (this also includes locals that are used solely in the block after being initialized in the method), as well as (if this is a local-holding block) any mutable locals used by this or other blocks.

When a `[`some-value`]` closure is required and some-value is a literal, self, or a parameter to the method (i.e. something that can't be assigned to), runtime code returns either a numeric or immediate thunk (if the value is numeric/immediate and fits), a heap thunk when the value is a heap object, with the low 48 bits referencing the object, or, if the value doesn't fit any of these constraints, then it will fall back to a full closure with 2 fields: the CompiledMethod reference and the value. This applies to `self` or any other runtime value.

There are pre-defined CompiledMethods for some common closures:
1. value:  `[some-value]` - use when value isn't covered by numeric, immediate or heap thunks. CompiledMethod reference and the value are the only things in the closure 
2. id: `[:x|x]` - 
3. return id: `[:x| ^ x]`
4. return value: `[^ value]` - when the value is outside the non-local thunk group. The value is the only thing in the closure other than the method address and the `Context` pointer.

More information on closures can be found at [[Execution#BlockClosures]].

### Object in Memory
This encoding was initially inspired by some of the basic ideas from the [SPUR](http://www.mirandabanda.org/cogblog/2013/09/05/a-spur-gear-for-cog/) encoding for objects on the heap, used by the [OpenSmalltalk VM](https://github.com/OpenSmalltalk).

There are a few significant changes:
1. Three kinds of objects are initially allocated on the stack and may move to the heap. They may be promoted to the heap if: (a) the stack overflows; (b) their reference is stored in an object already on the heap; or (c) their reference is returned from their method. These objects are:
	1. Context - this contains a header, return address (both native and threaded PC), a pointer to the previous context, a pointer to the corresponding CompiledMethod, all the locals, including references to any full BlockClosures and any ClosureData
	2. BlockClosure - this is a full block closure as described in [[Mapping#Thunks and Closures]]
	3. ClosureData - this contains mutable values shared by a combination of BlockClosures and their method
2. We are using a pure generational copying collector for the nursery arenas. This means that we need forwarding pointers during collection. We encode this with a special value for the `length` field of the header word.
3. `become:` will be implemented with similar forwarding.... When the objects are collected, the references will be updated.
4. References from old-generation to new generation will use forwarding as well (the new object will be copied to the older space, and leave a forwarding pointer behind - note if there is no space for this copy, this could force a collection on an older generation without collecting newer generations)

#### Object addresses
All object addresses point to a HeapObject word.  Every object has a HeapObject word at the end (except Contexts on the stack as mentioned below). This allows the global allocator to be used as a Zig Allocator, and can hence be used with any existiing Zig code that requires an allocator. When Zig code frees an allocation, we could return it to the appropriate freelist(s) or simply mark it as unallocated, so it will be garbage collected. Note that pointers to objects on a stack or in nursery arenas should **not** be passed to Zig libraries, because the objects can move. In a few special cases there is also a shadow (header) HeapObject word at the beginning. This is done when the object is variable size (like a CompiledMethod) and mostly used by Zig code so it is efficient to have a pointer to the start of the object. In one special cases (Context) when the object is on the stack there is only the shadow HeapObject word, and if copied to a heap, the object is fully reified (but the shadow remains as the first word of the resulting object).

##### HeapObject word format:
| Bits | What          | Characteristics                        |
| ---- | ------------- | -------------------------------------- |
| 12   | length        | number of long-words besides the footer |
| 4    | age           | number of times object has been copied |
| 8    | format        | see below                              |
| 24   | identityHash  |                                        |
| 16   | classIndex    | LSB                                    |

#### Length
The length field encodes the total size of the heap allocation except for the HeapObject word itself.

There are a number of special length values:
- 4095 - this isn't a header, it would be an object (see [[Mapping#Object encoding]], so it is never used, just reserved.
- 4094 - this is a forwarding pointer, the low 48 bits are the forwarding address. The rest of the original object will be described by a dummy object defined by the previous word (a HeapObject).
- 0-4093 - normal object 
Note that the total heap space for an object can't exceed 4094 words (and maybe smaller, depending on the HeapAllocation size). Anything larger will be allocated as a remote object.
#### Age
The age field encodes where the object is, and the number of times the object has been copied. Every time it is copied to a nursery arena, the count is incremented. When it gets to 6 if it is above a certain size, it will be promoted to the global heap. For global objects, see [MemoryManagement](MemoryManagement.md).

| Value | Meaning | Notes |
| -- | --- | -- |
|  0 | on Stack | only Context, BlockClosure, or ContextData |
| 1-5 | nursery heap | incremented on each copy |
|  6 | nursery heap | will be copied to global heap on next collect |

Not necessarily just age 6 objects will be copied. While a copy is happening, the space occupied by each age is accumulated, so if more space it required, it knows what what ages need to be moved to the global heap.
#### Format
Ignoring the high bit, which says the object is immutable, the object format tag is coded as follows:

| Value | Meaning |
| -- | --- |
| 0 | contains an empty indexable area |
| 1-61 | contains a byte-indexable area of this size |
| 62 | a struct (native Zig object) - ignored by GC |
| 63 | a header |
| 64 | a non-indexable object - no pointers |
| 65 | a non-indexable object - with pointers |
| 66-117 | an object-indexable area of size 1-25 - odd with pointers |
| 118 | a direct-indexed object (no iVars) - no pointers |
| 119 | a direct-indexed object (no iVars) - with pointers |
| 120 | an indexed object - no pointers |
| 121 | an indexed object - with pointers |
| 122 | an external object - no pointers |
| 123 | an external object - with pointers |
| 125 | an external weak object - with pointers |
| 127 | a weak object - with pointers |

The choice of values means that if the value anded with 65 is equal to 65, there are pointers, otherwise not.

The remaining format bit 7 encodes whether  the object is immutable, so any assignments will signal an exception.

For BlockClosure, the high 8 bits of the identityHash is the number of parameters for the block. The methods for `value`, `value:`, etc. will check this matches and then dispatch to the block code. `cull:`, etc. also use this to pare away the right number of parameters.

## Examples:

A simple object like Point with 2 instance variables would look like:

| Value               | Description         |
| ------------------- | ------------------- |
| xxxx xxxx xxxx xxxx | instance variable 1 (x) |
| xxxx xxxx xxxx xxxx | instance variable 2 (y) |
| 0002 40hh hhhh cccc | length=2, format=64  |

An array-like object of 5 elements with 2 instance variables and no pointers would look like:

| Value               | Description                  | 
| ------------------- | ---------------------------- |
| xxxx xxxx xxxx xxxx | instance variable 1          |
| xxxx xxxx xxxx xxxx | instance variable 2                      |
| xxxx xxxx xxxx xxxx | index 1                      |
| xxxx xxxx xxxx xxxx | index 2                      |
| xxxx xxxx xxxx xxxx | index 3                      |
| xxxx xxxx xxxx xxxx | index 4                      |
| xxxx xxxx xxxx xxxx | index 5                      |
| 0007 48hh hhhh cccc | length=7, format=72 |

And an Array of 768 elements (direct index) would look like:

| Value               | Description                      |
| ------------------- | -------------------------------- |
| xxxx xxxx xxxx xxxx | index 1                          |
| xxxx xxxx xxxx xxxx | index 2                          |
| xxxx xxxx xxxx xxxx | index 3                          |
| ...                 | intermediate values              |
| xxxx xxxx xxxx xxxx | index 768                       | 
| 0300 76hh hhhh 000A | length=768, format=118, class=10 (Array) |

And an Array of 2^20 elements would look like:

| Value               | Description                      |
| ------------------- | -------------------------------- |
| 0000 0000 0010 0000 | 2^20                             |
| 0000 aaaa aaaa aaaa | address of index 1 (in big-object area)              |
| 0002 78hh hhhh 000A | length=2, format=120, class=10 (Array) |

And a string would look like:

| Value               | Description         |
| ------------------- | ------------------- |
| 6548 6c6c 006f 0000 | Hello               |
| 0001 05hh hhhh 000B | length=1, format=5, class=11 (String) |

### Statistics from a recent Pharo image
| n | Description |
| -- | --- |
| 19325 | classes |
| 89242 | symbols |
| 127069 | methods |
| 443 | methods in Object |
| 60428 | defined selectors |
| 35490 | unary selectors |
| 16920 | binary or keyword selectors with 1 parameter |
| 5234 | keyword selectors with 2 parameters |
| 12 | classes have 45 or more instance variables |
| 1093 | classes have no instance variables and aren't indexable |
| 143 | of those are abstract |
| 1372 | classes have 1 instance variable and aren't indexable |
| 48 | classes are just indexable |
| 3 | indexable classes have 1 iVar |
| 3 | indexable classes have 2 iVars |
| 13 | indexable classes have 3-6 iVars |

