# Mapping


## Object encoding

We use one of 2 object encodings with different properties:
	1. NaN Encoding - which has the advantage of 64-bit floats being encoded with no change. This allows floating-point values to be directly used by other languages and potentially GPUs. This supports 50-bit SmallIntegers.
	2. Modified Spur Encoding - which probably has faster class determination (hence dispatch), and 61-bit SmallIntegers. Most-used 64-bit floats are encoded as immediates, but some very large values (more than 2e77) will be heap-allocated.
Both have immediate representations for symbols, characters, booleans, `nil`, and some special closures.

Defining a configuration flag allows choosing between these encodings. Once we can run real benchmarks, we will determine which is actually faster for particular workloads.

### [[Encoding-NaN]]
### [[Encoding-Modified-Spur]]
#### Class numbers
1. Object - this is reserved for the master superclass. This is also the value returned by `immediate_class` for all heap and thread-local objects. This is an address of an in-memory object, so sign-extending the address is all that is required (at most, for NaN encoding). This gives us 48-bit addresses, which is the maximum for current architectures. (This could be extended by 3 more bits, if required.)
2. SmallInteger - this is reserved for the bit patterns that encode small integers. This isn't encoded in the tag. In the NaN encoding, for integers the low 50 bits of the"hash code" make up the value, so this provides 50-bit integers (-562,949,953,421,312 to 562,949,953,421,311). The negative integers are first, followed by the positive integers. This allows numerous optimizations of SmallInteger operations (see [[Optimizations]]). In the (modified) Spur encoding, they are 61-bit integers, and different optimizations are possible.
3. UndefinedObject: This encodes the singleton value `nil`.
4. False: The False and True classes only differ by 1 bit so they can be tested easily if that is appropriate (in code generation). This encodes the singleton value `false`.
5. True: This encodes the singleton value `true`
6. Float - this is reserved  for the bit patterns that encode double-precision IEEE floating point. In the NaN encoding, this isn't encoded in the tag, but rather with all the values outside the range of literals (where the S+M is less than 0xFFF or the value -inf).
7. Symbol: See [Symbols](Symbols.md) for detailed information on the format.
8. Character: The hash code contains the full Unicode value for the character. This allows orders of magnitude more possible character values than the 830,606 reserved code points as of [Unicode v13](https://www.unicode.org/versions/stats/charcountv13_0.html) and even the 1,112,064 possible Unicode code points.

### Thunks and Closures
Full block closures are relatively expensive because most need to be heap allocated. Even though they will typically be discarded quickly, they take dozens of instructions to create, and put pressure on the heap - causing garbage collections to be more frequent. There are many common blocks that don't actually need access to method local variables, `self` or parameters. These can be encoded as immediate values with special subclasses of BlockClosure and obviate the need for heap allocation. 
1. a SmallInteger thunk acts as a niladic BlockClosure that returns a limited range of numeric values, encoded in the hash bits. Hence this supports 32/45-bit SmallIntegers.
2. a Float thunk similarly supports 32/45-bit float values.
3. an immediate thunk acts as a niladic BlockClosure that returns any immediate. For modified Spur format, this only supports  the first 8K classes and 32-bit hash values, but this includes all the common values. Examples: `[#foo]`, `[true]`, `[nil]`, `[$x]`.
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
1. Two kinds of objects are initially allocated on the stack and may move to the heap. They may be promoted to the heap if: (a) the stack overflows; (b) their reference is stored in an object already on the heap; or (c) their reference is returned from their method. These objects are:
	1. Context - this contains a header, return address (both native and threaded PC), a pointer to the previous context, a pointer to the corresponding CompiledMethod, all the locals, including references to any full BlockClosures and any ClosureData
	2. BlockClosure - this is a full block closure as described in [[Mapping#Thunks and Closures]]
2. We are using a pure generational copying collector for the nursery arenas. This means that we need forwarding pointers during collection. We encode this with a special value for the `length` field of the header word.
3. `become:` will be implemented with similar forwarding.... When the objects are collected, the references will be updated.
4. References from old-generation to new generation will use forwarding as well (the new object will be copied to the older space, and leave a forwarding pointer behind - note if there is no space for this copy, this could force a collection on an older generation without collecting newer generations)

#### Object addresses
All object addresses point to a HeapObject word.  Every object has a HeapHeader word at the beginning. This allows the global allocator to be used as a Zig Allocator, and can hence be used with any existiing Zig code that requires an allocator. When Zig code frees an allocation, we could return it to the appropriate freelist(s) or simply mark it as unallocated, so it will be garbage collected. Note that pointers to objects on a stack or in nursery arenas should **not** be passed to Zig libraries, because the objects can move.

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
- 4094 - this is a forwarding pointer, the low 48 bits are the forwarding address. The rest of the original object will be described by a dummy object defined by the next word (a HeapHeader).
- 0-4093 - normal object 
Note that the total heap space for an object (including any footer fields) can't exceed 4094 words (and maybe smaller, depending on the HeapAllocation size). Anything larger will be allocated as an external object.
#### Age
The age field encodes where the object is, and the number of times the object has been copied. Every time it is copied to a nursery arena, the count is incremented. When it gets to 6 if it is above a certain size, it will be promoted to the global heap.

| Value | Meaning      | Notes                                           |
| ----- | ------------ | ----------------------------------------------- |
| 0     | on Stack     | only Context, or BlockClosure                   |
| 1-5   | nursery heap | incremented on each copy                        |
| 6     | nursery heap | will be promoted to global heap on next collect |
| 7-15  | global heap  | see [[MemoryManagement#Object age fields]]      |

Not necessarily just age 6 objects will be copied. While a copy is happening, the space occupied by each age is accumulated, so if more space is required, it knows what ages need to be promoted to the global heap.
#### Format
The format field encodes whether there are instance variables, indexable portions, pointers. Ignoring the high bit, which says the object is immutable, the object format tag is coded as follows:

| Value | Name                          | Meaning                                      |
| ----- | ----------------------------- | -------------------------------------------- |
| 0     | immutableSizeZero             | any empty indexable area                     |
| 1-109 |                               | size 1-109 byte arrays                       |
| x6e   | indexedStruct                 | allocated Zig struct, not an Object          |
| x6f   | externalStruct                | allocated Zig struct, not an Object          |
| x70   | notIndexable                  | just instance variables                      |
| x71   | directIndexed                 | just index variables                         |
| x72   | indexed                       | has instvars+indexed Objects                 |
| x73   | indexedNonObject              | has indexed 8/16/32/64-bit non-Objects       |
| x74   | external                      | has instvars+indexed Objects                 |
| x75   | externalNonObject             | has indexed 8/16/32/64-bit non-Objects       |
| x76   | free                          |                                              |
| x77   | special                       | special format: Context, CompiledMethod      |
| x78   | notIndexableWithPointers      |                                              |
| x79   | directIndexedWithPointers     |                                              |
| x7a   | indexedWithPointers           |                                              |
| x7b   | indexedNonObjectWithPointers  | no pointers in array portion                 |
| x7c   | externalWithPointers          |                                              |
| x7d   | externalNonObjectWithPointers | no pointers in array portion                 |
| x7e   | externalWeakWithPointers      | only this and following have weak queue link |
| x7f   | weakWithPointers              |                                              |

The choice of values means that if the value is greater than x76, there are pointers, otherwise not. All the "WithPointers" versions are a fixed offset from their regular versions, so can be converted easily.

The remaining format bit 7 encodes whether  the object is mutable, so 0 means that any assignments will signal an exception.

For BlockClosure, the low 8 bits of the identityHash is the number of parameters for the block. The methods for `value`, `value:`, etc. will check this matches and then dispatch to the block code. `cull:`, etc. also use this to pare away the right number of parameters.

## Examples:

In the following, for the header word, the `a` is the age field, the `hhhhhh` is the hash value, and the `cccc` is the class number.

A simple object like Point with 2 instance variables would look like:

| Value               | Description         |
| ------------------- | ------------------- |
| 002a 70hh hhhh cccc | length=2, format=notIndexable  |
| xxxx xxxx xxxx xxxx | instance variable 1 (x) |
| xxxx xxxx xxxx xxxx | instance variable 2 (y) |

An array-like object of 5 elements with 2 instance variables and no pointers would look like:

| Value               | Description                  | 
| ------------------- | ---------------------------- |
| 009a 72hh hhhh cccc | length=9, format=indexed |
| xxxx xxxx xxxx xxxx | instance variable 1          |
| xxxx xxxx xxxx xxxx | instance variable 2                      |
| xxxx xxxx xxxx xxxx | index 1                      |
| xxxx xxxx xxxx xxxx | index 2                      |
| xxxx xxxx xxxx xxxx | index 3                      |
| xxxx xxxx xxxx xxxx | index 4                      |
| xxxx xxxx xxxx xxxx | index 5                      |
| aaaa aaaa aaaa aaaa | address of index 1 |
| 0000 0000 0000 0005 | indexable size = 5 |

And an Array of 768 elements (direct index) would look like:

| Value               | Description                      |
| ------------------- | -------------------------------- |
| 300a 71hh hhhh 000A | length=768, format=directIndexed, class=10 (Array) |
| xxxx xxxx xxxx xxxx | index 1                          |
| xxxx xxxx xxxx xxxx | index 2                          |
| xxxx xxxx xxxx xxxx | index 3                          |
| ...                 | intermediate values              |
| xxxx xxxx xxxx xxxx | index 768                       | 

And an Array of 2^20 elements would look like:

| Value               | Description                      |
| ------------------- | -------------------------------- |
| 002a 78hh hhhh 000A | length=2, format=120, class=10 (Array) |
| 0000 aaaa aaaa aaaa | address of index 1 (in big-object area)              |
| 0000 0000 0010 0000 | 2^20                             |

And a string would look like:

| Value               | Description         |
| ------------------- | ------------------- |
| 001a 05hh hhhh 000B | length=1, format=5, class=11 (String) |
| 6548 6c6c 006f 0000 | Hello               |

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

##  References
[Virtual Machine Warmup Blows Hot and Cold](https://youtu.be/vLl4GteL9Mw)
[mmap man page](https://www.man7.org/linux/man-pages/man2/mmap.2.html)
[Rusty Runtimes: Building Languages In Rust](https://youtu.be/U3upi-y2pCk)
[Unsafe Rust](https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html)