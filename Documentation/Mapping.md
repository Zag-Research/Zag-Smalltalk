# Mapping


## Object encoding

We use one of 2 object encodings with different properties:
	1. NaN Encoding - which has the advantage of 64-bit floats being encoded with no change. This allows floating-point values to be directly used by other languages and potentially GPUs. This supports 50-bit SmallIntegers.
	2. Modified Spur Encoding - which probably has faster class determination (hence dispatch), and 61-bit SmallIntegers. Most-used 64-bit floats are encoded as immediates, but some very large values (more than 2e77) will be heap-allocated.
Both have immediate representations for symbols, characters, booleans, `nil`, and some special closures.

Defining a configuration flag allows choosing between these encodings. Once we can run real benchmarks, we will determine which is actually faster for particular workloads.

### [[Encoding-NaN]]
### [[Encoding-Modified-Spur]]

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