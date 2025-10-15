# Symbols
One of the ways that this interpreter runs fast is that it encodes Symbols in a particular way.

Symbols are literals - meaning that they are immediate object like small integers, booleans, and characters.

The hash code contains 2 sub-fields: the low 24 bits are a hashed index into the symbol table, then the next 4 bits are the arity of the symbol. This supports 16 million symbols (a typical Pharo image has about 90,000 symbols). For Symbols, the hash-code index field is an index into a table that points to the internal representation of the Symbol, including the characters that make it up, but all that really is necessary is in the hash-code. It is used to hash into the selector table on a method dispatch, and is obviously necessary for testing equality. This means that several methods that access the string-ness of the symbol need to be specially coded, and means that unreferenced symbols cannot be reclaimed in the basic garbage collection process. However this seems worth it so that dispatch can proceed without having to follow a pointer. So a separate mechanism must be added to collect unused symbols and the associated Strings.

Also an internal data structure (an auto-[treap](https://en.wikipedia.org/wiki/Treap)) must be used for `String>>#asSymbol`. While a hash table could conceivably be faster, an auto-treap allows the actual symbol entries to be at arbitrary and unchanging positions, which is required by the way we handle symbols.

In a method that has a primitive, if the primitive fails, it executes the code that follows in the method.

Rather than storing the symbol table index directly in the symbol, we store a hashed value. In particular we multiply the index with the integer value of 2^24/phi (10368889) in 24-bit fixed arithmetic. This concatenated with the tag for `Symbol` (0x71) - i.e. the low 32 bits of the symbol value - provide the multiplier to calculate the index into the dispatch table for the class. This hashing is invertible, so to get back the index, we simply do a multiplication of the inverse of that number in 24-bit fixed arithmetic (11764425), but this is a rare operation (when we need to find the string value of a symbol) so it is better to maintain the hashed value in the symbol.
## Interning of Symbols
Part of the normal efficiency of Symbols is that they are unique, so that equality tests are simply an identity equality. This means that `asSymbol` has to guarantee that uniqueness. Creation of symbols is a fairly rare event, so it is Zig code that is behind a write lock. Even checking if a string is a symbol is rare, so that is behind a read lock.

In Pharo, the String `asSymbol` checks for the existence in a Dictionary, but AST Smalltalk goes directly to interning the symbol. The sequence is:
1. Grab a read lock on the table.
2. Look through the binary search tree for the string. If found, return the Symbol with the hash code from the location.
3. If not found, release the read lock and acquire the write lock on the table.
4. Search again, in case the symbol was allocated while we were waiting for the write lock.
5. If not found, allocate the next free location. It is rare for symbols to be deleted, so normally this is an additional location, but if symbols have been deleted, it will use the first element in the linked free list.
6. Reorder the tree to put this new location into the correct position in the BST.
7. Return the Symbol with the hash code from the new location.

