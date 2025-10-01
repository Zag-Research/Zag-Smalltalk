The way symbols are used is that they have a 24-bit hash that is uniformly distributed, despite there being considerably fewer that 2^24 symbols. This is used in hashing into the dispatch table for a class. The 24 bits can be thought of as a fixed-point decimal with the binary point left of the 24th bit. This make it a uniformly distributed number in the range `[0..1)`. So multiplying by the size of an array (`n`) and shifting out the "fractional" portion gives a number in the range `[0..n)`. This replaces a slow divide with a much faster multiply and shift.

Symbols are allocated sequentially, 1, 2, 3, ... and then a hash function is applied to generate a 24-bit uniformly distributed number. The hash function needs to be invertible so that we can go from a symbol's hash code back to the index into the symbol table.

## Weyl sequences
As of August 2025, the conversion of the index number of a symbol into its hash value uses a [Weyl sequence](https://marc-b-reynolds.github.io/math/2016/02/24/weyl.html) using `1/phi` as the irrational number. This uses an integer multiplication of the index by the 24-bit finite approximation (10368889). While this doesn't produce a "random" number, it produces a uniformly distributed quasirandom number that is used for the "random" tag for a node in the treap that is used to look up strings to convert to symbols.
- More on [Weyl sequences](https://en.wikipedia.org/wiki/Weyl_sequence) [and](https://en.wikipedia.org/wiki/Equidistributed_sequence#Discrepancy) [related](https://en.wikipedia.org/wiki/Pseudorandomness) [concepts](https://en.wikipedia.org/wiki/Equidistribution_theorem) [and](https://ieeexplore.ieee.org/document/4036247)
- This has 2 cycles: 0, and all the rest

This function is invertible by multiplying by the inverse mod 1 of the index above (11764425).
## Triple32 sequences
[Christopher Wellons](https://nullprogram.com/blog/2018/07/31/) did an [exploration](https://github.com/skeeto/hash-prospector) of hashing functions with the properties above. A 24-bit version of this would be theoretically better. But it would be a bit slower (although new symbol generation isn't that common).

## Avalanche Criteria
The [Strict Avalanche Criteria](https://en.wikipedia.org/wiki/Avalanche_effect) is the idea is that any single-bit change to an input has a 50% chance of changing each bit of the output. The principle was first considered by [Claude Shannon](https://en.wikipedia.org/wiki/Claude_Shannon). This is supposedly well tested by [the strict avalanche criterion randomness test](https://doi.org/10.1016/j.matcom.2004.09.001).  However that test actually does a somewhat different test, checking that the hashing of 2 sequential integers generates values that have a Hamming distance of about n/2, but sequential numbers can flip any number of bits. Further it only checks the low bits because it just looks at the first 10,000 integers.

[Questioning the Criteria for Evaluating Non-Cryptographic Hash Functions ](https://cacm.acm.org/practice/questioning-the-criteria-for-evaluating-non-cryptographic-hash-functions/) argues that the Avalanche Criteria may not be as important for non-cryptographic hash applications as it is for cryptographic hashes.

[Golden Ratio Encoder](https://arxiv.org/pdf/0809.1257) is invertible
[On Robustness Properties of Beta Encoders and Golden Ratio Encoders](https://arxiv.org/pdf/0806.1083)

[Bret Mulvey](https://hacker9.com/author/bret/) has written [about hashing](https://web.archive.org/web/20070115062135/http://bretm.home.comcast.net/hash/) including evaluating hash algorithms.
[Hacker9](https://hacker9.com/about/) has a lot of information about cybersecurity
[Here's an article](https://eng.libretexts.org/Courses/Folsom_Lake_College/CISP_430%3A_Data_Structures_(Aljuboori)/07%3A_Hash_Tables/7.02%3A_Choosing_a_good_hash_function) about choosing a good hash function