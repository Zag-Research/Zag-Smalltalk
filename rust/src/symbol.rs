// Implementation of the Symbol type

// This is a rather special implementation that codes Symbols in a particular way to facilitate fast message lookups

// A Symbol is a literal Object with a coding of:
// 0111 1111 1111 1000 0000 0000 000a aaaa 0000 0000 hhhh hhhh hhhh hhhh hhhh hccc
// where:
//     ccc is 110 (6) - the class index for Symbol
