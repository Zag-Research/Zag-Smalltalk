// Implementation of the Symbol type

// This is a rather special implementation that codes Symbols in a particular way to facilitate fast message lookups

// A Symbol is a literal Object with a coding of:
// 0111 1111 1111 1000 0000 0000 0000 000a aaaa 0000 hhhh hhhh hhhh hhhh hhhh hccc
// where:
//     ccc is 110 (6) - the class index for Symbol
//     hhhh hhhh hhhh hhhh hhhh h is the offset in the Symbol table
//     a aaaa is the arity of the symbol - 0-15 are valid (i.e. match existing methods)

use crate::object::*;

#[derive(Copy, Clone)]
pub struct Symbol {
    string : Option<&'static str>,
    left : i32,
    right : i32,
}
const MAX_SYMBOLS : usize = 100;
use std::mem::ManuallyDrop;
const NO_SYMBOL : Symbol = Symbol{string:None,left:0,right:0};
static mut symbolTable : [Symbol;MAX_SYMBOLS] = [NO_SYMBOL;MAX_SYMBOLS];
use std::sync::RwLock;
lazy_static! {
    static ref symbolFree : RwLock<usize> = RwLock::new(0);
    static ref symbolRoot : RwLock<usize> = RwLock::new(0);
}
pub fn intern(string : &'static str) -> Object {
    if let Some(object) = lookupSymbol(string) {
        object
    } else {
        let mut index = symbolFree.write().unwrap();
        let mut pos = *index;
        while let Symbol{string:Some(_), .. } = unsafe{symbolTable[pos]} {
            pos = pos + 1
        };
        *index = pos + 1;
        unsafe {
            std::mem::replace(
                &mut symbolTable[pos],
                Symbol {
                    string: Some(string),
                    left: -1,
                    right: -1,
                },
            )
        };
        insertSymbol(pos)
    }
}
pub fn lookupSymbol(string: &str) -> Option<Object> {
    None
}
pub fn insertSymbol(pos : usize) -> Object {
    nilObject
}
#[cfg(test)]
mod testsSymbol {
    use super::*;
    #[test]
    fn not_found() {
        assert_matches!(lookupSymbol("new string"),None)
    }
    #[test]
    fn add_and_lookup() {
        let abc = intern("abc");
        let def = intern("def");
        assert_matches!(lookupSymbol("abc"),abc);
        assert_matches!(lookupSymbol("def"),def);
        let ghi = String::from("gh");
        let ghi = ghi+"i";
//        let ghi = intern(&ghi);
    }
}
