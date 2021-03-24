// Implementation of the Symbol type

// This is a rather special implementation that codes Symbols in a particular way to facilitate fast message lookups

// A Symbol is a literal Object with a coding of:
// 0111 1111 1111 1000 0000 0000 0000 000a aaaa 0000 hhhh hhhh hhhh hhhh hhhh hccc
// where:
//     ccc is 110 (6) - the class index for Symbol
//     hhhh hhhh hhhh hhhh hhhh h is the offset in the Symbol table
//     a aaaa is the arity of the symbol - 0-15 are valid (i.e. match existing methods)

pub struct Symbol {
    string : &str,
    left,right : usize,
}
const MAX_SYMBOLS : usize = 100;
use std::mem::ManuallyDrop;
const NO_SYMBOL : ManuallyDrop<Option<Symbol>> = ManuallyDrop::new(None);
static mut symbolTable : [ManuallyDrop<Option<Symbol>>;MAX_SYMBOLS] = [NO_SYMBOL;MAX_SYMBOLS];
use std::sync::RwLock;
lazy_static! {
    static ref symbolFree : RwLock<usize> = RwLock::new(0);
    static ref symbolRoot : RwLock<usize> = RwLock::new(0);
}
pub fn addSymbol(c : Object, n : usize) -> Object {
    let mut index = symbolFree.write().unwrap();
    let pos = *index;
    *index += 1;
    replaceSymbol(pos,c,n);
}
pub fn replaceSymbol(pos : usize, s: &str) -> Option<Symbol> {
    let mut table = Vec::with_capacity(n);
    table.resize(n,MethodMatch{hash:0,method:None});
    unsafe {
        let old = std::mem::replace(
            &mut symbolTable[pos],
            ManuallyDrop::new(Some(Symbol {
                symbol: c,
            })),
        );
        ManuallyDrop::into_inner(old)
    }
}
