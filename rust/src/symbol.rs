// Implementation of the Symbol type

// This is a rather special implementation that codes Symbols in a particular way to facilitate fast message lookups

// A Symbol is a literal Object with a coding of:
// 0111 1111 1111 1000 0000 0000 0000 000a aaaa 0000 0hhh hhhh hhhh hhhh hhhh hccc
// where:
//     ccc is 110 (6) - the class index for Symbol
//     hhh hhhh hhhh hhhh hhhh h is the offset in the Symbol table
//     a aaaa is the arity of the symbol - 0-15 are valid (i.e. match existing methods)

use crate::object::*;
use crate::treap::LockingTreap;
use std::sync::Mutex;

static first_symbols: &[StaticStr]= &[
    "valueWithArguments:", "cull:", "cull:cull:", "cull:cull:cull:", "cull:cull:cull:cull:", /* need to be first 5 symbols so that short-circuit on dispatch works */
    "value", "value:", "value:value:", "value:value:value:", "value:value:value:value:", /* need to be the next 5 symbols */
    "Object", "BlockClosure", "False", "True",
    "UndefinedObject", "SmallInteger", "Symbol", "Character",
    "Float", "Array", "String", "Class", "Metaclass",
    "Behavior", "Method", "System",
    "yourself", "==", "~~", "~=", "=", "+", "-", "*", "size",
];
lazy_static!{
    static ref symbolTable: LockingTreap<StaticStr> = LockingTreap::new(first_symbols,"");
}
pub fn intern(string: StaticStr) -> Object {
    symbolOf(&string,symbolTable.intern(&string) as usize)
}
lazy_static!{
    static ref endSymbol: Mutex<u32> = Mutex::new(u32::MAX);
}
/* used to generate symbols for metaclasses, so will never be many of them */
pub fn unique_symbol() -> Object {
    let mut end = endSymbol.lock().unwrap();
    let n = *end;
    *end = n - 1;
    uncheckedSymbolOf(n)
}
pub fn str_of(obj:Object) -> StaticStr {
    symbolTable.at(obj.hash())
}

#[cfg(test)]
mod testSymbol {
    use super::*;
    #[test]
    fn preload() {
        for s in first_symbols {
            assert_eq!(str_of(intern(&s)),*s);
        }
    }
    #[test]
    fn object() {
        assert_eq!(intern("Object"),symbolOf("Object",10));
        assert_eq!(format!("{:?}",intern("Object")),"Object");
    }
}
