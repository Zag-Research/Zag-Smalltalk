// Implementation of the Symbol type

// This is a rather special implementation that codes Symbols in a particular way to facilitate fast message lookups

// A Symbol is a literal Object with a coding of:
// 0111 1111 1111 1000 0000 0000 0000 000a aaaa 0000 hhhh hhhh hhhh hhhh hhhh hccc
// where:
//     ccc is 110 (6) - the class index for Symbol
//     hhhh hhhh hhhh hhhh hhhh h is the offset in the Symbol table
//     a aaaa is the arity of the symbol - 0-15 are valid (i.e. match existing methods)

use crate::object::*;

pub struct Symbol {
    string : &'static str, //Option<&'static str>,
    left : u32,
    right : u32,
}
impl Symbol {
    fn set(&mut self,string : &'static str) -> &'static str {
        self.left = 0;
        self.right = 0;
        std::mem::replace(&mut self.string,string)
    }
    #[inline]
    fn isEmpty(&self) -> bool {
        self.string.len() == 0
    }
    #[inline]
    fn compare(&self, string : &str) -> u32 {
        if self.string <= string {
            if self.string == string {
                0
            } else {
                self.right
            }
        } else {
            self.left
        }
    }
}
const NO_SYMBOL : Symbol = Symbol{string:"",left:0,right:0};
use std::sync::{RwLock,RwLockReadGuard,RwLockWriteGuard};
lazy_static! {
    static ref symbolFree : RwLock<usize> = RwLock::new(0);
    static ref symbolRoot : RwLock<usize> = RwLock::new(0);
    static ref symbolTable: RwLock<Vec<Symbol>> = RwLock::new(Vec::new());
}
pub fn intern(string : String) -> Object {
    if let Some(object) = lookupSymbol(&string) {
        object
    } else {
        let mut table = symbolTable.write().unwrap();
        let mut index = symbolFree.write().unwrap();
        let mut pos = *index;
        loop {
            if pos==table.len() || table[pos].isEmpty() {
                break
            } else {
                pos = pos + 1
            }
        };
        *index = pos + 1;
        drop(index);
        if pos==table.len() {table.push(NO_SYMBOL)};
        table[pos].set(Box::leak(string.into_boxed_str()));
        insertSymbol(table,pos)
    }
}
pub fn lookupSymbol(string: &str) -> Option<Object> {
    let root = symbolRoot.read().unwrap();
    let table = symbolTable.read().unwrap();
    lookup(*root as u32,table,string)
}
fn lookup(root:u32,table:RwLockReadGuard<'_, Vec<Symbol>>,string: &str) -> Option<Object> {
    if root==0 {
        None
    } else {
        let pos = table[root as usize].compare(string);
        if pos == 0 {
            Some(symbolOf(string,root as usize))
        } else {
            lookup(pos,table,string)
        }
    }
}
pub fn insertSymbol(table: RwLockWriteGuard<'_, Vec<Symbol>>, pos : usize) -> Object {
    let mut root = symbolRoot.write().unwrap();
    // MORE TO DO
    nilObject
}
#[cfg(test)]
mod testsSymbol {
    use super::*;
    #[test]
    fn not_found() {
        assert_matches!(lookupSymbol("new string"),None)
    }
//    #[test]
    fn add_and_lookup() {
        let abc = intern(String::from("abc"));
        let def = intern(String::from("def"));
        assert_matches!(lookupSymbol("abc"),abc);
        assert_matches!(lookupSymbol("def"),def);
        let ghi = String::from("gh");
        let ghi = ghi+"i";
        let ghi = intern(ghi);
    }
}
