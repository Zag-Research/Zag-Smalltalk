// Implementation of the Symbol type

// This is a rather special implementation that codes Symbols in a particular way to facilitate fast message lookups

// A Symbol is a literal Object with a coding of:
// 0111 1111 1111 1000 0000 0000 0000 000a aaaa 0000 hhhh hhhh hhhh hhhh hhhh hccc
// where:
//     ccc is 110 (6) - the class index for Symbol
//     hhhh hhhh hhhh hhhh hhhh h is the offset in the Symbol table
//     a aaaa is the arity of the symbol - 0-15 are valid (i.e. match existing methods)

use crate::object::*;

#[derive(Copy,Clone,Debug)]
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
    fn compare(&self, string : &str) -> usize {
        if self.string <= string {
            if self.string == string {
                0
            } else {
                self.right as usize
            }
        } else {
            self.left as usize
        }
    }
}
pub struct SymbolTable {
    table : Vec<Symbol>,
    root : usize,
    free : usize,
}
impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            table : Vec::new(),
            root : 0,
            free : 0,
        }
    }
    fn lookupSymbol(&self,string: &str) -> Option<Object> {
        self.lookup(self.root,string)
    }
    fn lookup(&self,current:usize,string: &str) -> Option<Object> {
        if current==0 {
            None
        } else {
            let pos = self.table[current].compare(string);
            if pos == 0 {
                Some(symbolOf(string,current))
            } else {
                self.lookup(pos,string)
            }
        }
    }
    fn insertSymbol(& mut self,string: String) -> Object {
        let mut pos = self.free;
        loop {
            if pos==self.table.len() {
                self.table.push(Symbol{string:"",left:0,right:0});
                break
            } else if self.table[pos].isEmpty() {
                break
            } else {
                pos = pos + 1
            }
        };
        self.free = pos + 1;
        self.table[pos].set(Box::leak(string.into_boxed_str()));
        self.insert(pos,self.root);
        symbolOf(self.table[pos].string,pos)
    }
    fn insert(&self,current:usize,root:usize) {
        
    }
}
#[cfg(test)]
mod testsSymbol {
    use super::*;
    #[test]
    fn not_found() {
        let st = SymbolTable::new();
        assert_matches!(st.lookupSymbol("new string"),None);
        assert_matches!(st.lookupSymbol("new string"),None);
    }
    #[test]
    fn add_and_lookup() {
        let mut st = SymbolTable::new();
        let abc = st.insertSymbol(String::from("abc"));
        let def = st.insertSymbol(String::from("def"));
        assert!(abc!=def);
        assert_matches!(st.lookupSymbol("abc"),Some(x) if x==abc);
        assert_matches!(st.lookupSymbol("def"),Some(x) if x==def);
        let ghi = String::from("gh");
        let ghi = ghi+"i";
        let ghi = st.insertSymbol(ghi);
        assert!(abc!=ghi);
        assert!(def!=ghi);
        assert_matches!(st.lookupSymbol("ghi"),Some(x) if x==ghi);
    }
}

use std::sync::{RwLock,RwLockReadGuard,RwLockWriteGuard};
lazy_static! {
    static ref symbolTable: RwLock<SymbolTable> = RwLock::new(SymbolTable::new());
}
pub fn intern(string : String) -> Object {
    if let Some(object) = lookup(&*symbolTable.read().unwrap(),&string) {
        object
    } else {
        let mut table = symbolTable.write().unwrap();
        if let Some(object) = lookup(&*table,&string) { // might have been added while waiting for the write lock
            object
        } else {
            insert(&mut *table,string)
        }
    }
}
#[inline]
fn lookup(table: &SymbolTable,string: &str) -> Option<Object> {
    table.lookupSymbol(string)
}
#[inline]
fn insert(table: & mut SymbolTable,string: String) -> Object {
    table.insertSymbol(string)
}
