// Implementation of the Symbol type

// This is a rather special implementation that codes Symbols in a particular way to facilitate fast message lookups

// A Symbol is a literal Object with a coding of:
// 0111 1111 1111 1000 0000 0000 0000 000a aaaa 0000 hhhh hhhh hhhh hhhh hhhh hccc
// where:
//     ccc is 110 (6) - the class index for Symbol
//     hhhh hhhh hhhh hhhh hhhh h is the offset in the Symbol table
//     a aaaa is the arity of the symbol - 0-15 are valid (i.e. match existing methods)

use crate::object::*;
use std::cmp::Ordering::{self,Equal,Less,Greater};
use std::fmt::{self,Debug};
use once_cell::sync::Lazy;
#[derive(Copy,Clone,Debug)]
pub struct Symbol {
    string: &'static str, //Option<&'static str>,
    left: u32,
    right: u32,
}
impl Symbol {
    fn set(&mut self,string: &'static str) -> &'static str {
        self.left = 0;
        self.right = 0;
        std::mem::replace(&mut self.string,string)
    }
    #[inline]
    fn isEmpty(&self) -> bool {
        self.string.len() == 0
    }
    #[inline]
    fn compare(&self, string: &str) -> Ordering {
        self.string.cmp(string)
    }
}
impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(&other.string)
    }
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for Symbol {}
impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}
pub struct SymbolTable {
    table: Vec<Symbol>,
    root: u32,
    free: u32,
}
#[inline]
fn priority(pos:u32) -> u32 {
    pos.wrapping_mul(1999999973_u32)
}
impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            table: Vec::new(),
            root: 0,
            free: 0,
        }
    }
    fn lookupSymbol(&self,string: &str) -> Option<Object> {
        self.lookup(self.root,string)
    }
    fn lookup(&self,current:u32,string: &str) -> Option<Object> {
        if current==0 {
            None
        } else {
            match self.table[current as usize].compare(string) {
                Equal => Some(symbolOf(string,current as usize)),
                Less => self.lookup(self.table[current as usize].right,string),
                Greater => self.lookup(self.table[current as usize].left ,string),
            }
        }
    }
    fn insertSymbol(& mut self,string: String) -> Object {
        let mut pos = self.free;
        loop {
            if pos>=self.table.len() as u32 {
                self.table.push(Symbol{string:"",left:0,right:0});
                if pos>0 {break}
            } else if self.table[pos as usize].isEmpty() {
                break
            };
            pos = pos + 1
        };
        self.free = pos + 1;
        self.table[pos as usize].set(Box::leak(string.into_boxed_str()));
        self.root = self.insert(pos,self.root);
        symbolOf(self.table[pos as usize].string,pos as usize)
    }
    fn insert(&mut self,target:u32,root:u32) -> u32 {
        if root==0 {
            target
        } else {
            match self.table[target as usize].cmp(&self.table[root as usize]) {
                Equal => panic!("shouldn't be inserting an existing key: {}",self.table[target as usize].string),
                Less => {
                    self.table[root as usize].left = self.insert(target,self.table[root as usize].left);
                    // Fix Heap property if it is violated
                    if priority(self.table[root as usize].left) > priority(root) {
                        self.rightRotate(root)
                    } else {
                        root
                    }
                },
                Greater => {
                    self.table[root as usize].right = self.insert(target,self.table[root as usize].right);
                    // Fix Heap property if it is violated
                    if priority(self.table[root as usize].right) > priority(root) {
                        self.leftRotate(root)
                    } else {
                        root
                    }
                },
            }
        }
    }
    /* T1, T2 and T3 are subtrees of the tree rooted with y
     * (on left side) or x (on right side)
     *           y                               x
     *          / \     Right Rotation          /  \
     *         x   T3   – – – – – – – >        T1   y
     *        / \       < - - - - - - -            / \
     *       T1  T2     Left Rotation            T2  T3
     */
    fn rightRotate(&mut self,y:u32) -> u32 {
        let x = self.table[y as usize].left;
        let t2 = self.table[x as usize].right;
        // Perform rotation
        self.table[x as usize].right = y;
        self.table[y as usize].left = t2;
        x
    }
    fn leftRotate(&mut self,x:u32) -> u32 {
        let y = self.table[x as usize].right;
        let t2 = self.table[y as usize].left;
        // Perform rotation
        self.table[y as usize].left = x;
        self.table[x as usize].right = t2;
        y
    }
}
impl Debug for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        //        for (i,s) in self.table.iter().enumerate() {
        for i in 0..self.table.len() {
            let s=self.table[i];
            write!(f,"{}: {:?} {}\n",i,s,priority(i as u32));
        };
        write!(f,"root:{} free:{}",self.root,self.free)

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
        assert_matches!(st.lookupSymbol("abc"),Some(x) if x==abc);
        let def = st.insertSymbol(String::from("def"));
        assert_matches!(st.lookupSymbol("def"),Some(x) if x==def);
        assert!(abc!=def);
        assert_matches!(st.lookupSymbol("abc"),Some(x) if x==abc);
        assert_matches!(st.lookupSymbol("def"),Some(x) if x==def);
        let ghi = String::from("gh");
        let ghi = ghi+"i";
        let ghi = st.insertSymbol(ghi);
        st.insertSymbol(String::from("jkl"));
        st.insertSymbol(String::from("mno"));
        st.insertSymbol(String::from("pqr"));
        st.insertSymbol(String::from("stu"));
        st.insertSymbol(String::from("vwx"));
        st.insertSymbol(String::from("yza"));
//        println!("{:?}",st);
        assert!(abc!=ghi);
        assert!(def!=ghi);
        assert_matches!(st.lookupSymbol("ghi"),Some(x) if x==ghi);
    }
}

use std::sync::{RwLock,RwLockReadGuard,RwLockWriteGuard};
static symbolTable: Lazy<RwLock<SymbolTable>> = Lazy::new(|| RwLock::new(SymbolTable::new()));
pub fn intern(string: String) -> Object {
    fn lookup(table: &SymbolTable,string: &str) -> Option<Object> {
        table.lookupSymbol(string)
    }
    fn insert(table: & mut SymbolTable,string: String) -> Object {
        table.insertSymbol(string)
    }
    {
        if let Some(object) = lookup(&*symbolTable.read().unwrap(),&string) {
            return object
        }
    }
    let mut table = symbolTable.write().unwrap();
    if let Some(object) = lookup(&*table,&string) { // might have been added while waiting for the write lock
        object
    } else {
        insert(&mut *table,string)
    }
}
