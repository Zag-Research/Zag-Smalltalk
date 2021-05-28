// Implementation of a treap

use std::cmp::Ordering::{self,Equal,Less,Greater};
use std::fmt::{self,Debug};
use std::cmp::max;
#[derive(Copy,Clone,Debug)]
pub struct Element<K:Copy + PartialEq + Ord + Debug> {
    value: K,
    left: i32,
    right: i32,
}
impl <K:Copy + Ord + Debug> Element<K> {
    fn set(&mut self,value: K) -> K {
        self.left = -1;
        self.right = -1;
        std::mem::replace(&mut self.value,value)
    }
    #[inline]
    fn compare(&self, other: &K) -> Ordering {
        self.value.cmp(other)
    }
}
impl <K:Copy + PartialEq + Ord + Debug> Ord for Element<K> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(&other.value)
    }
}

impl <K:Copy + PartialEq + Ord + Debug> PartialOrd for Element<K> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl <K:Copy + PartialEq + Ord + Debug> Eq for Element<K> {}
impl <K:Copy + PartialEq + Ord + Debug> PartialEq for Element<K> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
pub struct Treap<K:'static + Copy + PartialEq + Ord + Debug> {
    table: Vec<Element<K>>,
    root: i32,
    free: usize,
    init: &'static [K],
    empty: K,
}
#[inline]
fn priority(pos:i32) -> u32 {
    ((pos+1) as u32).wrapping_mul(1999999973_u32)
}
impl <K: Copy + PartialEq + Ord + Debug> Treap<K> {
    fn new(init:&'static [K],empty:K) -> Treap<K> {
        Treap::<K> {
            table: Vec::new(),
            root: -1,
            free: 0,
            init,empty,
        }
    }
    fn at(&self,index: usize) -> K {
        self.table[index].value
    }
    fn lookupElement(&self,key:K) -> Option<u32> {
        self.lookup(self.root,key)
    }
    fn lookup(&self,current:i32,key:K) -> Option<u32> {
        if current<0 {
            None
        } else {
            match self.table[current as usize].compare(&key) {
                Equal => Some(current as u32),
                Less => self.lookup(self.table[current as usize].right,key),
                Greater => self.lookup(self.table[current as usize].left ,key),
            }
        }
    }
    fn checkInitialized(& self) -> bool {
        self.free>=self.init.len()
    }
    fn ensureInitialized(& mut self) -> usize {
        let mut pos = self.free;
        while pos<self.init.len() {
            self.table.push(Element::<K>{value:self.init[pos],left:-1,right:-1});
            self.root = self.insert(pos,self.root);
            pos = pos + 1;
            self.free = pos;
        }
        pos
    }
    pub fn insertElement(& mut self,key: K) -> usize {
        let mut pos = self.ensureInitialized();
        if let Some(result) = self.lookupElement(key) {
            // might have been added while waiting for the write lock or in the init
            return result as usize
        }
        loop {
            if pos>=self.table.len() {
                self.table.push(Element::<K>{value:self.empty,left:-1,right:-1});
                break
            } else if self.empty == self.table[pos].value {
                break
            };
            pos = pos + 1
        };
        self.free = pos + 1;
        self.table[pos].set(key);
        self.root = self.insert(pos,self.root);
        pos
    }
    fn insert(&mut self,target:usize,iroot:i32) -> i32 {
        if iroot<0 {
            return target as i32
        }
        let root = iroot as usize;
        match self.table[target].cmp(&self.table[root]) {
            Equal => panic!("shouldn't be inserting an existing key: {:?}",self.table[target].value),
            Less => {
                self.table[root].left = self.insert(target,self.table[root].left);
                // Fix Heap property if it is violated
                if priority(self.table[root].left) > priority(iroot) {
                    self.rightRotate(root)
                } else {
                    root as i32
                }
            },
            Greater => {
                self.table[root].right = self.insert(target,self.table[root].right);
                // Fix Heap property if it is violated
                if priority(self.table[root].right) > priority(iroot) {
                    self.leftRotate(root)
                } else {
                    root as i32
                }
            },
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
    fn rightRotate(&mut self,y:usize) -> i32 {
        let x = self.table[y].left;
        let t2 = self.table[x as usize].right;
        // Perform rotation
        self.table[x as usize].right = y as i32;
        self.table[y].left = t2;
        x
    }
    fn leftRotate(&mut self,x:usize) -> i32 {
        let y = self.table[x].right;
        let t2 = self.table[y as usize].left;
        // Perform rotation
        self.table[y as usize].left = x as i32;
        self.table[x].right = t2;
        y
    }
    #[cfg(test)]
    fn size(&self) -> usize {
        self.table.len()
    }
    #[cfg(test)]
    fn depth(&self) -> u32 {
        self.internal_depth(self.root)
    }
    #[cfg(test)]
    fn internal_depth(&self,root:i32) -> u32 {
        if root<0 {
            1
        } else {
            max(self.internal_depth(self.table[root as usize].left),
                self.internal_depth(self.table[root as usize].right))+1
        }
    }
}
impl <K:Copy + PartialEq + Ord + Debug> Debug for Treap<K> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        //        for (i,s) in self.table.iter().enumerate() {
        for i in 0..self.table.len() {
            let s=self.table[i];
            writeln!(f,"{}: {:?} {}",i,s,priority(i as i32));
        };
        write!(f,"root:{} free:{}",self.root,self.free)

    }
}
use std::sync::{RwLock,RwLockReadGuard,RwLockWriteGuard};
pub struct LockingTreap<K:'static + Copy + PartialEq + Ord + Debug> {
    treap : RwLock<Treap<K>>,
}
impl <K:Copy + PartialEq + Ord + Debug> LockingTreap<K> {
    pub fn new(init:&'static [K],empty:K) -> LockingTreap<K> {
        LockingTreap::<K> {
            treap: RwLock::new(Treap::new(init,empty)),
        }
    }
    pub fn at(&self,index: usize) -> K {
        {
            let treap = self.treap.read().unwrap();
            if (&*treap).checkInitialized() {
                return (&*treap).at(index)
            }
        }
        let mut treap = self.treap.write().unwrap();
        (&mut *treap).ensureInitialized();
        (&*treap).at(index)
    }
    pub fn intern(&self,key: K) -> usize {
        {
            let treap = self.treap.read().unwrap();
            if (&*treap).checkInitialized() {
                if let Some(result) = (&*treap).lookupElement(key) {
                    return result as usize
                }
            }
        }
        let mut treap = self.treap.write().unwrap();
        (&mut *treap).insertElement(key)
    }
}
#[cfg(test)]
static default_isize_values: &[isize] = &[-100,-99,-98,7];
#[cfg(test)]
fn create_isize_treap() -> LockingTreap<isize> {
    LockingTreap::new(default_isize_values,-1)
}

#[cfg(test)]
lazy_static!{
    static ref v_lazy_static: LockingTreap<isize> = create_isize_treap();
}

#[cfg(test)]
mod testTreap {
    use super::*;
    fn intern<K:Copy + PartialEq + Ord + Debug>(tp:&mut Treap<K>,key: K) -> usize {
//        if let Some(result) = tp.lookupElement(key) {
//            return result as usize
//        } else {
            tp.insertElement(key)
//        }
    }
    fn int_treap() -> Treap<isize> {
        Treap::new(default_isize_values,-1)
    }
    #[test]
    fn empty() {
        let tp = int_treap();
        assert_matches!(tp.depth(),1);
    }
    #[test]
    fn not_found() {
        let tp = int_treap();
        assert_matches!(tp.lookupElement(1),None);
    }
    #[test]
    fn add_some() {
        let mut tp = int_treap();
        assert_matches!(intern(&mut tp,7),3);
        assert_matches!(intern(&mut tp,42),4);
        assert_matches!(intern(&mut tp,17),5);
        assert_matches!(intern(&mut tp,42),4);
    }
    #[test]
    fn add_some_locking() {
        let mut tp = &*v_lazy_static;
        assert_matches!(tp.intern(42),4);
        assert_matches!(tp.intern(17),5);
        assert_matches!(tp.intern(42),4);
    }
    extern crate rand;
    #[test]
    fn add_lots() {
        let mut tp = int_treap();
        for i in 0..50 {
            intern(&mut tp,i);
        }
        assert_matches!(intern(&mut tp,42),45);
        assert_matches!(intern(&mut tp,17),20);
        assert_matches!(intern(&mut tp,42),45);
        assert_matches!(tp.size(),53);
        assert_matches!(tp.depth(),12);
        for i in 50..4092 {
            intern(&mut tp,i);
        }
        assert_matches!(tp.size(),4095);
        assert_matches!(tp.depth(),24); // not as good as the 12 for perfectly balanced, but way better than the 4095 for unbalanced
        for i in 4092..131068 {
            intern(&mut tp,rand::random());
        }
        assert_matches!(tp.size(),131071);
        //assert_matches!(tp.depth(),41 or 42); // versus 17 for perfectly balanced
    }
    type TString = &'static str;
    fn string_treap() -> Treap<TString> {
        Treap::new(&[],"")
    }
    #[test]
    fn emptyS() {
        let tp = string_treap();
        assert_matches!(tp.depth(),1);
    }
    #[test]
    fn not_foundS() {
        let tp = string_treap();
        assert_matches!(tp.lookupElement(""),None);
    }
    #[test]
    fn add_someS() {
        let mut tp = string_treap();
        assert_matches!(intern(&mut tp,"hello"),0);
        assert_matches!(intern(&mut tp,"goodbye"),1);
        assert_matches!(intern(&mut tp,"hello"),0);
    }
}
