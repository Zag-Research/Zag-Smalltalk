// Implementation of a treap

use std::cmp::Ordering::{self,Equal,Less,Greater};
use std::fmt::{self,Debug};
use once_cell::sync::Lazy;
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
pub struct Treap<K:Copy + PartialEq + Ord + Debug,R> {
    table: Vec<Element<K>>,
    root: i32,
    free: u32,
    init: Box<Fn(u32)->(K,bool)>,
    result: Box<Fn(&K,u32)->R>,
    empty: Box<Fn(&K)->bool>,
}
#[inline]
fn priority(pos:i32) -> u32 {
    ((pos+1) as u32).wrapping_mul(1999999973_u32)
}
impl <K: Copy + PartialEq + Ord + Debug,R> Treap<K,R> {
    fn new(init:Box<Fn(u32)->(K,bool)>,result:Box<Fn(&K,u32)->R>,empty:Box<Fn(&K)->bool>) -> Treap<K,R> {
        Treap::<K,R> {
            table: Vec::new(),
            root: -1,
            free: 0,
            init,result,empty,
        }
    }
    fn lookupElement(&self,key:K) -> Option<R> {
        self.lookup(self.root,key)
    }
    fn lookup(&self,current:i32,key:K) -> Option<R> {
        if current<0 {
            None
        } else {
            match self.table[current as usize].compare(&key) {
                Equal => Some((self.result)(&key,current as u32)),
                Less => self.lookup(self.table[current as usize].right,key),
                Greater => self.lookup(self.table[current as usize].left ,key),
            }
        }
    }
    fn insertElement(& mut self,key: K) -> R {
        let mut pos = self.free;
        loop {
            if pos>=self.table.len() as u32 {
                let (value,done) = (self.init)(pos);
                self.table.push(Element::<K>{value,left:-1,right:-1});
                if done {
                    break
                } else {
                    self.root = self.insert(pos,self.root) as i32;
                }
            } else if (self.empty)(&self.table[pos as usize].value) {
                break
            };
            pos = pos + 1
        };
        self.free = pos + 1;
        self.table[pos as usize].set(key);
        self.root = self.insert(pos,self.root) as i32;
        (self.result)(&self.table[pos as usize].value,pos)
    }
    fn insert(&mut self,target:u32,root:i32) -> i32 {
        if root<0 {
            target as i32
        } else {
            match self.table[target as usize].cmp(&self.table[root as usize]) {
                Equal => panic!("shouldn't be inserting an existing key: {:?}",self.table[target as usize].value),
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
    fn rightRotate(&mut self,y:i32) -> i32 {
        let x = self.table[y as usize].left;
        let t2 = self.table[x as usize].right;
        // Perform rotation
        self.table[x as usize].right = y;
        self.table[y as usize].left = t2;
        x
    }
    fn leftRotate(&mut self,x:i32) -> i32 {
        let y = self.table[x as usize].right;
        let t2 = self.table[y as usize].left;
        // Perform rotation
        self.table[y as usize].left = x;
        self.table[x as usize].right = t2;
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
impl <K:Copy + PartialEq + Ord + Debug,R> Debug for Treap<K,R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        //        for (i,s) in self.table.iter().enumerate() {
        for i in 0..self.table.len() {
            let s=self.table[i];
            write!(f,"{}: {:?} {}\n",i,s,priority(i as i32));
        };
        write!(f,"root:{} free:{}",self.root,self.free)

    }
}
#[cfg(test)]
mod testTreap {
    use super::*;
    fn intern<K:Copy + PartialEq + Ord + Debug,R>(tp:&mut Treap<K,R>,key: K) -> R {
        if let Some(result) = tp.lookupElement(key) {
            return result
        } else {
            tp.insertElement(key)
        }
    }
    fn int_treap() -> Treap<isize,u32> {
        Treap::new(Box::new(|x| (x as isize - 100,x>3)),Box::new(|_,x| x),Box::new(|x| false))
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
        assert_matches!(intern(&mut tp,42),4);
        assert_matches!(intern(&mut tp,17),5);
        assert_matches!(intern(&mut tp,42),4);
    }
    extern crate rand;
    #[test]
    fn add_lots() {
        let mut tp = int_treap();
        for i in 0..50 {
            intern(&mut tp,i);
        }
        //println!("{:?} Depth: {}",tp,tp.depth());
        assert_matches!(intern(&mut tp,42),46);
        assert_matches!(intern(&mut tp,17),21);
        assert_matches!(intern(&mut tp,42),46);
        assert_matches!(tp.size(),54);
        assert_matches!(tp.depth(),12);
        for i in 50..4091 {
            intern(&mut tp,i);
        }
        assert_matches!(tp.size(),4095);
        assert_matches!(tp.depth(),24); // not as good as the 12 for perfectly balanced, but way better than the 4095 for unbalanced
        for i in 4091..131067 {
            intern(&mut tp,rand::random());
        }
        assert_matches!(tp.size(),131071);
        //assert_matches!(tp.depth(),41 or 42); // versus 17 for perfectly balanced
    }
    type TString = &'static str;
    fn string_treap() -> Treap<TString,u32> {
        Treap::new(Box::new(|x| ("",true)),Box::new(|_,x| x),Box::new(|x| false))
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
