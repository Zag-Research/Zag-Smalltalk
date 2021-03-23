use crate::memory::*;
use crate::class::*;
#[derive(Copy, Clone)]
#[repr(C)]  // don't shuffle the fields
struct Method {
}
#[derive(Copy, Clone)]
#[repr(C)]  // don't shuffle the fields
struct MethodMatch {
    hash: i64,
    method: Option<Method>,
}
#[repr(C)]  // don't shuffle the fields
#[derive(Clone)]
struct Dispatch {
    class: Class,
    table: Vec<MethodMatch>,
}

const max_classes : usize = 100;
static dispatchTable : [Option<Dispatch>;max_classes] = [None;max_classes];
use std::sync::RwLock;
lazy_static! {
    static ref dispatchFree : RwLock<usize> = {RwLock::new(0)};
}

pub const classObject : usize = 0;
pub const classBlockClosure : usize = 1;
pub const classUndefinedObject : usize = 2;
pub const classTrue : usize = 3;
pub const classFalse : usize = 4;
pub const classSmallInteger : usize = 5;
pub const classSymbol : usize = 6;
pub const classCharacter : usize = 7;
pub const classString : usize = 8;
pub const classClass : usize = 9;
pub const classMetaclass : usize = 10;
pub const classBehavior : usize = 11;
pub const classFloat : usize = 12;
pub fn addClass(c : &Class, n : usize) {
    let mut index = dispatchFree.write().unwrap();
    let pos = *index;
    *index += 1;
    replaceDispatch(pos,c,n);
}
pub fn replaceDispatch(pos : usize, c : &Class, n : usize) -> Option<Dispatch> {
    let table = Vec::with_capacity(n);
    table.resize(n,MethodMatch{hash:0,method:None});
    unsafe {
        let old = dispatchTable[pos];
        dispatchTable[pos]=Some(Dispatch{class:*c,table:table});
        old
    }
}
fn dispatch(this: Object,symbol: Object,p1: Object,p2: Object) { // `self` is reserved
    
}
