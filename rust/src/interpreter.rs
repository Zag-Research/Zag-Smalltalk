use crate::object::*;
//type Object = u64;
type Method = Fn(Object,Object,Object,Object)->Object;
#[derive(Clone)]
struct MethodMatch {
    hash: Object,
    method: Option<&'static Method>,
}
impl MethodMatch {
    fn getMethod(&self,symbol:Object) -> Option<&Method> {
        if self.hash == symbol {
            self.method
        } else {
            None
        }
    }
}
pub struct Dispatch {
    class: Object,
    table: Box<[MethodMatch]>,
}
impl Dispatch {
    fn getMethod(&self,symbol:Object) -> Option<&Method> {
        let hash = symbol.immediateHash()%self.table.len();
        self.table[hash].getMethod(symbol)
    }
}
const MAX_CLASSES: usize = 1000;
use std::mem::ManuallyDrop;
const NO_DISPATCH: ManuallyDrop<Option<Dispatch>> = ManuallyDrop::new(None);
static mut dispatchTable: [ManuallyDrop<Option<Dispatch>>;MAX_CLASSES] = [NO_DISPATCH;MAX_CLASSES];
use std::sync::RwLock;
use once_cell::sync::Lazy;

static dispatchFree: Lazy<RwLock<usize>> = Lazy::new(|| RwLock::new(0));
#[cfg(test)]
static debugClasses: Lazy<bool> = Lazy::new(|| {
    
    true
});

pub fn addClass(c: Object, n: usize) {
    let mut index = dispatchFree.write().unwrap();
    let pos = *index;
    if pos >= MAX_CLASSES {panic!("too many classes")}
    *index = pos + 1;
    replaceDispatch(pos,c,n);
}
pub fn replaceDispatch(pos: usize, c: Object, n: usize) -> Option<Dispatch> {
    let mut table = Vec::with_capacity(n);
    table.resize(n,MethodMatch{hash:zeroObject,method:None});
    unsafe {
        let old = std::mem::replace(
            &mut dispatchTable[pos],
            ManuallyDrop::new(Some(Dispatch {
                class: c,
                table: table.into_boxed_slice(),
            })),
        );
        ManuallyDrop::into_inner(old)
    }
}
fn dispatch(this: Object,symbol: Object,p1: Object,p2: Object,more:Option<&[Object]>) -> Object { // `self` is reserved
    if let Some(disp) = unsafe{&dispatchTable[this.class()].take()} {
        if let Some(method) = disp.getMethod(symbol) {
            method(this,symbol,p1,p2)
        } else {
            panic!("no method found")
        }
    } else {
        panic!("no dispatch found")
    }
}

#[cfg(test)]
mod testsInterpreter {
    use super::*;
    use crate::symbol::intern;
    
    #[test]
    #[should_panic(expected = "no dispatch found")]
    fn dispatch_non_existant_class() {
        dispatch(Object::from(3.14),intern(String::from("foo")),nilObject,nilObject,None);
    }
/*    #[test]
    #[should_panic(expected = "no method found")]
    fn dispatch_non_existant_method() {
        dispatch(Object::from(3.14),intern(String::from("foo")),nilObject,nilObject,None);
    }*/
}
