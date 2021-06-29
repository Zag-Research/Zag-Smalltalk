use super::*;
pub mod object;
pub mod smallInteger;
pub mod double;
use std::collections::HashMap;
lazy_static!{
    static ref primitives: RwLock<HashMap<Object,Function>> = RwLock::new(HashMap::new());
}
pub fn primitive_fail(thread:&mut Thread,selector:Object,self_offset:u16,extra:Object) -> FunctionResult {
    thread.push(extra);
    panic!("primitive failed")
}
fn prim_add(k:Object,f:Function) {
    let mut prims = primitives.write().unwrap();
    prims.insert(k,f);
}
fn add_str(s:&'static str,f:Function) {
    prim_add(Object::from(s),f)
}
fn add_i32(i:i32,f:Function) {
    prim_add(Object::from(i as isize),f)
}
pub fn prim_lookup(k:Object) -> Option<Function> {
    {
        let prims = primitives.read().unwrap();
        if !prims.is_empty() {
            if let Some(f) = prims.get(&k) {
                return Some(*f)
            } else {
                return None
            }
        }
    }
    object::load(add_str,add_i32);
    smallInteger::load(add_str,add_i32);
    double::load(add_str,add_i32);
    super::stack::load(add_str,add_i32);
    prim_lookup(k)
}
#[cfg(test)]
mod testsInterpreter {
    use super::*;
    use crate::symbol::intern;

    #[test]
    fn check_prim_lookup() {
//        assert_eq!(prim_lookup(Object::from(1)),Some(crate::interpreter::primitives::smallInteger::addI));
        assert!(matches!(prim_lookup(Object::from(1)),Some(_)));
        assert!(matches!(prim_lookup(Object::from("nop")),Some(_)));
    }

    #[test]
    #[should_panic(expected = "no dispatch found")]
    fn dispatch_non_existant_class() {
        let mut thread:Thread = Default::default();
        thread.push(nilObject);
        dispatch(&mut thread,intern("foo"));
    }
}
