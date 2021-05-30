use super::*;
pub mod object;
pub mod smallInteger;
pub mod double;
use std::collections::HashMap;
#[derive(Eq,PartialEq,Hash)]
enum Key {
    S(&'static str),
    I(i32),
}
lazy_static!{
    static ref primitives: RwLock<HashMap<Key,Function>> = RwLock::new(HashMap::new());
}
pub fn primitive_fail(thread:&mut Thread,selector:Object,self_offset:u16,extra:Object) -> FunctionResult {
    thread.push(extra);
    panic!("primitive failed")
}
fn prim_add(k:Key,f:Function) {
    let mut prims = primitives.write().unwrap();
    prims.insert(k,f);
}
fn add_str(s:&'static str,f:Function) {
    prim_add(Key::S(s),f)
}
fn add_i32(i:i32,f:Function) {
    prim_add(Key::I(i),f)
}
fn prim_lookup(k:Key) -> Option<Function> {
    {
        let prims = primitives.read().unwrap();
        if !prims.is_empty() {
            if let Some(k) = prims.get(&k) {
                return Some(*k)
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
    #[should_panic(expected = "no dispatch found")]
    fn dispatch_non_existant_class() {
        let mut thread:Thread = Default::default();
        dispatch(&mut thread,intern("foo"));
    }
}
