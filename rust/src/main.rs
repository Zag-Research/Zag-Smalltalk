// Following ideas from http://cliffle.com/p/dangerust/ to support assembler/C interface
// https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html
// https://stackoverflow.com/questions/24759028/how-should-you-do-pointer-arithmetic-in-rust
// https://stackoverflow.com/questions/48795329/what-is-the-difference-between-fromfrom-and-as-in-rust

#![allow(warnings)] 

// Classes: 0: Object, 1: UndefinedObject, 2: Boolean, 3: True, 4: False, 5: SmallInteger, 6: Symbol, 7: Class, 8: Metaclass, 9: Behavior

#[repr(C)]  // don't shuffle the fields
#[derive(Copy, Clone)]
struct Class {
//    superclass : &'a Class,
//    class encoded in the header
}
#[derive(Copy, Clone)]
#[repr(C)]  // don't shuffle the fields
struct Method {
}
#[derive(Copy, Clone)]
#[repr(C)]  // don't shuffle the fields
struct MethodMatch {
    hash: i64,
    method: Method,
}
#[repr(C)]  // don't shuffle the fields
struct Dispatch {
    class: Class,
    value: Method, // pointer to the method that implements `value` for this class
    mask: i64,
    table: [Box<[MethodMatch;4]>;32], // neither really fixed size - will access unsafely eventually
}
mod memory;
use memory::*;

fn dispatch(this: Object,p1: Object,p2: Object) { // `self` is reserved
    
}
fn main() {
    let gc = AllocableRegion::new(gc_main,gc_size,1000000).extend();
    let temp = Object::from(-1<<49);
    let mut temp2 = Object::from((1<<49)-1);
    let temp3 = Object::from(2.0_f64);
    let temp3b = Object::from(0.0625_f64);
    let temp4 = Object::from(&nilObject);
    println!("map address {:?} {:?} {:?} {:?}",temp,temp2,temp3,temp4);
//    println!("map address {:?} {:?} {:?} {:?}",temp,temp2,temp2b,temp3);
    for i in 50..49 {
        let n = 1<<i;
        let temp = Object::from(n);
        let temp2 = Object::from(-2<<i);
        println!("{} : {} {:?} {:?}",i,n,temp,temp2);
    }
            
}
