// Following ideas from http://cliffle.com/p/dangerust/ to support assembler/C interface
// https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html
// https://stackoverflow.com/questions/24759028/how-should-you-do-pointer-arithmetic-in-rust
// https://stackoverflow.com/questions/48795329/what-is-the-difference-between-fromfrom-and-as-in-rust

#![allow(warnings)] 
extern crate mmap;

#[repr(C)]  // don't shuffle the fields
#[derive(Copy, Clone)]
struct Class {
//    superclass : &'a Class,
//    class : &'a Class,
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
pub mod memory {
    use std::fmt;
    use std::fmt::Debug;
    pub union Object {
        i : i64,
        f : f64,
    }
    const nan_value : i64 = 0x7FF8000000000001;
    const integer_boundary : i64 = 0x7FFC000000000000;
    const integer_zero : i64 = 0x7FFE000000000000;
    impl Object {
        pub fn is_int(&self) -> bool {
            (unsafe{self.i})>=integer_boundary
        }
        pub fn is_double(&self) -> bool {
            (unsafe{self.i})<=nan_value
        }
        pub fn as_i64(&self) -> i64 {
            (unsafe{self.i})-integer_zero
        }
        pub fn as_f64(&self) -> f64 {
            unsafe{self.f}
        }
        pub fn as_ptr(&self) -> * const Object {
            ((unsafe{self.i})-nan_value+1) as * const Object// as &Object
        }
    }
    impl From<i64> for Object {
        fn from(i:i64) -> Self {
            Object {i:i+integer_zero}
        }
    }
    impl From<f64> for Object {
        fn from(f:f64) -> Self {
            Object {f:f}
        }
    }
    impl From<& Object> for Object {
        fn from(o: &Object) -> Self {
            Object {i:(o as *const Object as i64)+nan_value-1}
        }
    }
    impl From<* const Object> for Object {
        fn from(o: * const Object) -> Self {
            Object {i:(o as i64)+nan_value-1}
        }
    }
    impl Debug for Object {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            // The `f` value implements the `Write` trait, which is what the
            // write! macro is expecting. Note that this formatting ignores the
            // various flags provided to format strings.
            if self.is_int() {
                write!(f, "{}", self.as_i64())
            } else if self.is_double() {
                write!(f, "{:e}", self.as_f64())
            } else {
                write!(f, "{:p}", self.as_ptr())
            }
        }
    }
}
use self::memory::*;

fn dispatch(this: Object,p1: Object,p2: Object) { // `self` is reserved
    
}
fn main() {
    let temp = Object::from(-1<<49);
    let mut temp2 = Object::from((1<<49)-1);
    let temp3 = Object::from(2.0_f64);
    let temp3b = Object::from(0.0625_f64);
    let temp4 = Object::from(&temp3);
    println!("map address {:?} {:?} {:?} {:?}",temp,temp2,temp3,temp4);
//    println!("map address {:?} {:?} {:?} {:?}",temp,temp2,temp2b,temp3);
    for i in 50..49 {
        let n = 1<<i;
        let temp = Object::from(n);
        let temp2 = Object::from(-2<<i);
        println!("{} : {} {:?} {:?}",i,n,temp,temp2);
    }
            
}
