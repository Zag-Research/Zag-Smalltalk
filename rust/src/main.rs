// Following ideas from http://cliffle.com/p/dangerust/ to support assembler/C interface
// https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html
// https://stackoverflow.com/questions/24759028/how-should-you-do-pointer-arithmetic-in-rust
// https://stackoverflow.com/questions/48795329/what-is-the-difference-between-fromfrom-and-as-in-rust

#![feature(const_mut_refs)]
#![feature(const_raw_ptr_deref)]
#![feature(const_fn_union)]
#![feature(assert_matches)]
#![feature(vec_into_raw_parts)]
#![feature(thread_local)]
#![allow(warnings)]
#![warn(clippy::default_numeric_fallback)]
#[macro_use]
extern crate static_assertions;
#[macro_use]
extern crate lazy_static;

mod treap;
mod interpreter;

#[macro_use]
mod object; // defines Object and UndefinedObject Classes
//mod blockClosure;
//mod boolean;
//mod character;
//mod smallInteger;
mod symbol;
//mod string;
mod class;
mod memory;

use object::*;
use symbol::intern;
//mod minimal;

fn main() {
/*
    let temp3 = Object::from(2.0_f64);
    let temp3b = Object::from(0.0625_f64);
    println!("{:?} {:?}",temp3,temp3b);
    println!("{:?} {:?} {:?}",nilObject,trueObject,falseObject);
    println!("{:x}",Object::from(-1).as_u48());
//    println!("{:x}",Object::from(u64::MAX).as_u48());
    println!("{} {:?}",42,Object::from(42));
    println!("{} {:?}",1,Object::from(1));
    println!("{} {:?}",'A',Object::from('A'));
    println!("{} {:?}",true,Object::from(true));
    println!("{} {:?}",false,Object::from(false));
    println!("{} {:?}","nil",nilObject);
    println!("{} {:?}","#value",intern("value"));
    println!("{} {:?}","#value:",intern("value:"));
    println!("{} {:?}","#value:value:",intern("value:value:"));
    println!("{} {:?}","#==",intern("=="));
    println!("{} {:?}",42.0,Object::from(42.0));
    for i in 48..50 {
        let n = 1<<(i-1);
        let temp = Object::from(n-1);
        let temp2 = Object::from(-n);
        println!("{} bit SmallInteger: {:?} {:x} {:?} {:x}",i,temp,temp.as_u48(),temp2,temp2.as_u48());
    }
    println!("notice 49 bit wraps around, showing that 48 bit is all we can represent");
// */    

    use interpreter::*;
    //memory::loadHeap();
    let mut thread:Thread = Default::default();
    let classIndex = class::class_index(intern("System"));
    let class = getClass(classIndex);
    thread.push(class);
    let mut method = Method::new(classIndex,0,0,intern("doIt").immediateHash());
    method.instr_with(dispatch,intern("start"));
    method.execute(&mut thread);
}
