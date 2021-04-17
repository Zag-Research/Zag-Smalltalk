// Following ideas from http://cliffle.com/p/dangerust/ to support assembler/C interface
// https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html
// https://stackoverflow.com/questions/24759028/how-should-you-do-pointer-arithmetic-in-rust
// https://stackoverflow.com/questions/48795329/what-is-the-difference-between-fromfrom-and-as-in-rust

#![feature(const_mut_refs)]
#![feature(const_raw_ptr_deref)]
#![feature(const_fn_union)]
#![feature(assert_matches)]
#![feature(vec_into_raw_parts)]
#![allow(warnings)] 
#[macro_use]
extern crate lazy_static;

// mod interpreter;

mod object; // defines Object and UndefinedObject Classes
//mod blockClosure;
//mod boolean;
//mod character;
//mod smallInteger;
mod symbol;
//mod string;
// mod class;

use object::*;

//mod minimal;

fn main() {
//   let gc = AllocableRegion::new(gc_main,gc_size);
    let temp = Object::from(-(1<<47));
    let mut temp2 = Object::from((1<<47)-1);
    let temp3 = Object::from(2.0_f64);
    let temp3b = Object::from(0.0625_f64);
    println!("{:?} {:?} {:?}",temp,temp2,temp3);
    println!("{:?} {:?} {:?}",nilObject,trueObject,falseObject);
    //    println!("map address {:?} {:?} {:?} {:?}",temp,temp2,temp2b,temp3);
    println!("{} {:?}",42,Object::from(42));
    println!("{} {:?}",'A',Object::from('A'));
    println!("{} {:?}",true,Object::from(true));
    println!("{} {:?}",false,Object::from(false));
    println!("{} {:?}","nil",nilObject);
    println!("{} {:?}","#foo",symbolOf("foo",19));
    println!("{} {:?}","#value:",symbolOf("value:",1));
    println!("{} {:?}","#value:value:",symbolOf("value:value:",2));
    for i in 40..48 {
        let n = 1<<i;
        let temp = Object::from(n-1);
        let temp2 = Object::from(-n);
        println!("{} : {:?} {:?} {:?} {:?}",i,temp,(n-1) as * const Object,temp2,-n as * const Object);
    }
//    
            
}
