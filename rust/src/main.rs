// Following ideas from http://cliffle.com/p/dangerust/ to support assembler/C interface
// https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html
// https://stackoverflow.com/questions/24759028/how-should-you-do-pointer-arithmetic-in-rust
// https://stackoverflow.com/questions/48795329/what-is-the-difference-between-fromfrom-and-as-in-rust

#![allow(warnings)] 
#[macro_use]
extern crate lazy_static;

mod interpreter;

mod memory; // defines Object and UndefinedObject Classes
//mod blockClosure;
//mod boolean;
//mod character;
//mod smallInteger;
mod symbol;
//mod string;
mod class;

//use memory::*;

//mod minimal;

fn main() {
/*    let gc = AllocableRegion::new(gc_main,gc_size);
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
     }*/
            
}
