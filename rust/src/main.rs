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
extern crate once_cell;

mod interpreter;

mod object; // defines Object and UndefinedObject Classes
//mod blockClosure;
//mod boolean;
//mod character;
//mod smallInteger;
mod symbol;
//mod string;
// mod class;

use object::*;
use symbol::intern;
//mod minimal;

fn main() {
/*
    let temp = Object::from(-(1<<47));
    let mut temp2 = Object::from((1<<47)-1);
    let temp3 = Object::from(2.0_f64);
    let temp3b = Object::from(0.0625_f64);
    println!("{:?} {:?} {:?} {:?}",temp,temp2,temp3,temp3b);
    println!("{:?} {:?} {:?}",nilObject,trueObject,falseObject);
    println!("{} {:?}",42,Object::from(42));
    println!("{} {:?}",1,Object::from(1));
    println!("{} {:?}",'A',Object::from('A'));
    println!("{} {:?}",true,Object::from(true));
    println!("{} {:?}",false,Object::from(false));
    println!("{} {:?}","nil",nilObject);
    for s in &["valueWithArguments:","cull:","cull:cull:","cull:cull:cull:","cull:cull:cull:cull:",
               "value","value:","value:value:","value:value:value:","value:value:value:value:",
               "yourself","==","~~","~=","=","+","-","*","size"] {
        intern(s.to_string());
    }
    println!("{} {:?}","#value",intern(String::from("value")));
    println!("{} {:?}","#value:",intern(String::from("value:")));
    println!("{} {:?}","#value:value:",intern(String::from("value:value:")));
    println!("{} {:?}","#==",intern(String::from("==")));
    println!("{} {:?}",42.0,Object::from(42.0));
    for i in 38..48 {
        let n = 1<<i;
        let temp = Object::from(n-1);
        let temp2 = Object::from(-n);
        println!("{}: {:?} {:x} {:?} {:x}",i,temp,(n-1)<<3,temp2,-n<<3);
    }
*/    
//    let gc = AllocableRegion::new(gc_main,gc_size);
    let system = intern(String::from("System"));
//    let system_class = 
}
