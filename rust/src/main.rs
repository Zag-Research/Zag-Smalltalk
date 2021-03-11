// Following ideas from http://cliffle.com/p/dangerust/ to support assembler/C interface
// https://doc.rust-lang.org/nightly/book/ch19-01-unsafe-rust.html
// https://stackoverflow.com/questions/24759028/how-should-you-do-pointer-arithmetic-in-rust
// https://stackoverflow.com/questions/48795329/what-is-the-difference-between-fromfrom-and-as-in-rust
#[repr(C)]  // don't shuffle the fields
struct Class {
}
#[repr(C)]  // don't shuffle the fields
struct Method {
}
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
struct Object {
}
fn dispatch(this: Object,p1: Object,p2: Object) { // `self` is reerved
    
}
fn main() {
    println!("Hello, world!");
}
