use super::*;
pub fn load(add_str:AddStrPrimitive,add_i32:AddI32Primitive) {
    add_str("yourself",yourself);
    add_str("log:",log_);
}
fn log_(thread:&mut Thread,_:Object) -> FunctionResult {
    println!("{:?}",thread.pop());
    NormalReturn
}
pub fn yourself(thread:&mut Thread,_:Object) -> FunctionResult {
    NormalReturn
}

