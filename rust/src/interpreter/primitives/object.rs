use super::*;
pub fn load(add_str:AddStrPrimitive,add_i32:AddI32Primitive) {
    add_str("yourself prim",yourself);
    add_str("log: prim",log_);
}
fn log_(thread:&mut Thread,_:Object) -> FunctionResult {
    println!("{:?}",thread.pop());
    NormalReturn
}
pub fn yourself(thread:&mut Thread,_:Object) -> FunctionResult {
    NormalReturn
}

