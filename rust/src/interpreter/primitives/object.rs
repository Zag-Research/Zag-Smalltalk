use super::*;
pub fn load(add_str:AddStrFunction,add_i32:AddI32Function) {
    add_str("yourself",yourself);
}
pub fn yourself(thread:&mut Thread,_:Object) -> FunctionResult {
    NormalReturn
}

