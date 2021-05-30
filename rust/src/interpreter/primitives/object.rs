use super::*;
pub fn load(add_str:AddStrPrimitive,add_i32:AddI32Primitive) {
    add_str("yourself",yourself);
}
pub fn yourself(thread:&mut Thread,_:Object) -> FunctionResult {
    NormalReturn
}

