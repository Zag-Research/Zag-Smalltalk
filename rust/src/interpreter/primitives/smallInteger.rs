use super::*;
pub fn load(add_str:AddStrPrimitive,add_i32:AddI32Primitive) {
    add_i32(7,eq);
}
pub fn eq(thread:&mut Thread,selector:Object) -> FunctionResult {
    let other = thread.pop();
    if thread.top()==other {
        thread.replace(trueObject)
    } else {
        if !other.is_integer() {
            thread.push(other);
            return super::primitive_fail(thread,selector,1,nilObject)
        }
        thread.push(falseObject)
    }
    NormalReturn
}
