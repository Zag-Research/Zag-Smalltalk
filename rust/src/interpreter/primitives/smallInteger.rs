use super::*;
pub fn load(add_str:AddStrFunction,add_i32:AddI32Function) {
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
