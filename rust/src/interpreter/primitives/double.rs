use super::*;
pub fn load(add_str:AddStrPrimitive,add_i32:AddI32Primitive) {
    add_i32(41,add);
    add_i32(47,eq);
}
pub fn add(thread:&mut Thread,selector:Object) -> FunctionResult {
    if !thread.top().is_double() { return super::primitive_fail(thread,selector,1,nilObject) }
    let result = Object::from(thread.pop().as_f64()+thread.pop().as_f64());
    thread.push(result);
    NormalReturn
}
pub fn eq(thread:&mut Thread,selector:Object) -> FunctionResult {
    let other = thread.pop();
    if thread.top()==other {
        thread.replace(trueObject)
    } else {
        if !other.is_double() {
            thread.push(other);
            return super::primitive_fail(thread,selector,1,nilObject)
        }
        thread.push(falseObject)
    }
    NormalReturn
}
