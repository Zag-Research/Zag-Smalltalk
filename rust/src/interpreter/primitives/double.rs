use super::*;
pub fn load(add_str:AddStrFunction,add_i32:AddI32Function) {
    add_i32(41,add);
    add_i32(47,eq);
}
pub fn add(thread:&mut Thread,selector:Object) -> FunctionResult {
    if !thread.top().is_double() { return super::primitive_fail(thread,selector,1,nilObject) }
    thread.push(Object::from(thread.pop().as_double()+thread.pop().as_double()));
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
