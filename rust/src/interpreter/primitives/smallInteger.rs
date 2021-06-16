use super::*;
pub fn load(add_str:AddStrPrimitive,add_i32:AddI32Primitive) {
    add_i32(7,eq);
    add_i32(1,addI);
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
pub fn addI(thread:&mut Thread,selector:Object) -> FunctionResult {
    let other = thread.pop();
    if !other.is_integer() {
        thread.push(other);
        return super::primitive_fail(thread,selector,1,nilObject)
    }
    thread.replace(Object::from(thread.top().as_i48()+other.as_i48()));
    NormalReturn
}
