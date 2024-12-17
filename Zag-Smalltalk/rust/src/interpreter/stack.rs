use super::*;
pub fn load(add_str:AddStrPrimitive,add_i32:AddI32Primitive) {
    add_i32(110,eq);
    add_i32(1,add);
    add_str("nop",nop);
    add_str("dup",dup);
}
pub fn dup(thread:&mut Thread,_:Object) -> FunctionResult {
    thread.push(thread.top());
    NormalReturn
}
pub fn drop(thread:&mut Thread,_:Object) -> FunctionResult {
    thread.pop_to(thread.offset(1));
    NormalReturn
}
pub fn pre_return(_:&mut Thread,_:Object) -> FunctionResult {
    ReturnIsNext
}
pub fn constant(thread:&mut Thread,object:Object) -> FunctionResult {
    thread.push(object);
    NormalReturn
}
pub fn eq_constant(thread:&mut Thread,object:Object) -> FunctionResult {
    if thread.pop()==object {
        thread.push(trueObject)
    } else {
        thread.push(falseObject)
    }
    NormalReturn
}
pub fn eq_class(thread:&mut Thread,object:Object) -> FunctionResult {
    if thread.pop().class()==object.as_u16() {
        thread.push(trueObject)
    } else {
        thread.push(falseObject)
    }
    NormalReturn
}
pub fn eq(thread:&mut Thread,_:Object) -> FunctionResult {
    if thread.pop()==thread.pop() {
        thread.push(trueObject)
    } else {
        thread.push(falseObject)
    }
    NormalReturn
}
pub fn br_true(thread:&mut Thread,object:Object) -> FunctionResult {
    if thread.pop()==trueObject {
        JumpTo(object.as_u48() as u32)
    } else {
        NormalReturn
    }
}
pub fn nop(thread:&mut Thread,_:Object) -> FunctionResult {
    NormalReturn
}
pub fn add(thread:&mut Thread,selector:Object) -> FunctionResult {
    if !thread.top().is_integer() { return super::primitives::primitive_fail(thread,selector,1,nilObject) }
    let result = Object::from(thread.pop().as_i48()+thread.pop().as_i48());
    thread.push(result);
    NormalReturn
}
pub fn restack(thread:&mut Thread,fields:Object) -> FunctionResult {
    let mut fields = fields.as_u48();
    let discard = fields&255;
    let mut keep = Vec::new();
    fields = fields>>8;
    while fields>0 {
        keep.push(thread.atOffset(fields as u32 & 31));
        fields = fields >> 5
    }
    thread.discard(discard&127);
    thread.append(&mut keep);
    if discard>127 {
        ReturnIsNext
    } else {
        NormalReturn
    }
}
#[cfg(test)]
mod testMethod {
    use super::*;
    use crate::symbol::intern;
    #[test]
    fn restack_mask() {
        assert_eq!(restack_mask!(10=>3,1,5),Object::from((5<<18)|(1<<13)|(3<<8)|10));
    }
    #[test]
    fn noop() {
        let mut thread:Thread = Default::default();
        thread.push(Object::from(42));
        let mut method = Method::new(classObject,0,0,intern("yourself").immediateHash(),0);
        method.instr(stack::nop);
        assert_eq!(method.execute(&mut thread),NormalReturn);
        assert_eq!(thread.top(),Object::from(42));
    }
    #[test]
    fn addSmallInt() {
        let mut thread:Thread = Default::default();
        thread.push_i48(25);
        thread.push_i48(17);
        let mut method = Method::new(classSmallInteger,1,0,intern("+").immediateHash(),0);
        method.instr(stack::add);
        assert_eq!(method.execute(&mut thread),NormalReturn);
        assert_eq!(thread.top().as_i48(),42);
    }
    #[test]
    fn stackManipulation() {
        let mut thread:Thread = Default::default();
        for i in 1..6 {
            thread.push_i48(i);
        }
        let mut method = Method::new(classSmallInteger,4,0,intern("foo").immediateHash(),0);
        method.instr(stack::dup);
        method.instr_i48(stack::constant,3);
        method.instr(stack::add);
        method.instr_with(stack::restack,restack_mask!(5=>1,3,4));
        assert_eq!(method.execute(&mut thread),NormalReturn);
        assert_eq!(thread.pop().as_i48(),3);
        assert_eq!(thread.pop().as_i48(),4);
        assert_eq!(thread.pop().as_i48(),8);
        assert_eq!(thread.pop().as_i48(),1);
    }
}
