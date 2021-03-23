use std::sync::Weak;
#[repr(C)]  // don't shuffle the fields
#[derive(Clone)]
pub struct Class {
    superclass : Weak<Class>,
//    class encoded in the header
}
