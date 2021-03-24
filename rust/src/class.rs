use crate::memory::Object;
#[repr(C)]  // don't shuffle the fields
#[derive(Copy, Clone)]
pub struct Class {
    superclass : Object,
//    class encoded in the header
}
