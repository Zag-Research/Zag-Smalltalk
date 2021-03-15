use std::mem;
use std::fmt;
use std::fmt::Debug;
#[derive(Copy, Clone)]
pub union Object {
    i : i64,
    f : f64,
}
const nan_value : i64 = 0x7FF8000000000001;
const integer_boundary : i64 = 0x7FFC000000000000;
const integer_zero : i64 = 0x7FFE000000000000;
impl Object {
    #[inline]
    pub fn is_int(&self) -> bool {
        (unsafe{self.i})>=integer_boundary
    }
    #[inline]
    pub fn is_double(&self) -> bool {
        (unsafe{self.i})<=nan_value
    }
    #[inline]
    pub fn is_heap_object(&self) -> bool {
        !self.is_double() && !self.is_int()
    }
    #[inline]
    pub fn as_i64(&self) -> i64 {
        (unsafe{self.i})-integer_zero
    }
    #[inline]
    pub fn as_f64(&self) -> f64 {
        unsafe{self.f}
    }
    #[inline]
    pub fn as_ptr(&self) -> * mut HeapObject {
        ((unsafe{self.i})-nan_value+1) as * mut HeapObject
    }
}
impl From<i64> for Object {
    fn from(i:i64) -> Self {
        Object {i:i+integer_zero}
    }
}
impl From<f64> for Object {
    fn from(f:f64) -> Self {
        Object {f:f}
    }
}
impl From<& HeapObject> for Object {
    fn from(o: &HeapObject) -> Self {
        Object {i:(o as *const HeapObject as i64)+nan_value-1}
    }
}
impl From<* const HeapObject> for Object {
    fn from(o: * const HeapObject) -> Self {
        Object {i:(o as i64)+nan_value-1}
    }
}
impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        if self.is_int() {
            write!(f, "{}", self.as_i64())
        } else if self.is_double() {
            write!(f, "{:e}", self.as_f64())
        } else {
            write!(f, "{:p}", self.as_ptr())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn convert_object() {
        //assert_eq!(Object::from(-1<<49).as_i64(),-1<<49);
        //assert_eq!(Object::from(1<<49).as_i64(),1<<49);
        assert_eq!(Object::from(-1).as_i64(),-1);
        assert_eq!(Object::from(1).as_i64(),1);
        assert_eq!(Object::from(2.0_f64).as_f64(),2.0_f64);
        let temp3 = Object::from(2.0_f64);
        assert_eq!(Object::from(&nilObject).as_ptr() as * const HeapObject,&nilObject as * const HeapObject);
    }
}

union HeapHeader {
    all : i64,
    bytes : [u8;8],  // x86_64: 0 is lsb 7 is msb
    words : [u32;2], // x86_64: 0 is low word, 1 is high
}
impl Debug for HeapHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        write!(f, "HH")
    }
}

#[derive(Debug)]
pub struct HeapObject {
    header : HeapHeader,
    fields : [Object;0], // typically many more than 1, but will be accessed as unsafe
}
impl PartialEq for HeapObject {
    fn eq(&self,other:&HeapObject) -> bool {
        self==other
    }
}

pub static nilObject : HeapObject = HeapObject{header:HeapHeader{all:1+(0<<32)},fields:[]};
pub static trueObject : HeapObject = HeapObject{header:HeapHeader{all:3+(1<<32)},fields:[]};
pub static falseObject : HeapObject = HeapObject{header:HeapHeader{all:4+(2<<32)},fields:[]};
const mask47 : u64 = 0x7fffffffffff;
const mask22 : u32 = 0x3fffff;
const mask5 : u8 = 0x1f;
impl HeapObject {
    #[inline]
    #[cfg(target_arch="x86_64")] // order of fields may differ
    pub fn size(&self) -> usize {
        unsafe{self.header.bytes[3] as usize}
    }
    #[inline]
    #[cfg(target_arch="x86_64")]
    pub fn size_set(&mut self,new:usize) {
        unsafe{self.header.bytes[3]=new as u8}
    }
    #[inline]
    #[cfg(target_arch="x86_64")]
    pub fn identityHash(&self) -> usize {
        ((unsafe{self.header.words[1]}) & mask22) as usize
    }
    #[inline]
    #[cfg(target_arch="x86_64")]
    pub fn classIndex(&self) -> usize {
        ((unsafe{self.header.words[0]}) & mask22) as usize
    }
    #[inline]
    pub fn at(&self,index:usize) -> Object {
        unsafe{self.fields[index]}
    }
    #[inline]
    pub fn at_put(&mut self,index:usize,value:Object) -> Object {
        unsafe{self.fields[index]=value};
        value
    }
}
pub struct AllocableRegion {
    base : * mut Object,
    end : * mut Object,
    current : * mut Object,
    base_allocation : isize,
    increment : isize,
}
const min_page_size : isize = 16384;
impl AllocableRegion {
    pub fn new(address: usize,size:isize,increment:isize) -> Self {
        let address = address as * mut Object;
        let size = (size+min_page_size-1)&(-min_page_size)/(mem::size_of::<Object>() as isize);
        let increment = (increment+min_page_size-1)&(-min_page_size)/(mem::size_of::<Object>() as isize);
        AllocableRegion {
            base : address,
            end : unsafe{address.offset(size)},
            current : address,
            base_allocation : size,
            increment : increment,
        }
    }
    pub fn extend(& mut self) -> &Self {
        self.end = unsafe{self.end.offset(self.increment)};
        self
    }
}
pub const gc_main : usize = 0x100000000000;
pub const gc_size : isize = 0x000001000000;
