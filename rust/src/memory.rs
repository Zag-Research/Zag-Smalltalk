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
    pub fn as_ptr(&self) -> & mut HeapObject {
        unsafe{& mut *((self.i-nan_value+1) as * mut HeapObject)}
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
            write!(f, "{:?}", self.as_ptr())
        }
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

#[cfg(test)]
mod testsObject {
    use super::*;
    #[test]
    fn convert_object() {
        //assert_eq!(Object::from(-1<<49).as_i64(),-1<<49);
        //assert_eq!(Object::from(1<<49).as_i64(),1<<49);
        assert_eq!(Object::from(-1).as_i64(),-1);
        assert_eq!(Object::from(1).as_i64(),1);
        assert_eq!(Object::from(2.0_f64).as_f64(),2.0_f64);
        assert_eq!(Object::from(&nilObject).as_ptr() as * const HeapObject,&nilObject as * const HeapObject);
    }
}

union HeapHeader {
    i : isize,
    u : usize,
    bytes : [u8;8],  // x86_64: 0 is lsb 7 is msb
    words : [u32;2], // x86_64: 0 is low word, 1 is high
}
impl HeapHeader {
    fn class(&self) -> usize {
        (unsafe{self.u})&(1<<22)-1
    }
    fn hash(&self) -> usize {
        (unsafe{self.u})>>32 & (1<<22)-1
    }
    fn size(&self) -> usize {
        (unsafe{self.u})>>24 & 255
    }
    fn size_set(&mut self,new:usize) {
        unsafe{self.bytes[3]=new as u8}
    }
    fn format(&self) -> usize {
        (unsafe{self.u})>>56 & 31
    }
    fn immutable(&self) -> bool {
        (unsafe{self.u})>>22 != 0
    }
    fn forwarded(&self) -> bool {
        (unsafe{self.i})<0
    }
}
impl Debug for HeapHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        write!(f, "cl:{} hs:{} sz:{} fm:{}:-",self.class(),self.hash(),self.size(),self.format())
    }
}
macro_rules! headerfield {
    (class = $e:expr) => {$e};
    (immutable) => {1<<22};
    (pinned) => {1<<23};
    (size = $e:expr) => {($e)<<24};
    (hash = $e:expr) => {($e)<<32};
    (unused) => {1<<54};
    (grey) => {1<<55};
    (format = $e:expr) => {($e)<<56};
    (remembered) => {1<<61};
    (marked) => {1<<62};
    (forward) => {1<<63};
}
macro_rules! header {
    ($($i:ident $( = $e:expr)?),+) => {HeapHeader{u:0 $(| (headerfield!($i $( = $e)? )))+}}
}
static hh : HeapHeader = header!(class=3,hash=1,immutable);
pub struct HeapObject {
    header : HeapHeader,
    fields : [Object;0], // typically many more than 1, but will be accessed as unsafe
}

pub static nilObject : HeapObject = HeapObject{header:header!(class=1,hash=10101),fields:[]};
pub static trueObject : HeapObject = HeapObject{header:header!(class=3,hash=12121),fields:[]};
pub static falseObject : HeapObject = HeapObject{header:header!(class=4,hash=21212),fields:[]};
const mask47 : u64 = 0x7fffffffffff;
const mask22 : u32 = 0x3fffff;
const mask5 : u8 = 0x1f;
impl HeapObject {
    #[inline]
    #[cfg(target_arch="x86_64")] // order of fields may differ
    pub fn size(&self) -> usize {
        let size = self.header.size();
        size
    }
    #[inline]
    #[cfg(target_arch="x86_64")]
    pub fn size_set(&mut self,new:usize) {
        self.header.size_set(new)
    }
    #[inline]
    #[cfg(target_arch="x86_64")]
    pub fn identityHash(&self) -> usize {
        self.header.hash()
    }
    #[inline]
    #[cfg(target_arch="x86_64")]
    pub fn classIndex(&self) -> usize {
        self.header.class()
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
#[cfg(test)]
mod testHeapObject {
    use super::*;
    #[test]
    fn sizes() {
        asssert_eq!(nilObject.size(),0);
    }
}
impl Debug for HeapObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        write!(f, "({:?}",self.header);
        write!(f,")")
    }
}
impl PartialEq for HeapObject {
    fn eq(&self,other:&HeapObject) -> bool {
        self==other
    }
}

pub struct AllocableRegion {
    base : * mut Object,
    size : usize,
    end : * mut Object,
    current : * mut Object,
}
const min_page_size : isize = 16384;
extern crate libc;
impl AllocableRegion {
    pub fn new(address: usize,size:isize) -> Self {
        let address = address as * mut Object;
        let size = (size+min_page_size-1)&(-min_page_size)/(mem::size_of::<Object>() as isize);
        AllocableRegion {
            base : address,
            size : size,
            end : address,
            current : address,
        }
    }
    pub fn mapMemory(& mut self) {
        let data = unsafe{
            libc::mmap(
                /* addr: */ self.base as *mut libc::c_void,
                /* len: */ self.size,
                /* prot: */ libc::PROT_READ | libc::PROT_WRITE,
                /* flags: */ libc::MAP_ANON,
                /* fd: */ 0,
                /* offset: */ 0,
            )};
        if data == libc::MAP_FAILED {
            panic!("Could not memory map")
        }
        let data = data as *mut Object;
        if data != self.address {
            panic!("data mapped at wrong address")
        }
        self.end = unsafe{data.offset(size)};
    }
    pub fn releaseMemory(& mut self) {
        self.end = self.base;
        self.current = self.base;
        if -1 == unsafe{
            libc::munmap(
                /* addr: */ self.base as *mut libc::c_void,
                /* len: */ self.size,
            )} {
            panic!("Failed to release memory map")
        }
    }
    pub fn allocObject(& mut self,size:isize) -> &Self {
        self.current = unsafe{self.current.offset(size)};
        // have to set the header and initialize the object
        self
    }
    pub fn gc(& mut self,sink: & mut AllocableRegion) {
    }
}
pub struct Memory {
    genOld1 : AllocableRegion,
    genOld2 : AllocableRegion,
    genTeen : AllocableRegion,
    nursery : AllocableRegion,
}
const min_page_size : isize = 16384;
extern crate libc;
impl Memory {
    pub fn assignObject(& mut self,target: Object, offset: isize,value: Object) {
    }
    pub fn allocObject(& mut self,size:isize) {
    }
    pub fn minorGC(&mut self) {
    }
    pub fn fullGC(&mut self) {
    }
    pub fn become(& mut self,target: Object, value: Object) {
        if !target.is_ptr || !value.is_ptr {
            panic!("can't become a fixed value")
        }
        if value<target {
            self.become(value,target)
        } else {

        }
    }
}
pub const gc_main : usize = 0x100000000000;
pub const gc_size : isize = 0x000001000000;
