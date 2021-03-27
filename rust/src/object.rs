use std::fmt;
use std::fmt::Debug;
pub const classObject : i64 = 0;
pub const classBlockClosure : i64 = 1;
pub const classUndefinedObject : i64 = 2;
pub const classTrue : i64 = 3;
pub const classFalse : i64 = 4;
pub const classSmallInteger : i64 = 5;
pub const classSymbol : i64 = 6;
pub const classCharacter : i64 = 7;
pub const classFloat : i64 = 8;
pub const classString : i64 = 9;
pub const classClass : i64 = 10;
pub const classMetaclass : i64 = 11;
pub const classBehavior : i64 = 12;
lazy_static! {
    static ref class_names : Vec<&'static str> = vec!["object","closure","nil","true","false","integer","symbol","character","float"];
}
#[derive(Copy, Clone)]
pub union Object {
    i : i64,
    f : f64,
}
const NAN_VALUE : i64 = 0x7FF8_0000_0000_0001;
macro_rules! literalfield {
    (class = $e:expr) => {$e};
    (value = $e:expr) => {($e)<<3};
    (arity = $e:expr) => {($e)<<28};
}
macro_rules! literal {
    ($($i:ident $( = $e:expr)?),+) => {Object{i:(NAN_VALUE-1) $(| (literalfield!($i $( = $e)? )))+}}
}
pub const nilObject : Object = literal!(class=classUndefinedObject,value=0x10100);
pub const trueObject : Object = literal!(class=classTrue,value=0x12120);
pub const falseObject : Object = literal!(class=classFalse,value=0x21212);
#[inline]
pub fn symbolOf(string: &str,hash: i64) -> Object {
    let mut arity = 0;
    for ch in string.chars() {
        if ch == ':' {arity += 1}
    }
    literal!(class=classSymbol,value=hash,arity=arity)
}
impl Object {
    #[inline]
    pub const fn is_integer(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i&7==classSmallInteger
    }
    #[inline]
    pub const fn is_double(&self) -> bool {
        (unsafe{self.i})<=NAN_VALUE
    }
    #[inline]
    pub const fn is_literal(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i&7 > classBlockClosure && i!= classSmallInteger
    }
    #[inline]
    pub const fn is_object(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i&7==classObject
    }
    #[inline]
    pub const fn is_closure(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i&7==classBlockClosure
    }
    pub fn class_name(&self) -> &str {
        class_names[self.class()]
    }
    #[inline]
    pub const fn class(&self) -> usize {
        (if self.is_double() {
            classFloat
        } else {
            (unsafe{self.i})&7
        }) as usize
    }
    #[inline]
    pub const fn as_i64(&self) -> i64 {
        (unsafe{self.i})<<13>>16
    }
    #[inline]
    // this is the same as as_i64, but keeping them separate allows changing encoding if we ever wanted to change the encoding
    pub const fn as_literal(&self) -> i64 {
        (unsafe{self.i})<<13>>16
    }
    #[inline]
    pub const fn as_f64(&self) -> f64 {
        unsafe{self.f}
    }
    #[inline]
    pub const fn as_object_ptr(&self) -> & mut HeapObject {
        unsafe{& mut *((self.i-NAN_VALUE+1) as * mut HeapObject)}
    }
    #[inline]
    pub const fn as_closure_ptr(&self) -> & mut HeapObject {
        unsafe{& mut *((self.i-NAN_VALUE) as * mut HeapObject)}
    }
    #[inline]
    pub const fn raw(&self) -> * const Object {
        unsafe{self.i as * const Object}
    }
}
impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        if self.is_literal() {
            write!(f, "{}:{} {:?}",self.as_literal(),self.class_name(),self.raw())
        } else if self.is_integer() {
            write!(f, "{}:{}",self.as_i64(),self.class_name())
        } else if self.is_double() {
            write!(f, "{:e}:{}",self.as_f64(),self.class_name())
        } else if self.is_object() {
            write!(f, "{:?}:object", self.as_object_ptr())
        } else {
            write!(f, "{:?}:closure", self.as_closure_ptr())
        }
    }
}
impl From<bool> for Object {
    #[inline]
    fn from(b:bool) -> Self {
        if b {
            trueObject
        } else {
            falseObject
        }
    }
}
impl From<i64> for Object {
    #[inline]
    fn from(i:i64) -> Self {
        Object{i:(((i<<16) as usize)>>13) as i64+NAN_VALUE-1+classSmallInteger}
    }
}
impl From<char> for Object {
    #[inline]
    fn from(c:char) -> Self {
        literal!(class=classCharacter,value=c as u32 as i64)
    }
}
impl From<f64> for Object {
    #[inline]
    fn from(f:f64) -> Self {
        Object {f:f}
    }
}
impl From<& HeapObject> for Object {
    #[inline]
    fn from(o: &HeapObject) -> Self {
        Object {i:(o as *const HeapObject as i64)+NAN_VALUE-1}
    }
}
impl From<* const HeapObject> for Object {
    #[inline]
    fn from(o: * const HeapObject) -> Self {
        Object {i:(o as i64)+NAN_VALUE-1}
    }
}

#[cfg(test)]
mod testsObject {
    use super::*;
    #[test]
    fn convert_object() {
        // could get 2 more bits by changing the encoding, but makes several things more complicated
        assert_eq!(Object::from(-1<<47).as_i64(),-1<<47);
        assert_eq!(Object::from((1<<47)-1).as_i64(),(1<<47)-1);
        assert_eq!(Object::from(-1).as_i64(),-1);
        assert_eq!(Object::from(1).as_i64(),1);
        assert_eq!(Object::from(2.0_f64).as_f64(),2.0_f64);
//        assert_eq!(Object::from(&nilObject).as_ptr() as * const HeapObject,&nilObject as * const HeapObject);
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
    pub fn raw_at(&self,index:usize) -> Object {
        unsafe{self.fields[index]}
    }
    #[inline]
    pub fn raw_at_put(&mut self,index:usize,value:Object) -> Object {
        unsafe{self.fields[index]=value};
        value
    }
}
#[cfg(test)]
mod testHeapObject {
    use super::*;
    #[test]
    fn sizes() {
//        assert_eq!(nilObject.size(),0);
    }
    #[test]
    fn info() {
        assert_eq!(core::mem::size_of::<Object>(), 8);
        assert_eq!(core::mem::size_of::<Option<bool>>(), 1);
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
