use std::fmt;
use std::fmt::Debug;
pub const classObject : usize = 0;
pub const classBlockClosure : usize = 1;
pub const classFalse : usize = 2;
pub const classTrue : usize = 3;
pub const classUndefinedObject : usize = 4;
pub const classSmallInteger : usize = 5;
pub const classSymbol : usize = 6;
pub const classCharacter : usize = 7;
pub const classFloat : usize = 8;
pub const classString : usize = 9;
pub const classClass : usize = 10;
pub const classMetaclass : usize = 11;
pub const classBehavior : usize = 12;
pub const classArray : usize = 13;
lazy_static! {
    static ref class_names : Vec<&'static str> = vec![
        "Object","closure","False","True",
        "UndefinedObject","SmallInteger","Symbol","Character",
        "Float","String","Class","Metaclass",
        "Behavior","Array",
    ];
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
    ($($i:ident $( = $e:expr)?),+) => {Object{i:((NAN_VALUE as usize-1) $(| (literalfield!($i $( = $e)? )))+) as i64}}
}
pub const nilObject : Object = literal!(class=classUndefinedObject,value=7);
pub const trueObject : Object = literal!(class=classTrue,value=5);
pub const falseObject : Object = literal!(class=classFalse,value=3);
impl Default for Object {
    fn default() -> Self { nilObject }
}
pub const zeroObject : Object = Object{i:0}; // used to init non-Object array objects

impl PartialEq for Object {
    fn eq(&self, other: &Object) -> bool {
        let i = unsafe{self.i};
        if (i|1)==NAN_VALUE {
            false
        } else {
            unsafe{i==other.i}
        }
    }
}
#[inline]
pub fn symbolOf(string: &str,hash: usize) -> Object {
    let mut arity = 0;
    for ch in string.chars() {
        if ch == ':' {arity += 1}
        else if !(ch.is_alphanumeric() || ch=='_') {arity = 1}
    }
    literal!(class=classSymbol,value=hash,arity=arity)
}
impl Object {
    #[inline]
    pub const fn is_integer(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i as usize &7==classSmallInteger
    }
    #[inline]
    pub const fn is_bool(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i as usize &6==classTrue&6
    }
    #[inline]
    pub const fn is_double(&self) -> bool {
        (unsafe{self.i})<=NAN_VALUE
    }
    #[inline]
    pub const fn is_literal(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && ({let cl = i as usize &7; cl > classBlockClosure && cl!= classSmallInteger})
    }
    #[inline]
    pub const fn is_on_heap(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i as usize &6==classObject
    }
    #[inline]
    pub const fn is_object(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i as usize &7==classObject
    }
    #[inline]
    pub const fn is_closure(&self) -> bool {
        let i=unsafe{self.i};
        i>NAN_VALUE && i as usize &7==classBlockClosure
    }
    pub fn class_name(&self) -> &str {
        class_names[self.class()]
    }
    #[inline]
    pub const fn class(&self) -> usize {
        (if self.is_double() {
            classFloat
        } else {
            let cl = (unsafe{self.i}) as usize &7;
            cl
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
    pub const fn raw(&self) -> usize {
        unsafe{self.i as usize}
    }
}
impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        if self.is_literal() {
            write!(f, "{}:{} {:x}",self.as_literal(),self.class_name(),self.raw())
        } else if self.is_integer() {
            write!(f, "{}:{} {:x}",self.as_i64(),self.class_name(),self.raw())
        } else if self.is_double() {
            write!(f, "{:e}:{} {:x}",self.as_f64(),self.class_name(),self.raw())
        } else if self.is_object() {
            write!(f, "{:?}:object {:x}", self.as_object_ptr(),self.raw())
        } else {
            write!(f, "{:?}:closure {:x}", self.as_closure_ptr(),self.raw())
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
        Object{i:((((i<<16) as usize)>>13)+NAN_VALUE as usize -1+classSmallInteger) as i64}
    }
}
impl From<char> for Object {
    #[inline]
    fn from(c:char) -> Self {
        literal!(class=classCharacter,value=c as u32 as usize)
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
        Object::from(o as *const HeapObject)
    }
}
impl From<* const HeapObject> for Object {
    #[inline]
    fn from(o: * const HeapObject) -> Self {
        Object {i:(o as i64)+NAN_VALUE-1}
    }
}
impl std::ops::Not for Object {
    type Output = Self;
    fn not(self) -> Self::Output {
        if self==trueObject {
            falseObject
        } else {
            trueObject
        }
    }
}


#[cfg(test)]
mod testsObject {
    use super::*;
    use std::mem;
    #[test]
    fn default_object() {
        let def : [Object;10] = Default::default();
        assert_eq!(def[1],nilObject);
        assert_eq!(mem::size_of::<Object>(),8);
    }
    #[test]
    fn equal_objects() {
        assert_eq!(Object::from(-1<<47),Object::from(-1<<47));
        assert_eq!(Object::from(-1<<47),Object::from(-1<<47));
        assert_eq!(!Object::from(true),falseObject);
        assert_eq!(!Object::from(false),trueObject);
        assert!(f64::NAN!=f64::NAN);
        let nan = Object::from(f64::NAN);
        assert!(nan!=nan);
        assert!(nan!=trueObject);
        assert!(trueObject!=nan);
        let nan = Object::from(0.0_f64/0.0);
        assert!(nan!=nan);
    }
    #[test]
    fn convert_object() {
        // could get 2 more bits by changing the encoding, but makes several things more complicated
        assert_eq!(Object::from(-1<<47).as_i64(),-1<<47);
        assert_eq!(Object::from((1<<47)-1).as_i64(),(1<<47)-1);
        assert!(Object::from(-1<<48).as_i64()!=-1<<48);
        assert_eq!(Object::from(-1).as_i64(),-1);
        assert_eq!(Object::from(1).as_i64(),1);
        assert_eq!(Object::from(2.0_f64).as_f64(),2.0_f64);
        assert!( classTrue&6 == classFalse&6);
        assert!( classObject&6 == classBlockClosure&6);
        assert!(Object::from(true).is_bool());
        assert!(Object::from(false).is_bool());
        assert!(!Object::from(1).is_bool());
        assert_eq!(symbolOf("abc",123),literal!(class=classSymbol,value=123,arity=0));
        assert_eq!(symbolOf("+",123),literal!(class=classSymbol,value=123,arity=1));
        assert_eq!(symbolOf("abc:def:ghi:",123),literal!(class=classSymbol,value=123,arity=3));
    }
}


union HeapHeader {
    i : isize,
    u : usize,
    bytes : [u8;8],  // x86_64: 0 is lsb 7 is msb
    halfWords : [u16;4],  // x86_64: 0 is low 3 is high
    words : [u32;2], // x86_64: 0 is low word, 1 is high
}
const hashShift : usize = 20;
const classMask : usize = (1<<hashShift)-1;
const hashMask : usize = 0xfffff;

impl HeapHeader {
    #[inline]
    fn class(&self) -> usize {
        (unsafe{self.u})&classMask
    }
    #[inline]
    fn hash(&self) -> usize {
        (unsafe{self.u})>>hashShift & hashMask
    }
    #[inline]
    fn size(&self) -> usize {
        (unsafe{self.halfWords[3]}) as usize
    }
    #[inline]
    fn size_set(&mut self,new:usize) {
        unsafe{self.halfWords[3]=new as u16}
    }
    #[inline]
    fn format(&self) -> usize {
        ((unsafe{self.bytes[5]}) & 31) as usize
    }
    #[inline]
    fn is_immutable(&self) -> bool {
        (unsafe{self.bytes[5]}) & 128 != 0
    }
    #[inline]
    fn is_forwarded(&self) -> bool {
        (unsafe{self.i})<0
    }
    #[inline]
    fn is_indexed(&self) -> bool {
        self.format()>=8
    }
    #[inline]
    fn is_weak(&self) -> bool {
        (self.format()&20)==4
    }
    #[inline]
    fn has_instVars(&self) -> bool {
        (self.format()&17)==1
    }
    #[inline]
    fn may_have_pointers(&self) -> bool {
        self.format()<16
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
    (immutable) => {1<<47};
    (size = $e:expr) => {($e)<<48};
    (hash = $e:expr) => {(($e)&hashMask)<<hashShift};
    (format = $e:expr) => {($e)<<40};
    (forward) => {1<<63};
}
macro_rules! header {
    ($($i:ident $( = $e:expr)?),+) => {HeapHeader{u:0_usize $(| (headerfield!($i $( = $e)? )))+}}
}
#[cfg(test)]
mod testHeapHeader {
    use super::*;
    #[test]
    fn transforms() {
        let h = header!(class=11,hash=17,format=1,immutable);
        assert_eq!(h.class(),11);
        assert_eq!(h.hash(),17);
        assert!(h.is_immutable());
        assert!(h.has_instVars());
        assert!(h.may_have_pointers());
        assert!(!h.is_weak());
        assert!(!h.is_forwarded());
        assert!(!h.is_indexed());
    }
    #[test]
    fn sizes() {
        assert_eq!(core::mem::size_of::<HeapHeader>(), core::mem::size_of::<Object>());
    }
}

#[inline]
fn heapObjectSize(n_instVars: usize, n_indexed: isize, width: usize) -> usize {
    if n_indexed < 0 {
        1 + n_instVars
    } else if n_instVars > 0 {
        panic!("don't support format=9 objects")
    } else {
        let items_per_word = 8 / width;
        let words = ((n_indexed as usize) + items_per_word - 1) / items_per_word;
        if words < 32767 {
            1 + words
        } else {
            2 + words
        }
    }
}
#[cfg(test)]
mod testHeapObjectSize {
    use super::*;
    #[test]
    fn sizes() {
        assert_eq!(heapObjectSize(3,-1,8),4);
        assert_eq!(heapObjectSize(0,5,8),6);
        assert_eq!(heapObjectSize(0,40000,8),40002);
        assert_eq!(heapObjectSize(0,17,4),10);
        assert_eq!(heapObjectSize(0,17,2),6);
        assert_eq!(heapObjectSize(0,17,1),4);
        assert_eq!(heapObjectSize(0,16,4),9);
        assert_eq!(heapObjectSize(0,16,2),5);
        assert_eq!(heapObjectSize(0,16,1),3);
    }
}
#[repr(C)]
pub struct HeapObject {
    header : HeapHeader,
    fields : [Object;0], // typically many more than 1, but will be accessed as unsafe
}

impl HeapObject {
    #[inline]
    pub fn size(&self) -> usize {
        let size = self.header.size();
        size
    }
    #[inline]
    pub fn identityHash(&self) -> usize {
        self.header.hash()
    }
    #[inline]
    pub fn classIndex(&self) -> usize {
        self.header.class()
    }
    #[inline]
    pub fn raw_at(&self,index:usize) -> Object {
        let fields : * const Object = &self.fields as * const Object;
        unsafe{fields.offset(index as isize).read()} // shouldn't be +1
    }
    #[inline]
    pub fn raw_at_put(&mut self,index:usize,value:Object) {
        let fields : * mut Object = &mut self.fields as * mut Object;
        unsafe{fields.offset(index as isize).write(value)} // shouldn't be +1
    }
    pub fn initialize(&mut self,class:usize,n_instVars:usize,n_indexed:isize,width:usize,hash:usize,init:Object) -> * mut HeapObject {
        let format=(if n_indexed>=0 {8} else {1});
        self.header=header!(class=class,hash=hash,format=format);
        let words = heapObjectSize(n_instVars,n_indexed,width);
        for i in 0..words-1 {
            self.raw_at_put(i,init)
        };
        unsafe{(self as *mut HeapObject).offset(words as isize)}
    }
}
#[cfg(test)]
mod testHeapObject {
    use super::*;
    fn print_first(array:&[usize],n:usize) {
        for (index,value) in array[..n].iter().enumerate() {
            if index%8==0 {
                if index>0 {println!("")};
                print!("{:4x}:",index)
            };
            print!(" {:016x}",value)
        }
        if n>0 {println!("")}
    }
    #[test]
    fn correct() {
        let mut mem = [0_usize;100];
        let one = Object::from(1);
        let two = Object::from(2);
        let next : * mut HeapObject = &mut mem as * const usize as * mut HeapObject;
        let obj1 = unsafe { next.as_mut().unwrap() };
        let mut next = obj1.initialize(classArray,0,5,8,0x11,Default::default());
        obj1.raw_at_put(0,falseObject);
        obj1.raw_at_put(1,one);
        obj1.raw_at_put(2,two);
        let obj2 = unsafe { next.as_mut().unwrap() };
        let next = obj2.initialize(classClass,3,-1,8,0x22,Default::default());
        obj2.raw_at_put(0,trueObject);
        obj2.raw_at_put(1,one);
        let obj3 = unsafe { next.as_mut().unwrap() };
        let _ = obj3.initialize(classBehavior,0,-1,8,0x33,Default::default());
//      print_first(&mem,20);
        assert_eq!(mem[10],0x000001000330000c); // obj3
        assert_eq!(mem[0],0x000008000110000d); // obj1
        assert_eq!(mem[1],falseObject.raw());
        assert_eq!(mem[2],one.raw());
        assert_eq!(mem[3],two.raw());
        assert_eq!(mem[4],nilObject.raw());
        assert_eq!(mem[5],nilObject.raw());
        assert_eq!(mem[6],0x000001000220000a); // obj2
        assert_eq!(mem[7],trueObject.raw());
        assert_eq!(mem[8],one.raw());
        assert_eq!(mem[9],nilObject.raw());
    }
    #[test]
    fn sizes() {
        assert_eq!(core::mem::size_of::<HeapObject>(), core::mem::size_of::<Object>());
//        assert_eq!(core::mem::size_of::<Option<Object>>(), core::mem::size_of::<Object>());
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
