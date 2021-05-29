use std::cmp::Ordering::{self,Equal,Less,Greater};
use std::fmt;
use std::fmt::Debug;
pub const classObject: u16 = 0;
pub const classBlockClosure: u16 = 1;
pub const classFalse: u16 = 2;
pub const classTrue: u16 = 3;
pub const classUndefinedObject: u16 = 4;
pub const classSmallInteger: u16 = 5;
pub const classSymbol: u16 = 6;
pub const classCharacter: u16 = 7;
pub const classFloat: u16 = 8;
pub const classString: u16 = 9;
pub const classClass: u16 = 10;
pub const classMetaclass: u16 = 11;
pub const classBehavior: u16 = 12;
pub const classArray: u16 = 13;
#[derive(Copy, Clone)]
pub union Object {
    i: isize,
    f: f64,
}
static_assertions::assert_eq_size!(isize, f64);
pub type StaticStr = &'static str;
const NAN_VALUE: isize = 0x7FF8_0000_0000_0001;
macro_rules! literalfield {
    (class = $e:expr) => {($e as usize)};
    (value = $e:expr) => {($e)<<3};
    (arity = $e:expr) => {($e)<<28};
}
macro_rules! literal {
    ($($i:ident $( = $e:expr)?),+) => {Object{i:((NAN_VALUE as usize-1) $(| (literalfield!($i $( = $e)? )))+) as isize}}
}
pub const falseObject: Object = literal!(class=classFalse,value=3);
pub const trueObject: Object = literal!(class=classTrue,value=5);
pub const nilObject: Object = literal!(class=classUndefinedObject,value=7);
pub const placeholderObject: Object = literal!(class=classUndefinedObject,value=0x1ffffff);
impl Default for Object {
    fn default() -> Self {
        if cfg!(test) {Object{i:42}}
        else {nilObject}
    }
}
pub const zeroObject: Object = Object{i:0}; // used to init non-Object array objects

impl Ord for Object {
    fn cmp(&self, other: &Self) -> Ordering {
        let s = unsafe{self.i};
        let o = unsafe{other.i};
        s.cmp(&o)
    }
}
impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for Object {}
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
pub const fn uncheckedSymbolOf(hash: u32) -> Object {
    literal!(class=classSymbol,value=hash as usize)
}
#[inline]
const fn object_test(i:usize) -> bool {
    (i as isize)>NAN_VALUE
}
const fn on_heap(i:usize) -> bool {
    object_test(i) && (i&6) as u16==classObject
}
impl Object {
    #[inline]
    pub const fn is_integer(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as u16==classSmallInteger
    }
    #[inline]
    pub const fn is_symbol(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as u16==classSymbol
    }
    #[inline]
    pub const fn is_bool(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&6) as u16==classTrue&6
    }
    #[inline]
    pub const fn is_double(&self) -> bool {
        !object_test(self.raw())
    }
    #[inline]
    pub const fn is_literal(&self) -> bool {
        let i = self.raw();
        object_test(i) && ({let cl = (i&7) as u16; cl > classBlockClosure && cl!= classSmallInteger})
    }
    #[inline]
    pub const fn is_on_heap(&self) -> bool {
        on_heap(self.raw())
    }
    #[inline]
    pub const fn is_object(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as u16==classObject
    }
    #[inline]
    pub const fn is_closure(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as u16==classBlockClosure
    }
    pub fn class_name(&self) -> &str {
        crate::class::name_str(self.class())
    }
    #[inline]
    pub const fn immediateHash(&self) -> u32 {
        (self.raw()>>3) as u32
    }
    #[inline]
    pub fn hash(&self) -> usize {
        if on_heap(self.raw()) {
            (unsafe{*self.as_object_ptr()}).hash()
        } else {
            self.immediateHash() as usize&hashMask
        }
    }
    #[inline]
    pub fn class(&self) -> u16 {
        if self.is_double() {
            classFloat
        } else {
            let cl = (self.raw() & 7) as u16;
            if cl == classObject {
                panic!("other class")
            } else {
                cl
            }
        }
    }
    #[inline]
    pub const fn as_i48(&self) -> isize {
        (unsafe{self.i as isize})<<13>>16
    }
    #[inline]
    pub const fn as_u48(&self) -> usize {
        ((unsafe{self.i}) as usize)<<13>>16
    }
    #[inline]
    pub const fn as_u16(&self) -> u16 {
        (unsafe{self.i>>3}) as u16
    }
    #[inline]
    // this is the same as as_i48, but keeping them separate allows changing encoding if we ever wanted to
    pub const fn as_literal(&self) -> isize {
        (unsafe{self.i})<<13>>16
    }
    #[inline]
    pub const fn as_f64(&self) -> f64 {
        unsafe{self.f}
    }
    #[inline]
    pub const fn as_object_ptr(&self) -> * mut HeapHeader {
        unsafe{& mut *((self.i-NAN_VALUE+1) as * mut HeapHeader)}
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
        if self.is_symbol() {
            write!(f, "{}",crate::symbol::str_of(*self))
        } else if self.is_literal() {
            write!(f, "{:x}: {:x}",self.as_literal(),self.raw())
        } else if self.is_integer() {
            write!(f, "{}:{} {:x}",self.as_i48(),self.class_name(),self.raw())
        } else if self.is_double() {
            write!(f, "{:e}:{} {:x}",self.as_f64(),self.class_name(),self.raw())
        } else if self.is_object() {
            write!(f, "{:?}:object {:x}", self.as_object_ptr(),self.raw())
        } else {
            write!(f, "{:?}:closure {:x}", self.as_object_ptr(),self.raw())
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
impl From<isize> for Object {
    #[inline]
    fn from(i:isize) -> Self {
        Object{i:((((i<<16) as usize)>>13)+NAN_VALUE as usize -1+classSmallInteger as usize) as isize}
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
        Object {i:(o as isize)+NAN_VALUE-1}
    }
}
#[cfg(test)]
impl From<&mut [HeapObject]> for Object {
    #[inline]
    fn from(o:&mut [HeapObject]) -> Self {
        Object {i:(o.as_mut_ptr() as isize)+NAN_VALUE-1}
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

 // This type is used a lot. Make sure it doesn't unintentionally get bigger.
#[cfg(target_arch = "x86_64")]
static_assertions::assert_eq_size!(Object, u64);
#[cfg(target_arch = "x86_64")]
static_assertions::assert_eq_size!(Option<u32>, u64);

#[cfg(test)]
mod testsObject {
    use super::*;
    #[test]
    fn class_names() {
        assert_eq!(symbolOf("Object",10).class_name(),"Symbol");
        assert_eq!(Object::from(42).class_name(),"SmallInteger");
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
        assert_eq!(Object::from(-1<<47).as_i48(),-1<<47);
        assert_eq!(Object::from((1<<47)-1).as_i48(),(1<<47)-1);
        assert!(Object::from(-1<<48).as_i48()!=-1<<48);
        assert_eq!(Object::from(-1).as_i48(),-1);
        assert_eq!(Object::from(1).as_i48(),1);
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

#[derive(Copy, Clone)]
union HeapHeader {
    i: isize,
    u: usize,
    bytes: [u8;8],  // x86_64: 0 is lsb 7 is msb
    halfWords: [u16;4],  // x86_64: 0 is low 3 is high
    words: [u32;2], // x86_64: 0 is low word, 1 is high
}

const hashShift: usize = 20;
const classMask: usize = (1<<hashShift)-1;
const hashMask: usize = 0xfffff;

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
        self.format()==13
    }
    #[inline]
    fn has_instVars(&self) -> bool {
        let format = self.format();
        format==16 || format<15 && format!=11
    }
    #[inline]
    fn may_have_pointers(&self) -> bool {
        self.format()<14
    }
}
impl From<Object> for HeapHeader {
    #[inline]
    fn from(o:Object) -> Self {
        unsafe{*o.as_object_ptr()}
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
    (class = $e:expr) => {($e as usize)};
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
        let h = header!(class=11,hash=17,format=11,immutable);
        assert_eq!(h.class(),11);
        assert_eq!(h.hash(),17);
        assert!(h.is_immutable());
        assert!(!h.has_instVars());
        assert!(h.may_have_pointers());
        assert!(!h.is_weak());
        assert!(!h.is_forwarded());
        assert!(h.is_indexed());
    }
}

const format_inst:usize = 10;
const format_idx:usize = 11;
const format_idx_inst:usize = 12;
const format_idx_inst_weak:usize = 13;
const format_inst_np:usize = 14;
const format_idx_np:usize = 15;
const format_idx_inst_np:usize = 16;
const format_idx_other_base:usize = 16;
const format_idx_other_8:usize = 17;
const format_idx_other_4:usize = 18;
const format_idx_other_2:usize = 20;
const format_idx_other_1:usize = 24;
#[inline]
fn formatAndSize(n_instVars:usize,n_indexed:isize,width:isize,weak:bool) -> (usize,usize,isize) {
    if weak {
        (format_idx_inst_weak, n_instVars, n_instVars as isize + std::cmp::max(0,n_indexed) + 2)
    } else if n_indexed < 0 {
        (format_inst_np , n_instVars, n_instVars as isize +1)
    } else if n_instVars > 0 {
        (format_idx_inst_np, n_instVars, n_instVars as isize + n_indexed + 2)
    } else if width<0 {
        if n_indexed < 32767 {
            (format_idx_np , n_indexed as usize, n_indexed + 1)
        } else {
            (format_idx_inst_np , 0, n_indexed + 2)
        }
    } else {
        let items_per_word = 8 / width;
        let words = (n_indexed + items_per_word - 1) / items_per_word;
        if words < 32767 {
            (format_idx_other_base+items_per_word as usize, words as usize, words + 1)
        } else {
            (format_idx_other_base+items_per_word as usize, 32767, words + 2)
        }
    }
}
#[cfg(test)]
mod testFormatAndSize {
    use super::*;
    #[test]
    fn sizes() {
        assert_eq!(formatAndSize(3,-1,-1,false),(format_inst_np,3,4));
        assert_eq!(formatAndSize(0,5,-1,false),(format_idx_np,5,6));
        assert_eq!(formatAndSize(3,-1,-1,true),(format_idx_inst_weak,3,5));
        assert_eq!(formatAndSize(0,5,-1,true),(format_idx_inst_weak,0,7));
        assert_eq!(formatAndSize(0,40000,-1,false),(format_idx_inst_np,0,40002));
        assert_eq!(formatAndSize(0,40000,-1,false),(format_idx_inst_np,0,40002));
        assert_eq!(formatAndSize(5,40000,-1,false),(format_idx_inst_np,5,40007));
        assert_eq!(formatAndSize(5,20000,-1,false),(format_idx_inst_np,5,20007));
        assert_eq!(formatAndSize(0,40000,8,false),(format_idx_other_8,32767,40002));
        assert_eq!(formatAndSize(0,20000,8,false),(format_idx_other_8,20000,20001));
        assert_eq!(formatAndSize(0,17,8,false),(format_idx_other_8,17,18));
        assert_eq!(formatAndSize(0,17,4,false),(format_idx_other_4,9,10));
        assert_eq!(formatAndSize(0,17,2,false),(format_idx_other_2,5,6));
        assert_eq!(formatAndSize(0,17,1,false),(format_idx_other_1,3,4));
        assert_eq!(formatAndSize(0,16,4,false),(format_idx_other_4,8,9));
        assert_eq!(formatAndSize(0,16,2,false),(format_idx_other_2,4,5));
        assert_eq!(formatAndSize(0,16,1,false),(format_idx_other_1,2,3));
    }
}
#[repr(C)]
pub struct HeapObject {
    header: HeapHeader,
    fields: [Object;0], // typically many more than 1, but will be accessed as unsafe
}
static_assertions::assert_eq_size!(Object, HeapObject);

impl HeapObject {
    #[inline]
    pub fn identityHash(&self) -> usize {
        self.header.hash()
    }
    #[inline]
    pub fn classIndex(&self) -> usize {
        self.header.class()
    }
    #[inline]
    pub fn n_instVars(&self) -> usize {
        let format = self.header.format();
        if format&27==11 {
            0
        } else if format<=16 {
            self.header.size()
        } else {
            0
        }
    }
    #[inline]
    pub fn firstIndex(&self) -> usize {
        let size = self.header.size();
        let format = self.header.format();
        if format&27==10 {
            size
        } else if format&27==11 {
            0
        } else if format<=16 {
            size+1
        } else if size<32767 {
            0
        } else {
            1
        }
    }
    #[inline]
    pub fn n_index(&self) -> usize {
        let size = self.header.size();
        let format = self.header.format();
        if format&27==10 {
            0
        } else if format&27==11 {
            size
        } else if format<=16 {
            self.raw_at(size).raw()
        } else if size<32767 {
            size
        } else {
            self.raw_at(0).raw()
        }
    }
    #[inline]
    pub fn raw_at(&self,index:usize) -> Object {
        let fields: * const Object = &self.fields as * const Object;
        unsafe{fields.offset(index as isize).read()}
    }
    #[inline]
    pub fn raw_at_put(&mut self,index:usize,value:Object) {
        let fields: * mut Object = &mut self.fields as * mut Object;
        unsafe{fields.offset(index as isize).write(value)}
    }
    pub fn initialize(&mut self,init:Object) {
        for i in 0..self.n_instVars() {
            self.raw_at_put(i,Default::default())
        };
        let first = self.firstIndex();
        for i in first..first+self.n_index() {
            self.raw_at_put(i,init)
        };
    }
    pub fn alloc(&mut self,class:u16,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool) -> * mut HeapObject {
        let (format,size,total_size)=formatAndSize(n_instVars,n_indexed,width,weak);
        self.header=header!(class=class,hash=hash,format=format,size=size);
        let extra = self.firstIndex();
        if extra>self.n_instVars() {self.raw_at_put(extra-1,Object{i:n_indexed})};
        unsafe{(self as *mut HeapObject).offset(total_size)}
    }
}
impl Default for HeapObject {
    fn default() -> Self {
        HeapObject{header:HeapHeader{i:0},fields:[]}
    }
}

#[cfg(test)]
mod testHeapObject {
    use super::*;
    fn print_first(array:&[usize],end:*const HeapObject) {
        let n = unsafe{(end as *const usize).offset_from(array.as_ptr()) as usize};
        for (index,value) in array[..n].iter().enumerate() {
            if index%8==0 {
                if index>0 {println!()};
                print!("{:4x}:",index)
            };
            print!(" {:016x}",value)
        }
        if n>0 {println!()}
    }
    #[test]
    fn correct() {
        let def: Object = Default::default();
        let uninit = 0xdead_usize;
        let mut mem = [uninit;100000];
        let one = Object::from(1);
        let two = Object::from(2);
        let next: * mut HeapObject = &mut mem as * const usize as * mut HeapObject;
        let obj1 = unsafe { next.as_mut().unwrap() };
        let next = obj1.alloc(classArray,0,5,-1,0x101,false);
        obj1.initialize(Default::default());
        obj1.raw_at_put(0,falseObject);
        obj1.raw_at_put(1,one);
        obj1.raw_at_put(2,two);
        let obj2 = unsafe { next.as_mut().unwrap() };
        let next = obj2.alloc(classClass,3,-1,-1,0x202,false);
        obj2.initialize(Default::default());
        obj2.raw_at_put(0,trueObject);
        obj2.raw_at_put(1,one);
        let obj3 = unsafe { next.as_mut().unwrap() };
        let end = obj3.alloc(classBehavior,0,-1,-1,0x303,false);
        obj3.initialize(Default::default());
        print_first(&mem,end);
        assert_eq!(mem[0],0x00050f001010000d); // obj1
        assert_eq!(mem[1],falseObject.raw());
        assert_eq!(mem[2],one.raw());
        assert_eq!(mem[3],two.raw());
        assert_eq!(mem[4],def.raw());
        assert_eq!(mem[5],def.raw());
        assert_eq!(mem[6],0x00030e002020000a); // obj2
        assert_eq!(mem[7],trueObject.raw());
        assert_eq!(mem[8],one.raw());
        assert_eq!(mem[9],def.raw());
        assert_eq!(mem[10],0x00000e003030000c); // obj3
        assert_eq!(mem[11],uninit);
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
