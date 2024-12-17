use std::cmp::Ordering::{self,Equal,Less,Greater};
use std::hash::{Hash,Hasher};
use std::fmt;
use std::fmt::Debug;
pub type ClassIndex = u16; // only allows 65535 classes and this size is baked into a few places, but Pharo only has 20000, so shouldn't be a problem
pub const classObject: ClassIndex = 0;
pub const classBlockClosure: ClassIndex = 1;
pub const classFalse: ClassIndex = 2;
pub const classTrue: ClassIndex = 3;
pub const classUndefinedObject: ClassIndex = 4;
pub const classSymbol: ClassIndex = 5;
pub const classCharacter: ClassIndex = 6;
pub const classSmallInteger: ClassIndex = 7;
pub const classFloat: ClassIndex = 8;
pub const classArray: ClassIndex = 9;
pub const classString: ClassIndex = 10;
pub const classClass: ClassIndex = 11;
pub const classMetaclass: ClassIndex = 12;
pub const classBehavior: ClassIndex = 13;
pub const classMethod: ClassIndex = 14;
pub const classSystem: ClassIndex = 15;
pub const classReturn: ClassIndex = 16;
pub const classSend: ClassIndex = 17;
pub const classLiteral: ClassIndex = 18;
pub const classLoad: ClassIndex = 19;
pub const classStore: ClassIndex = 20;
pub const number_of_constant_classes: usize = 21;
#[derive(Copy, Clone)]
pub union Object {
    i: isize,
    u: usize,
    f: f64,
}
static_assertions::assert_eq_size!(isize, f64);
pub type StaticStr = &'static str;
const NEG_INFINITY: usize = 0xFFF8_0000_0000_0000;
macro_rules! literalfield {
    (class = $e:expr) => {($e as usize<<49)};
    (value = $e:expr) => {$e};
    (arity = $e:expr) => {($e)<<28};
}
macro_rules! literal {
    ($($i:ident $( = $e:expr)?),+) => {Object{u:(NEG_INFINITY $(| (literalfield!($i $( = $e)? )))+)}}
}
pub const falseObject: Object = literal!(class=classFalse,value=3);
pub const trueObject: Object = literal!(class=classTrue,value=5);
pub const nilObject: Object = literal!(class=classUndefinedObject,value=7);
pub const selfSymbolObject: Object = literal!(class=classSymbol,value=10);
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
impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let i = unsafe{self.i};
        i.hash(state);
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
static_assertions::const_assert_eq!(classObject|1,classBlockClosure);
#[inline]
pub const fn on_heap(i:usize) -> bool {
    object_test(i) && (i&6) as ClassIndex==classObject // catches classBlockClosure too since that assertion
}
impl Object {
    #[inline]
    pub const fn raw(&self) -> usize {
        unsafe{self.i as usize}
    }
    #[inline]
    pub const fn is_integer(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as ClassIndex==classSmallInteger
    }
    #[inline]
    pub const fn is_symbol(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as ClassIndex==classSymbol
    }
    #[inline]
    pub const fn is_nil(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as ClassIndex==classUndefinedObject
    }
    #[inline]
    pub const fn is_bool(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&6) as ClassIndex==classTrue&6
    }
    #[inline]
    pub const fn is_double(&self) -> bool {
        !object_test(self.raw())
    }
    #[inline]
    pub const fn is_literal(&self) -> bool {
        let i = self.raw();
        object_test(i) && ({let cl = (i&7) as ClassIndex; cl > classBlockClosure && cl!= classSmallInteger})
    }
    #[inline]
    pub const fn is_on_heap(&self) -> bool {
        on_heap(self.raw())
    }
    #[inline]
    pub const fn is_object(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as ClassIndex==classObject
    }
    #[inline]
    pub const fn is_closure(&self) -> bool {
        let i = self.raw();
        object_test(i) && (i&7) as ClassIndex==classBlockClosure
    }
    #[inline]
    pub fn is_self_symbol(&self) -> bool {
        *self==selfSymbolObject
    }
    pub fn class_name(&self) -> &str {
        crate::class::name_str(self.class())
    }
    #[inline]
    pub const fn arity(&self) -> u8 {
        (self.raw()>>28) as u8
    }
    #[inline]
    pub const fn immediateHash(&self) -> u32 {
        (self.raw()>>3) as u32
    }
    #[inline]
    pub fn hash(&self) -> usize {
        if on_heap(self.raw()) {
            (unsafe{&*self.as_object_ptr()}).identityHash()
        } else {
            self.immediateHash() as usize&hashMask
        }
    }
    #[inline]
    pub fn class(&self) -> ClassIndex {
        if self.is_double() {
            classFloat
        } else {
            let cl = (self.raw() & 7) as ClassIndex;
            if cl == classObject {
                (unsafe{&*self.as_object_ptr()}).class()
            } else {
                cl
            }
        }
    }
    #[inline]
    pub const fn dispatch_class(&self,class:ClassIndex) -> ClassIndex {
        // lookups for super methods must add entry to dispatch that maps to something unique: Symbol hash + (target class << 20)
        if self.is_integer() {(self.raw()>>35) as ClassIndex} else {class}
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
    pub const fn as_object_ptr(&self) -> * mut HeapObject {
//        unsafe{& mut *((self.i-NAN_VALUE+1) as * mut HeapObject)}
        unsafe{& mut *(((self.i<<13) as usize>>16<<3) as * mut HeapObject)}
    }
}
impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        if self.is_symbol() {
            write!(f, "#{}",crate::symbol::str_of(*self))
        } else if self.is_integer() {
            write!(f, "{}",self.as_i48())
        } else if self.is_literal() {
            if *self==nilObject {
                write!(f,"nil")
            } else if *self==trueObject {
                write!(f,"true")
            } else if *self==falseObject {
                write!(f,"false")
            } else {
                write!(f, "{:?}",self.as_literal() as u8 as char)
            }
        } else if self.is_double() {
            write!(f, "{:e}",self.as_f64())
        } else if self.is_object() {
            write!(f, "{:?}", self.as_object_ptr())
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
impl From<&'static str> for Object {
    #[inline]
    fn from(symbol:&'static str) -> Self {
        crate::symbol::intern(symbol)
    }
}
impl From<& HeapObject> for Object {
    #[inline]
    fn from(o: &HeapObject) -> Self {
        Object::from(o as *const HeapObject)
    }
}
impl From<&mut HeapObject> for Object {
    #[inline]
    fn from(o: &mut HeapObject) -> Self {
        Object::from(o as * const HeapObject)
    }
}
impl From<* const HeapObject> for Object {
    #[inline]
    fn from(o: * const HeapObject) -> Self {
        Object {i:(o as isize)+NAN_VALUE-1}
    }
}
impl From<* mut HeapObject> for Object {
    #[inline]
    fn from(o: * mut HeapObject) -> Self {
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

const hashShift: usize = 16;
const classMask: usize = (1<<hashShift)-1;
const hashMask: usize = 0xfffff;

impl HeapHeader {
    #[inline]
    fn class(&self) -> ClassIndex {
        ((unsafe{self.u})&classMask)  as ClassIndex
    }
    pub fn class_name(&self) -> &str {
        crate::class::name_str(self.class())
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
    fn raw(&self) -> usize {
        unsafe{self.u}
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
impl From<Object> for &mut HeapObject {
    #[inline]
    fn from(o:Object) -> Self {
        unsafe{&mut *o.as_object_ptr()}
    }
}
impl Debug for HeapHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        let class = self.class();
        write!(f, "{}{}:{}",self.class_name(),(if crate::class::is_meta(class){" class"}else{""}),self.hash())
    }
}
macro_rules! header {
    (@field class = $e:expr) => {($e as usize)};
    (@field immutable) => {1<<47};
    (@field size = $e:expr) => {($e)<<48};
    (@field hash = $e:expr) => {(($e)&hashMask)<<hashShift};
    (@field format = $e:expr) => {($e)<<40};
    (@field forward) => {1<<63};
    ($($i:ident $( = $e:expr)?),+) => {HeapHeader{u:0_usize $(| (header!(@field $i $( = $e)? )))+}}
}
macro_rules! objects {
    ($($e:expr),*) => {&[$( Object::from($e)),*]}
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

pub const format_inst:isize = 10;
const format_idx:isize = 11;
const format_idx_inst:isize = 12;
const format_idx_inst_weak:isize = 13;
pub const format_inst_np:isize = 14;
const format_idx_np:isize = 15;
const format_idx_inst_np:isize = 16;
const format_idx_other_base:isize = 16;
const format_idx_other_8:isize = 17;
const format_idx_other_4:isize = 18;
const format_idx_other_2:isize = 20;
const format_idx_other_1:isize = 24;
#[inline]
fn formatAndSize(n_instVars:usize,n_indexed:isize,width:isize,weak:bool) -> (usize,usize,isize) {
    if weak {
        (format_idx_inst_weak as usize, n_instVars, n_instVars as isize + std::cmp::max(0,n_indexed) + 2)
    } else if n_indexed < 0 {
        (format_inst_np as usize, n_instVars, n_instVars as isize +1)
    } else if n_instVars > 0 {
        (format_idx_inst_np as usize, n_instVars, n_instVars as isize + n_indexed + 2)
    } else if width<0 {
        if n_indexed < 32767 {
            (format_idx_np as usize, n_indexed as usize, n_indexed + 1)
        } else {
            (format_idx_inst_np as usize, 0, n_indexed + 2)
        }
    } else {
        let items_per_word = 8 / width as usize;
        let mut remainder = n_indexed as usize%items_per_word;
        if remainder==0 {remainder=items_per_word}
        let words = (n_indexed as usize + items_per_word - 1) / items_per_word;
        if words < 32767 {
            (format_idx_other_base as usize+items_per_word*2-remainder, words, words as isize + 1)
        } else {
            (format_idx_other_base as usize+items_per_word*2-remainder, 32767, words as isize + 2)
        }
    }
}
#[cfg(test)]
mod testFormatAndSize {
    use super::*;
    #[test]
    fn sizes() {
        assert_eq!(formatAndSize(3,-1,-1,false),(format_inst_np as usize,3,4));
        assert_eq!(formatAndSize(0,5,-1,false),(format_idx_np as usize,5,6));
        assert_eq!(formatAndSize(3,-1,-1,true),(format_idx_inst_weak as usize,3,5));
        assert_eq!(formatAndSize(0,5,-1,true),(format_idx_inst_weak as usize,0,7));
        assert_eq!(formatAndSize(0,40000,-1,false),(format_idx_inst_np as usize,0,40002));
        assert_eq!(formatAndSize(0,40000,-1,false),(format_idx_inst_np as usize,0,40002));
        assert_eq!(formatAndSize(5,40000,-1,false),(format_idx_inst_np as usize,5,40007));
        assert_eq!(formatAndSize(5,20000,-1,false),(format_idx_inst_np as usize,5,20007));
        assert_eq!(formatAndSize(0,40000,8,false),(format_idx_other_8 as usize,32767,40002));
        assert_eq!(formatAndSize(0,20000,8,false),(format_idx_other_8 as usize,20000,20001));
        assert_eq!(formatAndSize(0,17,8,false),(format_idx_other_8 as usize,17,18));
        assert_eq!(formatAndSize(0,17,4,false),(format_idx_other_4 as usize+1,9,10));
        assert_eq!(formatAndSize(0,17,2,false),(format_idx_other_2 as usize+3,5,6));
        assert_eq!(formatAndSize(0,17,1,false),(format_idx_other_1 as usize+7,3,4));
        assert_eq!(formatAndSize(0,18,2,false),(format_idx_other_2 as usize+2,5,6));
        assert_eq!(formatAndSize(0,20,1,false),(format_idx_other_1 as usize+4,3,4));
        assert_eq!(formatAndSize(0,16,4,false),(format_idx_other_4 as usize,8,9));
        assert_eq!(formatAndSize(0,16,2,false),(format_idx_other_2 as usize,4,5));
        assert_eq!(formatAndSize(0,16,1,false),(format_idx_other_1 as usize,2,3));
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
    pub fn class(&self) -> ClassIndex {
        self.header.class()
    }
    #[inline]
    pub fn format(&self) -> usize {
        self.header.format()
    }
    #[inline]
    pub fn header_size(&self) -> usize {
        self.header.size()
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
    pub fn is_indexable(&self) -> bool {
        self.header.format()&27!=10
    }
    #[inline]
    fn firstIndex(&self) -> usize {
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
    pub fn words(&self) -> usize {
        let size = self.header.size();
        let format = self.header.format();
        if format&26==10 {
            size+1
        } else if format<=16 {
            size+2+self.raw_at(size).raw()
        } else if size<32767 {
            size+1
        } else {
            2+self.raw_at(size).raw()
        }
    }
    #[inline]
    pub fn size(&self) -> usize {
        let mut size = self.header.size();
        let format = self.header.format();
        if format&27==10 {
            0
        } else if format&27==11 {
            size
        } else if format<=16 {
            self.raw_at(size).raw()
        } else {
            if size>=32767 {
                size = self.raw_at(0).raw()
            }
            if format>=24 {
                size*8-(format&7)
            } else if format>=20 {
                size*4-(format&3)
            } else if format>=18 {
                size*2-(format&1)
            } else {
                size
            }
        }
    }
    #[inline]
    pub fn width(&self) -> isize {
        let mut size = self.header.size();
        let format = self.header.format();
        if format<=16 {
            -1
        } else if format>=24 {
            1
        } else if format>=20 {
            2
        } else if format>=18 {
            4
        } else {
            8
        }
    }
    #[inline]
    pub fn raw_at(&self,index:usize) -> Object {
        let fields = &self.fields as * const Object;
        unsafe{fields.offset(index as isize).read()}
    }
    #[inline]
    pub fn at(&self,index:usize) -> Object {
        let format = self.header.format();
        if format&27==10 {
            panic!("at for non-indexable")
        }
        let fields = &self.fields as * const Object;
        unsafe{fields.offset(index as isize).read()}
    }
    #[inline]
    pub fn raw_at_put(&mut self,index:usize,value:Object) {
        let fields = &mut self.fields as * mut Object;
        unsafe{fields.offset(index as isize).write(value)}
    }
    #[inline]
    pub fn at_u8(& self,index:usize) -> u8 {
        let format = self.header.format();
        if format<24 {
            panic!("at_put_u8 for non-indexable")
        }
        let fields = &self.fields as * const Object as * const u8;
        unsafe{fields.offset(index as isize).read()}
    }
    #[inline]
    pub fn at_put_u8(&mut self,index:usize,value:u8) {
        let format = self.header.format();
        if format<24 {
            panic!("at_put_u8 for non-indexable")
        }
        let fields = &self.fields as * const Object as * mut u8;
        unsafe{fields.offset(index as isize).write(value)};
    }
    #[inline]
    pub fn at_u16(& self,index:usize) -> u16 {
        let format = self.header.format();
        if format>=24 || format<20 {
            panic!("at_u16 for non-indexable")
        }
        let fields = &self.fields as * const Object as * const u16;
        unsafe{fields.offset(index as isize).read()}
    }
    #[inline]
    pub fn at_put_u16(&mut self,index:usize,value:u16) {
        let format = self.header.format();
        if format>=24 || format<20 {
            panic!("at_put_u16 for non-indexable")
        }
        let fields = &self.fields as * const Object as * mut u16;
        unsafe{fields.offset(index as isize).write(value)};
    }
    #[inline]
    pub fn at_u32(& self,index:usize) -> u32 {
        let format = self.header.format();
        if format>=20 || format<18 {
            panic!("at_u32 for non-indexable")
        }
        let fields = &self.fields as * const Object as * const u32;
        unsafe{fields.offset(index as isize).read()}
    }
    #[inline]
    pub fn at_put_u32(&mut self,index:usize,value:u32) {
        let format = self.header.format();
        if format>=20 || format<18 {
            panic!("at_put_u32 for non-indexable")
        }
        let fields = &self.fields as * const Object as * mut u32;
        unsafe{fields.offset(index as isize).write(value)};
    }
    #[inline]
    pub fn at_u64(& self,index:usize) -> u64 {
        let format = self.header.format();
        if format!=17 {
            panic!("at_put_u64 for non-indexable")
        }
        let fields = &self.fields as * const Object as * const u64;
        unsafe{fields.offset(index as isize).read()}
    }
    #[inline]
    pub fn at_put_u64(&mut self,index:usize,value:u64) {
        let format = self.header.format();
        if format!=17 {
            panic!("at_put_u64 for non-indexable")
        }
        let fields = &self.fields as * const Object as * mut u64;
        unsafe{fields.offset(index as isize).write(value)};
    }
    pub fn initialize(&mut self) {
        for i in 0..self.n_instVars() {
            self.raw_at_put(i,Default::default())
        };
        let init = {if self.header.format()<=16 {Default::default()} else {zeroObject}};
        let first = self.firstIndex();
        for i in first..self.words()-1 {
            self.raw_at_put(i,init)
        };
    }
    pub fn alloc(&mut self,class:ClassIndex,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool) -> * mut HeapObject {
        let (format,size,total_size)=formatAndSize(n_instVars,n_indexed,width,weak);
        let self_hash = self as *mut HeapObject as usize >> 3;
        let hash = if hash==0 {self_hash} else {hash};
        self.header=header!(class=class,hash=hash,format=format,size=size);
        let extra = self.firstIndex();
        if extra>self.n_instVars() {self.raw_at_put(extra-1,Object{i:n_indexed})};
        unsafe{(self as *mut HeapObject).offset(total_size)}
    }
    #[cfg(test)]
    pub fn init(&mut self,class:ClassIndex,fields:&[Object]) -> * mut HeapObject {
        let result = self.alloc(class,fields.len(),-1,-1,0,false);
        for (i,obj) in fields.iter().enumerate() {
            self.raw_at_put(i,*obj)
        }
        result
    }
    #[cfg(test)]
    pub fn array(&mut self,elements:&[Object]) -> * mut HeapObject {
        let result = self.alloc(classArray,0,elements.len() as isize,-1,0,false);
        for (i,obj) in elements.iter().enumerate() {
            self.raw_at_put(i,*obj)
        }
        result
    }
}
pub trait HeapObj {
    fn len(&self) -> usize;
}
impl HeapObj for *mut HeapObject {
    #[inline]
    fn len(&self) -> usize {
        (unsafe{&**self}).size()
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
        let n = unsafe{(end as *const usize).offset_from(array.as_ptr()) as usize}+1;
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
        let uninit = 0xdead0000dead_usize;
        let mut mem = [uninit;100000];
        let one = Object::from(1);
        let two = Object::from(2);
        let next: * mut HeapObject = &mut mem as * const usize as * mut HeapObject;
        let obj1 = unsafe { next.as_mut().unwrap() };
        let next = obj1.alloc(classArray,0,5,-1,0x101,false);
        obj1.initialize();
        obj1.raw_at_put(0,falseObject);
        obj1.raw_at_put(1,one);
        obj1.raw_at_put(2,two);
        let obj2 = unsafe { next.as_mut().unwrap() };
        let next = obj2.alloc(classClass,3,-1,-1,0x202,false);
        obj2.initialize();
        obj2.raw_at_put(0,trueObject);
        obj2.raw_at_put(1,one);
        let obj3 = unsafe { next.as_mut().unwrap() };
        let next = obj3.alloc(classBehavior,0,-1,-1,0x303,false);
        obj3.initialize();
        let object3 = Object::from(obj3);
        let end = next;
        let obj4 = unsafe { next.as_mut().unwrap() };
        let end = obj4.init(classBehavior,&[one,nilObject,object3,def,Object::from(3.14),Object::from('X'),trueObject]);
        print_first(&mem,end);
        assert_eq!(mem[0],0x00050f0001010009); // obj1
        assert_eq!(mem[1],falseObject.raw());
        assert_eq!(mem[2],one.raw());
        assert_eq!(mem[3],two.raw());
        assert_eq!(mem[4],def.raw());
        assert_eq!(mem[5],def.raw());
        assert_eq!(mem[6],0x00030e000202000b); // obj2
        assert_eq!(mem[7],trueObject.raw());
        assert_eq!(mem[8],one.raw());
        assert_eq!(mem[9],def.raw());
        assert_eq!(mem[10],0x00000e000303000d); // obj3
//        assert_eq!(mem[11],0x00050e000303000c); // obj4
        assert_eq!(mem[12],one.raw());
        assert_eq!(mem[13],nilObject.raw());
        assert_eq!(mem[14],object3.raw());
        assert_eq!(mem[15],def.raw());
        assert_eq!(mem[16],Object::from(3.14).raw());
        assert_eq!(mem[17],Object::from('X').raw());
        assert_eq!(mem[18],trueObject.raw());
        assert_eq!(mem[19],uninit);
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
        for iv in  0..self.n_instVars() {
            write!(f, " {:?}",self.raw_at(iv));
        };
        if self.is_indexable() {
            let size = self.size();
            let width = self.width();
            write!(f,"(");
            let mut prefix = "";
            for idx in 0..size {
                match width {
                    1 => write!(f,"{}{}",prefix,self.at_u8(idx)),
                    2 => write!(f,"{}{}",prefix,self.at_u16(idx)),
                    4 => write!(f,"{}{}",prefix,self.at_u32(idx)),
                    8 => write!(f,"{}{}",prefix,self.at_u64(idx)),
                    _ => write!(f,"{}{:?}",prefix,self.at(idx)),
                };
                prefix = " ";
            };
            write!(f,")");
        };
        write!(f,")")
    }
}
impl PartialEq for HeapObject {
    fn eq(&self,other:&HeapObject) -> bool {
        self==other
    }
}
