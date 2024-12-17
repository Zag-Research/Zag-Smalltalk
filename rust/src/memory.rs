use std::mem;
use std::cell::RefCell;
use std::sync::{RwLock,Mutex,Arc};
use crate::object::*;
extern crate libc;
extern crate errno;

type HOInit<'a> = &'a dyn Fn(&mut HeapObject);
trait AllocableRegion {
    fn assignObject(& mut self,target: Object, offset: isize,value: Object);
    fn allocObject(&mut self,class:ClassIndex,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool,f:HOInit) -> Option<* mut HeapObject>;
    #[cfg(test)]
    fn init(&mut self,class:ClassIndex,fields:&[Object]) -> * mut HeapObject;
}
struct ThreadLocalRegion {
    base: * mut HeapObject,
    end: * mut HeapObject,
    current: * mut HeapObject,
}
impl ThreadLocalRegion {
    fn new(mem:&mut [usize]) -> Self {
        let start = mem.as_mut_ptr() as *mut HeapObject;
        let size = mem.len();
        ThreadLocalRegion {
            base: start,
            end: (unsafe{start.offset(size as isize - 1)}),
            current: start,
        }
    }
    #[cfg(test)]
    fn memory_used(&mut self) -> isize {
        unsafe{self.current.offset_from(self.base)}
    }
}
unsafe impl Send for ThreadLocalRegion {}
unsafe impl Sync for ThreadLocalRegion {}

const gc_primary: usize = 0x100000000000;
const gc_secondary:  usize = 0x180000000000;
const gc_size: isize = 0x000001000000;
const min_page_size: isize = 16384;
impl AllocableRegion for ThreadLocalRegion {
    fn allocObject(&mut self,class:ClassIndex,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool,f:HOInit) -> Option<* mut HeapObject> {
        let new = self.current;
        if new < self.end {
            let mut next = (unsafe{&mut*new}).alloc(class,n_instVars,n_indexed,width,hash,weak);
            if next <= self.end {
                self.current = next;
                f(unsafe{&mut *new});
                return Some(new)
            }
        }
        None
    }
    fn assignObject(& mut self,target: Object, offset: isize,value: Object) {
        panic!("not implemented")
    }
    #[cfg(test)]
    fn init(&mut self,class:ClassIndex,fields:&[Object]) -> * mut HeapObject {
        let new = self.current;
        let mut next = (unsafe{&mut*new}).init(class,fields);
        if next > self.end {
            panic!("insufficient memory for 'init'")
        } else {
            self.current = next;
            new
        }
    }
}
struct MappedRegion {
    base: * mut HeapObject,
    end: * mut HeapObject,
    current: * mut HeapObject,
    other_region: Option<&'static AllocableRegion>,
}
impl AllocableRegion for MappedRegion {
    fn allocObject(&mut self,class:ClassIndex,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool,f:HOInit) -> Option<* mut HeapObject> {
        let new = self.current;
        if new < self.end {
            let mut next = (unsafe{&mut*new}).alloc(class,n_instVars,n_indexed,width,hash,weak);
            if next <= self.end {
                self.current = next;
                f(unsafe{&mut *new});
                return Some(new)
            }
        }
        None
    }
    fn assignObject(& mut self,target: Object, offset: isize,value: Object) {
        panic!("not implemented")
    }
    #[cfg(test)]
    fn init(&mut self,class:ClassIndex,fields:&[Object]) -> * mut HeapObject {
        let new = self.current;
        let mut next = (unsafe{&mut*new}).init(class,fields);
        if next > self.end {
            panic!("insufficient memory for 'init'")
        } else {
            self.current = next;
            new
        }
    }
}
impl MappedRegion {
    #[cfg(test)]
    fn array(&mut self,elements:&[Object]) -> * mut HeapObject {
        let new = self.current;
        let mut next = (unsafe{&mut*new}).array(elements);
        if next > self.end {
            panic!("insufficient memory for 'array'")
        } else {
            self.current = next;
            new
        }
    }
    const fn new(address: usize) -> Self {
        let end = address as *mut HeapObject;
        MappedRegion {
            base: end,
            end: end,
            current: end,
            other_region: None,
        }
    }
    fn is_mapped(&self) -> bool {
        self.end>self.base
    }
    fn mapMemory(& mut self,size:isize) {
        use libc::*;
        use errno::errno;
        let size = ((size+min_page_size-1)&(-min_page_size)) as usize;
        let end = unsafe{self.base.offset((size/mem::size_of::<HeapObject>()) as isize - 1)};
        if self.end < end {
            let data = unsafe{
                mmap(
                    /* addr: */ self.base as * mut c_void,
                    /* len: */ size,
                    /* prot: */ PROT_READ | PROT_WRITE,
                    /* flags: */ MAP_ANON | MAP_SHARED,
                    /* fd: */ -1,
                    /* offset: */ 0,
                )};
            if data == MAP_FAILED {
                panic!("mmap failed; Error: {}",errno())
            }
            let data = data as *mut HeapObject;
            if data != self.base {
                panic!("data mapped at wrong address")
            }
            self.end = end
        }
    }
    fn releaseMemory(& mut self) {
        use libc::*;
        use errno::errno;
        let size = (unsafe{self.end.offset_from(self.base)}+1) as usize*mem::size_of::<HeapObject>();
        self.end = self.base;
        self.current = self.base;
        if -1 == unsafe{
            munmap(
                /* addr: */ self.base as * mut c_void,
                /* len: */ size,
            )} {
            panic!("munmap failed; Error: {}",errno())
        }
    }
    fn collect_from(&mut self,source:&mut AllocableRegion) {
        panic!("collect_from not implemented")
    }
    fn assignObject(& mut self,target: Object, offset: isize,value: Object) {
        panic!("not implemented")
    }
    fn minorGC(&mut self) {
        panic!("not implemented")
    }
    fn fullGC(&mut self) {
        panic!("not implemented")
    }
    fn becomeX(& mut self,target: Object, value: Object) {
        if !target.is_on_heap() || !value.is_on_heap() {
            panic!("can't become a fixed value")
        }
        panic!("not implemented")
/*        if (unsafe{value.i<target.i}) {
            self.becomeX(value,target)
        } else {

        }
*/    }
}

const NurserySize : usize = 25000;
#[thread_local]
static mut NurseryMemory : [usize;NurserySize] = [0;NurserySize];
thread_local! {
    static memory: RefCell<ThreadLocalRegion> = RefCell::new(ThreadLocalRegion::new(unsafe{&mut NurseryMemory}));
}
pub fn assignObject(target: Object, offset: isize,value: Object) {
    memory.with(|mem| mem.borrow_mut().assignObject(target,offset,value))
}
pub fn allocObject(class:ClassIndex,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool) -> * mut HeapObject {
    memory.with(|mem|
                loop {
                    if let Some(result) = mem.borrow_mut().
                        allocObject(class,n_instVars,n_indexed,width,hash,weak,
                                    &| obj: &mut HeapObject | obj.initialize()) {
                            return result
                        }
                    primary_do(&|region| region.collect_from(&mut *mem.borrow_mut()));
                })
}
#[cfg(test)]
pub fn init(class:ClassIndex,fields:&[Object]) -> * mut HeapObject {
    memory.with(|mem| mem.borrow_mut().init(class,fields))
}
pub fn str_shape(string:&str) -> (isize,isize) {
    let mut max_width:isize = 0;
    let mut len = 0;
    for c in string.chars() {
        len += 1;
        max_width = std::cmp::max(max_width,c.len_utf8() as isize)
    }
    if max_width==3 {max_width=4}
    (len,max_width)
}
pub fn init_str(string:&str) -> * mut HeapObject {
    let (len,max_width) = str_shape(&string);
    memory.with(|mem|
                loop {
                    if let Some(result) = mem.borrow_mut().
                        allocObject(classString,0,len,max_width,0,false,
                                    &| obj: &mut HeapObject |
                                    {
                                        let lastIndex = obj.words()-2;
                                        if lastIndex>=0 {obj.raw_at_put(lastIndex,zeroObject)};
                                        for (i,c) in string.chars().enumerate() {
                                            if max_width==1 {
                                                obj.at_put_u8(i,c as u8)
                                            } else {
                                                obj.at_put_u32(i,c as u32)
                                            };
                                        }
                                    }) {
                            return result
                        }
                    primary_do(&|region| region.collect_from(&mut *mem.borrow_mut()));
                })
}
enum WhichRegion {Primary,Secondary,Unitialized}
lazy_static!{
    static ref which_region: Mutex<WhichRegion> = Mutex::new(WhichRegion::Unitialized);
}
static mut primary_region: MappedRegion = MappedRegion::new(gc_primary);
static mut secondary_region: MappedRegion = MappedRegion::new(gc_secondary);
fn primary_do<'a,T>(f:&'a dyn Fn(&mut MappedRegion)-> T) -> T {
    let mut which = which_region.lock().unwrap();
    match &*which {
        Unitialized => {
            *which = WhichRegion::Primary;
            unsafe{primary_region.mapMemory(gc_size)};
            unsafe{secondary_region.mapMemory(gc_size)};
            f(unsafe{&mut primary_region})
        },
        Primary => f(unsafe{&mut primary_region}),
        Secondary => f(unsafe{&mut secondary_region}),
    }
}
#[cfg(test)]
pub fn primary_init(class:ClassIndex,fields:&[Object]) -> Object {
    Object::from(primary_do(&|region| region.init(class,fields)))
}
#[cfg(test)]
mod testFoo {
    use super::*;
    use crate::symbol::*;
    pub fn old_convert(foo: Object, _elements: impl IntoIterator<Item = Object>) -> Object {
        foo
    }
    pub fn new_convert(
        foo: impl Into<Object>,
        elements: impl IntoIterator<Item = impl Into<Object>>,
    ) -> Object {
        old_convert(foo.into(), elements.into_iter().map(Into::into))
    }
    
    #[test]
    fn testOldConvert() {
        old_convert(Object::from(42), [Object::from(3.1415),Object::from("str")]);
    }
/* can never work perfectly because arrays have to be homogeneous
    #[test]
    fn testNewConvert() {
        new_convert(42, [3.1415,"abc"]);
    }
*/
}
#[cfg(test)]
pub fn primary_array(elements:&[Object]) -> Object {
    Object::from(primary_do(&|region| region.array(elements)))
}
pub fn primary_allocObject(class:ClassIndex,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool) -> * mut HeapObject {
    primary_do(&|region|
               loop {
                   if let Some(result) = region.
                       allocObject(class,n_instVars,n_indexed,width,hash,weak,
                                   &| obj: &mut HeapObject | obj.initialize()) {
                           return result
                       }
                   panic!()
               }
    )
}
pub fn primary_init_str(string:&str) -> * mut HeapObject {
    let (len,max_width) = str_shape(&string);
    primary_do(&|region|
                loop {
                    if let Some(result) = region.
                        allocObject(classString,0,len,max_width,0,false,
                                    &| obj: &mut HeapObject |
                                    {
                                        let lastIndex = obj.words()-2;
                                        if lastIndex>=0 {obj.raw_at_put(lastIndex,zeroObject)};
                                        for (i,c) in string.chars().enumerate() {
                                            if max_width==1 {
                                                obj.at_put_u8(i,c as u8)
                                            } else {
                                                obj.at_put_u32(i,c as u32)
                                            };
                                        }
                                    }) {
                            return result
                        }
                   panic!()
                })
}
#[cfg(test)]
#[macro_export]
macro_rules! init {
    ($c:expr => $($e:expr),*) => {init($c,&[$( Object::from($e)),*])}
}
#[cfg(test)]
pub fn memory_used() -> isize {
    memory.with(|mem| mem.borrow_mut().memory_used())
}
#[cfg(test)]
mod testMemory {
    use super::*;
    use crate::symbol::*;
    #[test]
    fn memory_direct() {
        memory.with(|n|
            loop {
                if let Some(result) = n.borrow_mut().allocObject(classArray,0,10,-1,0,false,&|obj| ()) {return}
                panic!("allocation failed")
            }
            )
    }
    #[test]
    fn memory_used_test() {
        let obj1 = init!(classArray => true,42,3.14);
        assert_eq!(memory_used(),4);
    }
    #[test]
    fn inits() {
        let obj1 = init!(classArray => true,42,3.14,1,2,3,4,5);
        assert_eq!(memory_used(),9);
        let obj2 = init!(classClass => intern("foo"),true,3,obj1);
        assert_eq!(memory_used(),14);
    }
    #[test]
    fn init_string() {
        let obj1 = init_str(&"hello world");
        assert_eq!(memory_used(),3);
        let obj2 = init_str(&"fredrick");
        assert_eq!(memory_used(),5);
        let obj3 = init_str(&"❤\n!");
        assert_eq!(memory_used(),8);
        assert_eq!(obj1.len(),11);
        assert_eq!(obj2.len(),8);
        assert_eq!(obj3.len(),3);
    }
}
