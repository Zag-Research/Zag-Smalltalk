use std::mem;
use std::cell::RefCell;
use std::sync::RwLock;
use std::sync::Arc;
use crate::object::*;
extern crate libc;

enum RegionType {
    Nursery,
    NurseryWithBackup(Arc<RwLock<AllocableRegion>>),
    // TeenWithBackup(Arc<RwLock<AllocableRegion>>),
    Old, // should go away
    PrimaryOld(Arc<RwLock<AllocableRegion>>),
    SecondaryOld(Arc<RwLock<AllocableRegion>>),
}
pub struct AllocableRegion {
    base: * mut HeapObject,
    size: usize,
    end: * mut HeapObject,
    current: * mut HeapObject,
    region_type: RegionType,
}
unsafe impl Send for AllocableRegion {}
unsafe impl Sync for AllocableRegion {}

const gc_main: usize = 0x100000000000;
const gc_size: isize = 0x000001000000;
const min_page_size: isize = 16384;
fn bytesRoundedToPageSize(size:isize) -> usize {
    ((size+min_page_size-1)&(-min_page_size)) as usize
}
impl AllocableRegion {
    fn makeLocal(mem:&mut [usize]) -> Self {
        let start = mem.as_mut_ptr() as *mut HeapObject;
        let size = mem.len();
        AllocableRegion {
            base: start,
            size: size*mem::size_of::<HeapObject>(),
            end: (unsafe{start.offset(size as isize - 1)}),
            current: start,
            region_type: RegionType::Nursery,
        }
    }
    fn new(address: usize,size:isize) -> Self {
        let end = address as *mut HeapObject;
        AllocableRegion {
            base: end,
            size: bytesRoundedToPageSize(size),
            end: end,
            current: end,
            region_type: RegionType::Old,
        }
    }
    fn mapMemory(& mut self) {
        let data = unsafe{
            libc::mmap(
                /* addr: */ self.base as * mut libc::c_void,
                /* len: */ self.size,
                /* prot: */ libc::PROT_READ | libc::PROT_WRITE,
                /* flags: */ libc::MAP_ANON,
                /* fd: */ -1,
                /* offset: */ 0,
            )};
        if data == libc::MAP_FAILED {
            panic!("Could not memory map")
        }
        let data = data as *mut HeapObject;
        if data != self.base {
            panic!("data mapped at wrong address")
        }
        self.end = unsafe{data.offset((self.size/mem::size_of::<Object>()) as isize - 1)};
    }
    pub fn releaseMemory(& mut self) {
        self.end = self.base;
        self.current = self.base;
        if -1 == unsafe{
            libc::munmap(
                /* addr: */ self.base as * mut libc::c_void,
                /* len: */ self.size,
            )} {
            panic!("Failed to release memory map")
        }
    }
    pub fn allocObject(&mut self,class:u16,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool) -> Option<* mut HeapObject> {
        let new = self.current;
        let mut next = (unsafe{&mut*new}).alloc(class,n_instVars,n_indexed,width,hash,weak);
        if next > self.end {
            None
        } else {
            self.current = next;
            (unsafe{&mut*new}).initialize();
            Some(new)
        }
    }
    #[cfg(test)]
    pub fn memory_used(&mut self) -> isize {
        unsafe{self.current.offset_from(self.base)}
    }
    #[cfg(test)]
    pub fn init(&mut self,class:u16,fields:&[Object]) -> * mut HeapObject {
        let new = self.current;
        let mut next = (unsafe{&mut*new}).init(class,fields);
        if next > self.end {
            panic!("allocation past end of memory")
        } else {
            self.current = next;
        }
        new
    }
    pub fn init_str(&mut self,string:&str) -> * mut HeapObject {
        let mut max_width:isize = 0;
        let mut len = 0;
        for c in string.chars() {
            len += 1;
            max_width = std::cmp::max(max_width,c.len_utf8() as isize)
        }
        if max_width==3 {max_width=4}
        if let Some(result) = self.allocObject(classString,0,len,max_width,0,false) {
            let obj = unsafe{result.as_ref().unwrap()};
            println!("max: {} len: {} format: {} size: {}",max_width,len,obj.format(),obj.header_size());
            for (i,c) in string.chars().enumerate() {
                if max_width==1 {
                    obj.at_put_u8(i,c as u8)
                } else {
                    obj.at_put_u32(i,c as u32)
                };
            }
            result
        } else {
            panic!("failed to allocate string")
        }
    }
    pub fn assignObject(& mut self,target: Object, offset: isize,value: Object) {
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
    pub static memory: RefCell<AllocableRegion> = RefCell::new(AllocableRegion::makeLocal(unsafe{&mut NurseryMemory}));
}
pub fn assignObject(target: Object, offset: isize,value: Object) {
    memory.with(|mem| mem.borrow_mut().assignObject(target,offset,value))
}
pub fn allocObject(class:u16,n_instVars:usize,n_indexed:isize,width:isize,hash:usize,weak:bool) -> Option<* mut HeapObject> {
    memory.with(|mem| mem.borrow_mut().allocObject(class,n_instVars,n_indexed,width,hash,weak))
}
#[cfg(test)]
pub fn init(class:u16,fields:&[Object]) -> * mut HeapObject {
    memory.with(|mem| mem.borrow_mut().init(class,fields))
}
pub fn init_str(string:&str) -> * mut HeapObject {
    memory.with(|mem| mem.borrow_mut().init_str(string))
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
        memory.with(|n| n.borrow_mut().allocObject(classArray,0,10,-1,0,false));
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
