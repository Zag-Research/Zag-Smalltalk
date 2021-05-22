use std::mem;
use crate::object::*;
extern crate libc;

pub struct AllocableRegion {
    base: * mut HeapObject,
    size: usize,
    end: * mut HeapObject,
    current: * mut HeapObject,
}
unsafe impl Send for AllocableRegion {}
unsafe impl Sync for AllocableRegion {}

const min_page_size: isize = 16384;
fn bytesRoundedToPageSize(size:isize) -> usize {
    ((size+min_page_size-1)&(-min_page_size)) as usize
}
impl AllocableRegion {
    fn makeLocal(memory:&mut [usize]) -> Self {
        let start = memory.as_mut_ptr() as *mut HeapObject;
        let size = memory.len();
        AllocableRegion {
            base: start,
            size: size*mem::size_of::<HeapObject>(),
            end: (unsafe{start.offset(size as isize)}),
            current: start,
        }
    }
    fn new(address: usize,size:isize) -> Self {
        let end = address as *mut HeapObject;
        AllocableRegion {
            base: end,
            size: bytesRoundedToPageSize(size),
            end: end,
            current: end,
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
        self.end = unsafe{data.offset((self.size/mem::size_of::<Object>()) as isize)};
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
    pub fn allocObject(& mut self,size:isize) -> Option<* mut HeapObject> {
        let new = self.current;
        let mut next = unsafe{self.current.offset(size)};
        if next > self.end {
            None
        } else {
            // have to set the header and initialize the object
            Some(new)
        }
    }
    #[cfg(not(test))]
    pub fn gc(& mut self,sink: & mut AllocableRegion) {
        panic!("trying to gc")
    }
}

const NurserySize : usize = 25000;
#[thread_local]
static mut NurseryMemory : [usize;NurserySize] = [0;NurserySize];
thread_local! {
    static Nursery: AllocableRegion = AllocableRegion::makeLocal(unsafe{&mut NurseryMemory});
}
use std::sync::RwLock;
pub struct Memory {
    genOld1: AllocableRegion,
    genOld2: AllocableRegion,
    genTeen: AllocableRegion,
    nursery: AllocableRegion,
}
impl Memory {
    pub fn assignObject(& mut self,target: Object, offset: isize,value: Object) {
    }
    pub fn allocObject(& mut self,size:isize) {
        // get code from HeapObject
        // need to lock long enough to allocate space for object
        // then can't allow GC until the object is initialized
    }
    pub fn minorGC(&mut self) {
        panic!("not implemented")
    }
    pub fn fullGC(&mut self) {
        panic!("not implemented")
    }
    pub fn becomeX(& mut self,target: Object, value: Object) {
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
pub const gc_main: usize = 0x100000000000;
pub const gc_size: isize = 0x000001000000;
#[cfg(testx)]
mod testMemory {
    use super::*;
    #[test]
    fn test1() {

    }
}
