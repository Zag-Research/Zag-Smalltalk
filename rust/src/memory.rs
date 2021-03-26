use std::mem;


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
        let size = ((size+min_page_size-1)&(-min_page_size)) as usize/mem::size_of::<Object>();
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
        if data != self.base {
            panic!("data mapped at wrong address")
        }
        self.end = unsafe{data.offset(self.size as isize)};
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
impl Memory {
    pub fn assignObject(& mut self,target: Object, offset: isize,value: Object) {
    }
    pub fn allocObject(& mut self,size:isize) {
    }
    pub fn minorGC(&mut self) {
    }
    pub fn fullGC(&mut self) {
    }
    pub fn becomeX(& mut self,target: Object, value: Object) {
        if !target.is_heap_object() || !value.is_heap_object() {
            panic!("can't become a fixed value")
        }
        if (unsafe{value.i<target.i}) {
            self.becomeX(value,target)
        } else {

        }
    }
}
pub const gc_main : usize = 0x100000000000;
pub const gc_size : isize = 0x000001000000;
