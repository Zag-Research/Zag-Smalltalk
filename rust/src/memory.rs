use std::mem;


extern crate libc;
pub struct AllocableRegion {
    base : * mut libc::c_void,
    size : usize,
    end : * mut Object,
    current : * mut Object,
}
const min_page_size : isize = 16384;
fn bytesRoundedToPageSize(size:isize) -> usize {
    ((size+min_page_size-1)&(-min_page_size)) as usize
}
impl AllocableRegion {
    pub fn new(address: usize,size:isize) -> Self {
        let address = address as * mut libc::c_void;
        let end = address as *mut Object;
        AllocableRegion {
            base : address,
            size : bytesRoundedToPageSize(size),
            end : end,
            current : end,
        }
    }
    pub fn mapMemory(& mut self) {
        let data = unsafe{
            libc::mmap(
                /* addr: */ self.base,
                /* len: */ self.size,
                /* prot: */ libc::PROT_READ | libc::PROT_WRITE,
                /* flags: */ libc::MAP_ANON,
                /* fd: */ -1,
                /* offset: */ 0,
            )};
        if data == libc::MAP_FAILED {
            panic!("Could not memory map")
        }
        if data != self.base {
            panic!("data mapped at wrong address")
        }
        let data = data as *mut Object;
        self.end = unsafe{data.offset((self.size/mem::size_of::<Object>) as isize)};
    }
    pub fn releaseMemory(& mut self) {
        let start = self.base as *mut Object;
        self.end = start;
        self.current = start;
        if -1 == unsafe{
            libc::munmap(
                /* addr: */ self.base,
                /* len: */ self.size,
            )} {
            panic!("Failed to release memory map")
        }
    }
    pub fn allocObject(& mut self,size:isize) -> * mut Object {
        let new = self.current;
        let mut next = unsafe{self.current.offset(size)};
        if next > self.end {
            self.gc(&self); // shouldn't be self!!!
            self.allocObject(size)
        } else {
            // have to set the header and initialize the object
            new
        }
    }
    pub fn gc(& mut self,sink: & mut AllocableRegion) {
        panic!("trying to gc")
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
