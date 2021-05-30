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
mod testMemory {
    use super::*;
    #[test]
    fn test1() {
        memory.with(|n| n.borrow_mut().allocObject(classArray,0,10,-1,0,false));
    }
}
