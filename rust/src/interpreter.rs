use crate::object::*;
pub struct Thread {
    stack:Vec<Object>,
}
impl Thread {
    #[inline]
    pub fn push(&mut self,o:Object) {
        self.stack.push(o)
    }
    #[inline]
    pub fn push_i48(&mut self,i:isize) {
        self.stack.push(Object::from(i))
    }
    #[inline]
    pub fn top(&self) -> Object {
        self.stack[self.stack.len()-1]
    }
    #[inline]
    pub fn pop(&mut self) -> Object {
        self.stack.pop().unwrap()
    }
    #[inline]
    pub fn atOffset(&self,position:usize) -> Object {
        self.stack[self.stack.len()-position]
    }
    #[inline]
    pub fn at(&self,position:usize) -> Object {
        self.stack[position]
    }
    #[inline]
    pub fn at_put(&mut self,position:usize,value:Object) {
        self.stack[position]=value;
    }
    #[inline]
    pub fn offset(&self,offset:usize) -> usize {
        self.stack.len()-offset-1
    }
    #[inline]
    pub fn reserve(&mut self,space:usize) {
        self.stack.resize(self.stack.len()+space,nilObject);
    }
    #[inline]
    pub fn pop_to(&mut self,position:usize) {
        self.stack.truncate(position+1);
    }
    #[inline]
    pub fn discard(&mut self,position:usize) {
        self.stack.truncate(self.stack.len()-position);
    }
    #[inline]
    pub fn append(&mut self,other:&mut Vec<Object>) {
        self.stack.append(other);
    }
}
impl Default for Thread {
    fn default() -> Self {
        Thread{ stack: vec![placeholderObject]}
    }
}
#[derive(Eq,PartialEq,Debug)]
pub enum FunctionResult {
    NormalReturn,
    NonLocalReturn,
    Branch(isize),
    ExceptionSignaled,
}
use FunctionResult::*;

#[macro_export]
macro_rules! restack_mask_field {
    () => { 0 };
    ($e:expr) => { $e };
    ($e:expr, $($es:expr),+) => { (restack_mask_field!($($es),*))<<5 | $e};
}
#[macro_export]
macro_rules! restack_mask {
    ($d:expr => $($es:expr),*) => { Object::from((
        (restack_mask_field!($($es),*))<<8 | $d
            ) as isize)
    }
}

type ThreadedFunction = fn(&mut Thread,Object) -> FunctionResult;
pub mod stack;
#[derive(Default,Clone)]
pub struct Method {
    class_index: u16,
    parameters: u8,
    locals: u8,
    symbol_index: u32,
    code: Vec<(ThreadedFunction,Object)>,
}
const Max_Method_Size:isize = 32000;
impl Method {
    pub fn new(class_index: u16,parameters: u8,locals: u8,symbol_index: u32) -> Self {
        Method{
            class_index,
            parameters,
            locals,
            symbol_index,
            code: Vec::new(),
        }
    }
    pub fn instr(&mut self,f:ThreadedFunction) {
        self.code.push((f,placeholderObject))
    }
    pub fn instr_with(&mut self,f:ThreadedFunction,o:Object) {
        self.code.push((f,o))
    }
    pub fn instr_i48(&mut self,f:ThreadedFunction,i:isize) {
        self.code.push((f,Object::from(i)))
    }
    pub fn execute(&self,thread:&mut Thread) -> FunctionResult {
        let mut pc:usize = 0;
        let end = self.code.len()-1;
        let self_index = thread.offset(self.parameters as usize);
        thread.reserve(self.locals as usize);
        loop {
            if pc==end {break};
            let (f,o)=self.code[pc];
            match f(thread,o) {
                NormalReturn => { pc = pc + 1},
                Branch(offset) => { pc = (pc as isize + offset + 1) as usize},
                NonLocalReturn => {panic!("non-local return")},
                ExceptionSignaled => {panic!("exception signaled")},
            }
        };
        let (f,o)=self.code[pc];
        f(thread,o)
    }
}
type Function = fn(&mut Thread,Option<&Method>) -> FunctionResult;
mod primitives {
    use super::*;
    pub mod object {
        use super::*;
        pub fn yourself(thread:&mut Thread,_:Option<&Method>) -> FunctionResult {
            NormalReturn
        }
    }
    pub mod smallInteger {
        use super::*;
        pub fn add(thread:&mut Thread,_:Option<&Method>) -> FunctionResult {
            let self_index = thread.stack.len()-2;
            let this = thread.at(self_index);
            let other = thread.at(self_index+1);
            if this.is_integer() && other.is_integer() {
                thread.at_put(self_index,Object::from(this.as_i48()+other.as_i48()));
                thread.pop_to(self_index)
            } else {
                panic!("not SmallIntegers")
            };
            NormalReturn
        }
    }
}
#[derive(Clone)]
struct MethodMatch {
    function: Function,
    method: Method,
}
impl MethodMatch {
    fn getMethod(&self,symbol_index:u32) -> Option<&Self> {
        if self.method.symbol_index == symbol_index {
            Some(&self)
        } else {
            None
        }
    }
}
impl Default for MethodMatch {
    fn default() -> Self {
        MethodMatch{function:primitives::object::yourself,method:Default::default()}
    }
}
pub struct Dispatch {
    class: Object,
    table: Box<[MethodMatch]>,
}
impl Dispatch {
    fn getMethod(&self,symbol_hash:u32) -> Option<&MethodMatch> {
        let len = self.table.len();
        let hash = symbol_hash as usize%len;
        for index in (hash..len).into_iter().chain((0..hash).into_iter()) {
            let m = self.table[index].getMethod(symbol_hash);
            if m.is_some() {return m}
        }
        None
    }
}
const MAX_CLASSES: usize = 1000;
use std::mem::ManuallyDrop;
type TDispatch = ManuallyDrop<Option<Dispatch>>;
const NO_DISPATCH: TDispatch = ManuallyDrop::new(None);
static mut dispatchTable: [TDispatch;MAX_CLASSES] = [NO_DISPATCH;MAX_CLASSES];
use std::sync::RwLock;

lazy_static!{
    static ref dispatchFree: RwLock<usize> = RwLock::new(0);
}

pub fn addClass(c: Object, n: usize) {
    let mut index = dispatchFree.write().unwrap();
    let pos = *index;
    if pos >= MAX_CLASSES {panic!("too many classes")}
    *index = pos + 1;
    replaceDispatch(pos,c,n);
}
pub fn replaceDispatch(pos: usize, c: Object, n: usize) -> Option<Dispatch> {
    let mut table = Vec::with_capacity(n);
    table.resize(n,Default::default());
    unsafe {
        let old = std::mem::replace(
            &mut dispatchTable[pos],
            ManuallyDrop::new(Some(Dispatch {
                class: c,
                table: table.into_boxed_slice(),
            })),
        );
        ManuallyDrop::into_inner(old)
    }
}
fn dispatch(thread:&mut Thread,selector:Object) -> FunctionResult {
    let selector_hash = selector.immediateHash();
    let arity = selector_hash>>25;
    let this = thread.stack[thread.stack.len()-(arity as usize)-1];
    if let Some(disp) = unsafe{&dispatchTable[this.class() as usize].take()} {
        if let Some(MethodMatch{function,method}) = disp.getMethod(selector_hash) {
            function(thread,Some(method))
        } else {
            panic!("no method found")
        }
    } else {
        panic!("no dispatch found")
    }
}

#[cfg(test)]
mod testsInterpreter {
    use super::*;
    use crate::symbol::intern;
    
    #[test]
    #[should_panic(expected = "no dispatch found")]
    fn dispatch_non_existant_class() {
        let mut thread:Thread = Default::default();
        dispatch(&mut thread,intern("foo"));
    }
/*    #[test]
    #[should_panic(expected = "no method found")]
    fn dispatch_non_existant_method() {
        dispatch(Object::from(3.14),intern("foo"),nilObject,nilObject,None);
    }*/
}
