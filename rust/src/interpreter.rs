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
    pub fn replace(&mut self,object:Object) {
        let len = self.stack.len()-1;
        self.stack[len]=object
    }
    #[inline]
    pub fn pop(&mut self) -> Object {
        self.stack.pop().unwrap()
    }
    #[inline]
    pub fn atOffset(&self,position:u32) -> Object {
        self.stack[self.stack.len()-position as usize]
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
    pub fn start_method_get_self_index(&self,parameters:u8) {
        self.offset(parameters as usize);
    }
    #[inline]
    pub fn end_method(&self) {}
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
    ReturnIsNext,
}
use FunctionResult::*;

#[macro_export] // use internal rules: https://danielkeep.github.io/tlborm/book/pat-internal-rules.html
macro_rules! restack_mask { 
    (@field) => { 0 };
    (@field $e:expr) => { $e };
    (@field $e:expr, $($es:expr),+) => { (restack_mask!(@field $($es),*))<<5 | $e};
    ($d:expr => $($es:expr),*) => { Object::from((
        (restack_mask!(@field $($es),*))<<8 | $d
            ) as isize)
    }
}

type Function = fn(&mut Thread,Object) -> FunctionResult;
type AddStrPrimitive = fn(&'static str,Function);
type AddI32Primitive = fn(i32,Function);
pub mod stack;
#[derive(Default,Clone)]
pub struct Method {
    class_index: u16,
    parameters: u8,
    locals: u8,
    symbol_index: u32,
    code: Box<Vec<(Function,Object)>>,
}
const Max_Method_Size:isize = 32000;
impl Method {
    pub fn new(class_index: u16,parameters: u8,locals: u8,symbol_index: u32) -> Self {
        Method{
            class_index,
            parameters,
            locals,
            symbol_index,
            code: Box::new(Vec::new()),
        }
    }
    pub fn instr(&mut self,f:Function) {
        self.code.push((f,placeholderObject))
    }
    pub fn instr_with(&mut self,f:Function,o:Object) {
        self.code.push((f,o))
    }
    pub fn instr_i48(&mut self,f:Function,i:isize) {
        self.code.push((f,Object::from(i)))
    }
    pub fn execute(&self,thread:&mut Thread) -> FunctionResult {
        let mut pc:usize = 0;
        let code = &*self.code;
        let end = code.len()-1;
        let self_index = thread.start_method_get_self_index(self.parameters);
        thread.reserve(self.locals as usize);
        loop {
            if pc==end {break};
            let (f,o)=code[pc];
            match f(thread,o) {
                NormalReturn => { pc = pc + 1},
                Branch(offset) => { pc = (pc as isize + offset + 1) as usize},
                ReturnIsNext => { pc = pc + 1;break}
                NonLocalReturn => {panic!("non-local return")},
                ExceptionSignaled => {panic!("exception signaled")},
            }
        };
        let (f,o)=code[pc];
        thread.end_method();
        f(thread,o)
    }
}
mod primitives;
#[derive(Clone)]
enum MethodType {
    Function(Function),
    Method(Method),
    NoType,
}
#[derive(Clone)]
struct MethodMatch {
    selector: u32,
    function: Box<MethodType>,
}
impl MethodMatch {
    fn getMethod(&self,selector:u32) -> &MethodType {
        if self.selector == selector {
            &self.function
        } else {
            &MethodType::NoType
        }
    }
}
impl Default for MethodMatch {
    fn default() -> Self {
        MethodMatch{function:Box::new(MethodType::NoType),selector:Default::default()}
    }
}
pub struct Dispatch {
    class: Object,
    table: Box<[MethodMatch]>,
}
impl Dispatch {
    fn getMethod(&self,selector:Object) -> &MethodType {
        let len = self.table.len();
        let iHash = selector.immediateHash();
        let hash = iHash as usize%len;
        for index in (hash..len).into_iter().chain((0..hash).into_iter()) {
            match self.table[index].getMethod(iHash) {
                MethodType::NoType => {},
                result => {return result},
            }
        }
        &MethodType::NoType
    }
}
const MAX_CLASSES: u16 = 1000;
use std::mem::ManuallyDrop;
type TDispatch = ManuallyDrop<Option<Dispatch>>;
const NO_DISPATCH: TDispatch = ManuallyDrop::new(None);
static mut dispatchTable: [TDispatch;MAX_CLASSES as usize] = [NO_DISPATCH;MAX_CLASSES as usize];
use std::sync::RwLock;

lazy_static!{
    static ref dispatchFree: RwLock<usize> = RwLock::new(0);
}
pub fn getClass(class:u16) -> Object {
    if class<MAX_CLASSES {
        if let Some(dispatch) = unsafe{dispatchTable[class as usize].take()} {
            return dispatch.class
        }
    }
    panic!("class {} not initialized",class)
}
pub fn addClass(c: Object, n: usize) {
    let mut index = dispatchFree.write().unwrap();
    let pos = *index;
    if pos >= MAX_CLASSES as usize {panic!("too many classes")}
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
pub fn dispatch(thread:&mut Thread,selector:Object) -> FunctionResult {
    let arity = selector.immediateHash()>>25;
    let this = thread.atOffset(arity+1);
    if let Some(disp) = unsafe{&dispatchTable[this.class() as usize].take()} {
        match disp.getMethod(selector) {
            MethodType::Function(function) => function(thread,selector),
            MethodType::Method(method) => method.execute(thread),
            MethodType::NoType => panic!("no method found"),
        }
    } else {
        panic!("no dispatch found")
    }
}
#[cfg(test)]
mod testsInterpreter {
    use super::*;
    use crate::symbol::intern;
    fn init_classes() {
//        let l1 = 
    }
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
