use crate::object::*;
type Function = fn(&mut Thread,Object,Option<&Method>) -> FunctionResult;
#[derive(Eq,PartialEq,Debug)]
pub enum FunctionResult {
    NormalReturn,
    NonLocalReturn,
    ExceptionSignaled,
}
use FunctionResult::*;
mod primitives {
    use super::*;
    pub fn yourself(thread:&mut Thread,selector:Object,method:Option<&Method>) -> FunctionResult {
        NormalReturn
    }
}
pub struct Thread {
    stack:Vec<Object>,
}
impl Thread {
    pub fn push(&mut self,o:Object) {
        self.stack.push(o)
    }
    pub fn top(&self) -> Object {
        self.stack[self.stack.len()-1]
    }
}
impl Default for Thread {
    fn default() -> Self {
        Thread{ stack: vec![placeholderObject]}
    }
}
#[derive(Default,Clone)]
pub struct Method {
    class_index: u16,
    parameters: u8,
    locals: u8,
    symbol_index: u32,
    code: Vec<(Function,Object)>,
}
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
    pub fn instr(&mut self,f:Function) {
        self.code.push((f,placeholderObject))
    }
    pub fn instr_with(&mut self,f:Function,o:Object) {
        self.code.push((f,o))
    }
    pub fn execute(&self,thread:&mut Thread) -> FunctionResult {
        let mut pc:usize = 0;
        let end = self.code.len()-1;
        let this_index = thread.stack.len()-self.parameters as usize-1;
        thread.stack.resize(thread.stack.len()+self.locals as usize,nilObject);
        loop {
            if pc==end {break};
            let (f,o)=self.code[pc];
            match f(thread,o,None) {
                NormalReturn => {pc = pc + 1},
                NonLocalReturn => {panic!("non-local return")},
                ExceptionSignaled => {panic!("exception signaled")},
            }
        };
        let (f,o)=self.code[pc];
        f(thread,o,None)
    }
}
#[cfg(test)]
mod testMethod {
    use super::*;
    use crate::symbol::intern;
    #[test]
    fn noop() {
        let mut thread:Thread = Default::default();
        thread.push(Object::from(42));
        let mut method = Method::new(classObject as u16,0,0,intern("yourself").immediateHash());
        method.instr(primitives::yourself);
        assert_eq!(method.execute(&mut thread),NormalReturn);
        assert_eq!(thread.top(),Object::from(42));
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
    table.resize(n,MethodMatch{function:primitives::yourself,method:Default::default()});
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
    if let Some(disp) = unsafe{&dispatchTable[this.class()].take()} {
        if let Some(MethodMatch{function,method}) = disp.getMethod(selector_hash) {
            function(thread,placeholderObject,Some(method))
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
