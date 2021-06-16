use crate::object::*;
use crate::memory::*;
use crate::class::*;
use std::sync::{RwLock,Mutex,Arc};

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
    pub fn second(&self) -> Object {
        self.stack[self.stack.len()-2]
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
    pub fn size(&self) -> usize {
        self.stack.len()
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
    pub fn start_method_get_self_index(&self,parameters:u8) -> usize {
        self.offset(parameters as usize)
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
    pub fn discard(&mut self,number:usize) {
        self.stack.truncate(self.stack.len()-number);
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
    NonLocalReturn(u32), // stack frame number
    JumpTo(u32), // instruction number
    MakeClosure(u32), // block number
    SpillReturn,
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
    class_index: ClassIndex,
    parameters: u8,
    locals: u8,
    symbol_index: u32,
    code: Vec<(Function,Object)>,
    blocks: Vec<Arc<Method>>,
}
impl Method {
    pub fn new(class_index: ClassIndex,parameters: u8,locals: u8,symbol_index: u32) -> Self {
        Method{
            class_index,
            parameters,
            locals,
            symbol_index,
            code: Vec::new(),
            blocks: Vec::new(),
        }
    }
    fn alloc(&self) -> &mut HeapObject {
        unsafe{allocObject(classBlockClosure,self.locals as usize+2,-1,-1,0,false).as_mut().unwrap()}
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
                ReturnIsNext => { pc = pc + 1;break}
                JumpTo(new_pc) => { pc = new_pc as usize},
                NonLocalReturn(index) => {
                    thread.end_method();
                    thread.pop_to(index as usize);
                    if thread.size()==self_index {
                        return NormalReturn
                    } else {
                        return SpillReturn
                    }
                },
                SpillReturn => {
                    thread.end_method();
                    if thread.size()==self_index {
                        return NormalReturn
                    } else {
                        return SpillReturn
                    }
                },
                MakeClosure(block) => {
                    pc = pc + 1;
                    let method = self.blocks[block as usize].clone();
                    let ptr = unsafe{&*Arc::as_ptr(&method)};
                    let closure = ptr.alloc();
                    closure.raw_at_put(0,Object::from(Arc::into_raw(method) as * const HeapObject));
                    closure.raw_at_put(1,Object::from(self_index as isize));
                    thread.push(Object::from(closure));
                },
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
        MethodMatch{function:Box::new(MethodType::NoType),selector:u32::MAX}
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
                MethodType::NoType => break,
                result => {return result},
            }
        }
        &MethodType::NoType
    }
}
const MAX_CLASSES: ClassIndex = 1000;
use std::mem::ManuallyDrop;
type TDispatch = ManuallyDrop<Option<Dispatch>>;
const NO_DISPATCH: TDispatch = ManuallyDrop::new(None);
static mut dispatchTable: [TDispatch;MAX_CLASSES as usize] = [NO_DISPATCH;MAX_CLASSES as usize];

lazy_static!{
    static ref dispatchFree: Mutex<()> = Mutex::new(());
}
pub fn getClass(class:ClassIndex) -> Object {
    if class<MAX_CLASSES {
        if let Some(dispatch) = unsafe{dispatchTable[class as usize].take()} {
            return dispatch.class
        }
    }
    panic!("class {} not initialized",class)
}
fn addClass(pos: ClassIndex, c: Object) {
    let lock = dispatchFree.lock().unwrap();
    if pos >= MAX_CLASSES {panic!("too many classes")}
    replaceDispatch(pos as usize,c,4);
}
fn replaceDispatch(pos: usize, c: Object, n: usize) -> Option<Dispatch> {
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
    let class = this.class();
    if let Some(disp) = unsafe{&dispatchTable[class as usize].take()} {
        execute(disp.getMethod(selector),thread,selector,class)
    } else {
        panic!("no dispatch found for {}",crate::class::name_str(class))
    }
}
#[inline]
fn execute(method:&MethodType,thread:& mut Thread,selector:Object,class:ClassIndex) -> FunctionResult {
    match method {
        MethodType::Function(function) => function(thread,selector),
        MethodType::Method(method) => method.execute(thread),
        MethodType::NoType => make_execute_method(thread,selector,class),
    }
}
fn make_execute_method(thread:&mut Thread,selector:Object,class:ClassIndex) -> FunctionResult {
    let method = make_method(selector,class);
    execute(&method,thread,selector,class)
}
fn make_method(selector:Object,class:ClassIndex) -> Box<MethodType> {
    
    panic!("make_method")
}
#[cfg(test)]
mod testsInterpreter {
    use super::*;
    use crate::symbol::intern;
    macro_rules! pinit {
        ($c:expr => $($e:expr),*) => {primary_init($c,&[$( Object::from($e)),*])}
    }
    macro_rules! array {
        ($($e:expr),*) => {primary_array(&[$( Object::from($e)),*])}
    }
    lazy_static!{
        static ref needs_initialization: Mutex<bool> = Mutex::new(true);
    }
    fn init_classes() {
        let mut init = needs_initialization.lock().unwrap();
        if *init {
            *init=false;
            let method_ = class_index("Method");
            let class_ = class_index("Class");
            assert_eq!(class_,classClass);
            let empty = array!();
            let meta_ = class_index("Metaclass");
            let object_meta_ = metaclass_index();
            let object = pinit!(object_meta_ => empty, empty, nilObject, "Object", format_inst_np, nilObject);
            addClass(classObject, object);
            let object_meta = pinit!(meta_ => empty, empty, nilObject, object);
            addClass(object_meta_, object_meta);
            let smallI_meta_ = metaclass_index();
            let addI_ = pinit!(method_ => "+",1,empty,empty,empty);
            let smallI = pinit!(smallI_meta_ => array!(addI_), empty, object, "SmallInteger", format_inst_np, nilObject);
            addClass(classSmallInteger, smallI);
            let system_ = class_index("System");
            let system_meta_ = metaclass_index();
            let log_ = pinit!(method_ => "log:","log: prim",empty,empty,empty);
            let system = pinit!(system_meta_ => empty, empty, object, "System", format_inst_np, object);
            addClass(system_, system);
            let system_meta = pinit!(meta_ => array!(log_), empty, object_meta, system);
            addClass(system_meta_, system_meta);
        }
    }
    #[test]
    #[should_panic(expected = "no dispatch found")]
    fn dispatch_non_existant_class() {
        let mut thread:Thread = Default::default();
        dispatch(&mut thread,intern("foo"));
    }

    #[test]
    fn add_integers_immediate() {
        use crate::interpreter::primitives::smallInteger::*;
        init_classes();
        let mut thread:Thread = Default::default();
        assert_eq!(thread.size(),1);
        thread.push_i48(25);
        thread.push_i48(17);
        let mut method = Method::new(classUndefinedObject,0,0,intern("doIt").immediateHash());
        method.instr(addI);
        method.execute(&mut thread);
        assert_eq!(thread.size(),2);
        assert_eq!(thread.top().as_i48(),42);
    }

    #[test]
    fn add_integers_dispatch() {
        use crate::interpreter::primitives::smallInteger::*;
        init_classes();
        let mut thread:Thread = Default::default();
        assert_eq!(thread.size(),1);
        thread.push_i48(25);
        thread.push_i48(17);
        let mut method = Method::new(classUndefinedObject,0,0,intern("doIt").immediateHash());
        method.instr_with(dispatch,intern("+"));
        method.execute(&mut thread);
        assert_eq!(thread.size(),2);
        assert_eq!(thread.top().as_i48(),42);
    }

/*    #[test]
    fn system_start() {
        init_classes();
        let mut thread:Thread = Default::default();
        let classIndex = class_index("System");
        let class = getClass(classIndex);
        thread.push(class);
        let mut method = Method::new(classIndex,0,0,intern("doIt").immediateHash());
        method.instr_with(dispatch,intern("start"));
        method.execute(&mut thread);
    }
*/
/*    #[test]
    #[should_panic(expected = "no method found")]
    fn dispatch_non_existant_method() {
        dispatch(Object::from(3.14),intern("foo"),nilObject,nilObject,None);
    }*/
}
