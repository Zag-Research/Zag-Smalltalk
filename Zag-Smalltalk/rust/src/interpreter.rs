use crate::object::*;
use crate::memory::*;
use crate::class::*;
use self::primitives::*;
use std::sync::{RwLock,Mutex,Arc};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct Thread {
    stack:Vec<Object>,
    self_index: usize,
    frames: Vec<usize>,
}
impl Default for Thread {
    fn default() -> Self {
        Thread{
            stack: Vec::new(),
            self_index:0,
            frames:Vec::new()}
    }
}
impl Thread {
    #[inline]
    pub fn push(&mut self,o:Object) {
        self.stack.push(o)
    }
    #[inline]
    pub fn push_self(&mut self) {
        self.stack.push(self.stack[self.self_index])
    }
    #[inline]
    pub fn push_local(&mut self,offset:usize) {
        self.stack.push(self.stack[self.self_index+offset])
    }
    #[inline]
    pub fn pop_to_local(&mut self,offset:usize) {
        self.stack[self.self_index+offset] = self.stack.pop().unwrap()
    }
    #[inline]
    pub fn assign_local(&mut self,offset:usize,value:Object) {
        self.stack[self.self_index+offset]=value
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
    pub fn start_method_get_self_index(&mut self,parameters:u8) -> usize {
        self.frames.push(self.self_index);
        let s = self.offset(parameters as usize);
        self.self_index = s;
        s
    }
    #[inline]
    pub fn end_method(&mut self) {
        self.self_index = self.frames.pop().unwrap()
    }
    #[inline]
    pub fn self_index(&self) -> usize {
        self.self_index
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
    pub fn return_with(&mut self,obj:Object) {
        self.stack.truncate(self.self_index+1);
        self.stack[self.self_index] = obj
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
    closure_fields: u8,
    symbol_index: u32,
    code: Vec<(Function,Object)>,
    blocks: Vec<Arc<Method>>,
}
impl Method {
    pub fn new(class_index: ClassIndex,parameters: u8,locals: u8,symbol_index: u32,closure_fields: u8) -> Self {
        Method{
            class_index,
            parameters,
            locals,
            symbol_index,
            code: Vec::new(),
            blocks: Vec::new(),
            closure_fields,
        }
    }
    fn allocBlockClosure(&self) -> &mut HeapObject {
        unsafe{allocObject(classBlockClosure,self.closure_fields as usize+2,-1,-1,0,false).as_mut().unwrap()}
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
                    let closure = ptr.allocBlockClosure();
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
    Method(Rc<Method>),
    NoType,
}
#[derive(Clone)]
struct MethodMatch {
    selector: usize,
    function: MethodType,
}
enum SelectorMatch {
    Match,
    Empty,
    Other,
}
impl MethodMatch {
    #[inline]
    fn selector_match(&self,selector:usize) -> SelectorMatch {
        if self.selector == selector {
            SelectorMatch::Match
        } else if self.selector == usize::MAX {
            SelectorMatch::Empty
        } else {
            SelectorMatch::Other
        }
    }
}
impl Default for MethodMatch {
    fn default() -> Self {
        MethodMatch{function:MethodType::NoType,selector:usize::MAX}
    }
}
pub struct Dispatch {
    class: Object,
    table: Box<[MethodMatch]>,
}
const DISPATCH_OVERFLOW:usize = 2; // always at least one entry past the hash
#[inline]
fn hash_index(table:&[MethodMatch],selector:usize) -> usize {
    let len = table.len();
    if len>DISPATCH_OVERFLOW {
        let hash = hash_mod(len,selector);
        for index in hash..len {
            match table[index].selector_match(selector) {
                SelectorMatch::Other => (),
                _ => return index,
            }
        }
    }
    usize::MAX
}
impl Dispatch {
    fn getMethod(&self,selector:Object) -> &MethodType {
        match hash_index(&self.table,selector.raw()>>3) {
            usize::MAX => &MethodType::NoType,
            index => &self.table[index].function,
        }
    }
}
const N_CLASS_DISPATCHES: ClassIndex = MAX_CLASS+METACLASS_OFFSET+1;
type TDispatch = Option<Dispatch>;
const NO_DISPATCH: TDispatch = None;
static mut dispatchTable: [TDispatch;N_CLASS_DISPATCHES as usize] = [NO_DISPATCH;N_CLASS_DISPATCHES as usize];

lazy_static!{
    static ref dispatchFree: Mutex<()> = Mutex::new(());
}
pub fn getClass(class:ClassIndex) -> Object {
    if class<N_CLASS_DISPATCHES {
        if let Some(dispatch) = unsafe{&dispatchTable[class as usize]} {
            return dispatch.class
        }
    }
    panic!("class {} not initialized",class)
}
fn addClass(class: ClassIndex, c: Object) {
    if class >= N_CLASS_DISPATCHES {panic!("too many classes")}
    let lock = dispatchFree.lock().unwrap();
    unsafe{dispatchTable[class as usize] = Some(Dispatch {
                class: c,
                table: Vec::with_capacity(0).into_boxed_slice(),
    })}
}
fn addMethodToTable(table:&mut Vec<MethodMatch>,selector:usize,method:&MethodType) -> bool {
    let len = table.capacity();
    let hash = hash_mod(len,selector);
    for index in hash..hash+DISPATCH_OVERFLOW {
        if let SelectorMatch::Other = table[index].selector_match(selector) {
            // slot taken
        } else {
            table[index].selector = selector;
            table[index].function = method.clone();
            return true
        }
    }
    false
}
fn addMethodToDispatch(class:ClassIndex,selector:Object,method:MethodType) {
    let selector = selector.raw()>>3;
    let lock = dispatchFree.lock().unwrap();
    if let Some(disp) = unsafe{&mut dispatchTable[class as usize]} {
        match hash_index(&disp.table,selector) {
            usize::MAX => {
                let mut count = 3;
                for mm in disp.table.iter() {
                    match mm.selector_match(selector) {
                        SelectorMatch::Empty => (),
                        _ => count += 1,
                    }
                }
                let mut unfinished = true;
                let mut table = Vec::new();
                while unfinished {
                    unfinished = false;
                    count = count*4/3;
                    table = Vec::with_capacity(count+DISPATCH_OVERFLOW);
                    addMethodToTable(&mut table,selector,&method);
                    for mm in disp.table.iter() {
                        if addMethodToTable(&mut table,mm.selector,&mm.function) {unfinished = true;break}
                    }
                }
                table.resize(table.capacity(),Default::default());
                disp.table=table.into_boxed_slice()
            },
            index => {
                disp.table[index].selector = selector;
                disp.table[index].function = method
            },
        }
    } else {
        panic!("class {} not initialized",class)
    }
}
// selector is normally a Symbol for normal lookup
// but for super, it is an integer with the low 32 bits being from the Symbol, and the high 16 bits a class
pub fn dispatch(thread:&mut Thread,selector:Object) -> FunctionResult {
    let arity = selector.immediateHash()>>25;
    let this = thread.atOffset(arity+1);
    let class = this.class();
    if class == classBlockClosure {
        dispatch_closure(thread,selector,this)
    } else {
        println!("send: {:?} {:?} {}",thread.top(),selector,class);
        let lookupClass = selector.dispatch_class(class);
        if let Some(disp) = unsafe{&dispatchTable[class as usize]} {
            execute(disp.getMethod(selector),thread,selector,class,lookupClass)
        } else {
            panic!("no dispatch found for {}",crate::class::name_str(class))
        }
    }
}
fn dispatch_closure(thread:&mut Thread,selector:Object,this_:Object) -> FunctionResult {
    let this = unsafe{&*this_.as_object_ptr()};
    if  this.identityHash()==selector.hash() {
        println!("dispatch_closure hash matches: {:?} {:?}",this,selector);
        if this.raw_at(0).is_on_heap() {
            interpret_method(unsafe{&*this.raw_at(0).as_object_ptr()},thread,selector)
        } else {
        panic!("dispatch_closure: {:?} {:?}",this,selector)
        }
    } else {
        panic!("dispatch_closure: {:?} {:?}",this,selector)
    }
}
#[inline]
fn execute(method:&MethodType,thread:& mut Thread,selector:Object,class:ClassIndex,lookupClass:ClassIndex) -> FunctionResult {
    match method {
        MethodType::Function(function) => function(thread,selector),
        MethodType::Method(method) => method.execute(thread),
        MethodType::NoType => interpret_or_make_execute_method(thread,selector,class,lookupClass),
    }
}
fn interpret_or_make_execute_method(thread:&mut Thread,selector:Object,class:ClassIndex,lookupClass:ClassIndex) -> FunctionResult {
    let method = find_method(selector,class,lookupClass);
    interpret_primitive_or_method(method,thread,selector,class,lookupClass)
}
fn find_method(selector:Object,class:ClassIndex,lookupClass:ClassIndex) -> &'static HeapObject {
    let classO = unsafe{&*getClass(lookupClass).as_object_ptr()};
    println!("classO {:?} selector {:?}",classO,selector);
    let methods = unsafe{&*classO.raw_at(0).as_object_ptr()};
    for idx in  0..methods.size() {
        let method = unsafe{&*methods.at(idx).as_object_ptr()};
        if method.raw_at(0)==selector{
            return method
        }
    }
    panic!("find_method")
}
fn interpret_primitive_or_method(method:&HeapObject,thread:&mut Thread,selector:Object,class:ClassIndex,lookupClass:ClassIndex) -> FunctionResult {
    let code = method.raw_at(1);
    if let Some(function) = prim_lookup(code) {
        return function(thread,selector)
    }
    if code!=Object::from(0) {
        panic!("interpret_primitive_or_method - missing primitive:{:?}",code)
    }
    interpret_method(method,thread,selector)
}
fn interpret_method(method:&HeapObject,thread:&mut Thread,selector:Object) -> FunctionResult {
    let self_index = thread.start_method_get_self_index(selector.arity());
    let body = unsafe{&*method.raw_at(2).as_object_ptr()};
    for idx in 0..body.size() {
        if interpret_expression(body.at(idx),method,thread)  {
            break
        }
    }
    thread.return_with(thread.top());
    thread.end_method();
    NormalReturn
}
fn interpret_expression(expression:Object,method:&HeapObject,thread:&mut Thread) -> bool  {
    if expression.is_on_heap() {
        let eptr = unsafe{&*expression.as_object_ptr()};
        match eptr.class() {
            classReturn => {
                interpret_expression(eptr.raw_at(0),method,thread);
                return true
            },
            classSend => {
                println!("send {:?}",eptr);
                interpret_expression(eptr.raw_at(0),method,thread);
                interpret_push_array(eptr.raw_at(2),method,thread);
                dispatch(thread,eptr.raw_at(1));
            },
            classLiteral => thread.push(eptr.raw_at(0)),
            classLoad => {panic!("expression {:?}",eptr)},
            classStore => {panic!("expression {:?}",eptr)},
            classMethod => {
                let closure_fields = Vec::new();
                // make closure
                println!("making closure: {:?} 0x{:x}",eptr.raw_at(0),eptr.raw_at(0).hash());
                let closure = unsafe{allocObject(classBlockClosure,closure_fields.len()+2,-1,-1,eptr.raw_at(0).hash()+0x100000000,false).as_mut().unwrap()};
                closure.raw_at_put(0,expression);
                closure.raw_at_put(1,Object::from(thread.self_index() as isize));
                for (index,obj) in closure_fields.iter().enumerate() {
                    closure.raw_at_put(2+index,*obj);
                }
                thread.push(Object::from(closure));
            },
            classBlockClosure => {panic!("expression {:?}",eptr)},
            _ => {panic!("expression {:?}",eptr)},
        }
    } else if expression.is_symbol() {
        if expression.is_self_symbol() {
            thread.push_self()
        } else {
            panic!("expression {:?}",expression);
        }
    } else {   
        thread.push(expression);
    }
    return false
}
fn interpret_push_array(array:Object,method:&HeapObject,thread:&mut Thread) {
    assert!(array.is_on_heap());
    let array = unsafe{&*array.as_object_ptr()};
    match array.class() {
        classArray => {
            for idx in 0..array.size() {
                interpret_expression(array.at(idx),method,thread);
            }
        },
        _ => panic!("expected array {:?} {:?}",array,thread),
    }
}
#[cfg(test)]
mod testsInterpreter {
    use super::*;
    use crate::symbol::intern;
    macro_rules! pinit {
        ($c:expr => $($e:expr),*) => {primary_init($c,&[$( Object::from($e)),*])}
    }
    macro_rules! class {
        ($c:expr => $($e:expr),*) => {primary_init($c+METACLASS_OFFSET,&[$( Object::from($e)),*])}
    }
    macro_rules! method {
        ($($e:expr),*) => {primary_init(classMethod,&[$( Object::from($e)),*])}        
    }
    macro_rules! doReturn {
        ($($e:expr),*) => {primary_init(classReturn,&[$( Object::from($e)),*])}        
    }
    macro_rules! send {
        ($($e:expr),*) => {primary_init(classSend,&[$( Object::from($e)),*])}        
    }
    macro_rules! load {
        ($($e:expr),*) => {primary_init(classLoad,&[$( Object::from($e)),*])}        
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
            let object_meta_ =classObject+METACLASS_OFFSET;
            let object = pinit!(object_meta_ => empty, empty, nilObject, "Object", format_inst_np, empty, empty);
            addClass(classObject, object);
            let object_meta = pinit!(meta_ => empty, empty, nilObject, object);
            addClass(object_meta_, object_meta);
            let smallI = class!(classSmallInteger => array!(
                method!("+",1,empty,empty,empty,nilObject),
                method!("-",2,empty,empty,empty,nilObject),
                method!("*",9,empty,empty,empty,nilObject),
                method!("=",7,empty,empty,empty,nilObject),
                method!("foo",0,array!(
                    doReturn!(
                        send!(send!("self","+",array!(2)),"*",array!(7)),
                        false)
                ),empty,empty,nilObject),
                method!("bar",0,
                        array!(
                            doReturn!(
                                send!(
                                    send!(
                                        method!("value",0,
                                                array!(
                                                    doReturn!(
                                                        send!(
                                                            load!("self",1),
                                                            "+",array!(2)),
                                                        false)),
                                                empty,empty,nilObject),
                                        "value",empty),
                                    "*",array!(7)),
                                false)),
                        empty,empty,nilObject),
                method!("factorial",0,
                        array!(
                            doReturn!(
                                send!(
                                    send!("self",
                                          "=",array!(1)),
                                    "ifTrue:ifFalse:",array!(
                                        method!("value",0,
                                                array!(
                                                    doReturn!(
                                                        load!("self",1),
                                                        false)),
                                                empty,empty,nilObject),
                                        method!("value",0,
                                                array!(
                                                    doReturn!(
                                                        send!(
                                                            load!("self",1),
                                                            "*",array!(
                                                                send!(
                                                                    send!(
                                                                        load!("self",1),
                                                                        "-",array!(1)),
                                                                    "factorial",empty))),
                                                        false)),
                                                empty,empty,nilObject))),
                                false)),
                        empty,empty,nilObject)
            ), empty, object, "SmallInteger", format_inst_np, empty, empty);
            addClass(classSmallInteger, smallI);
            let system_ = class_index("System");
            let system_meta_ = system_+METACLASS_OFFSET;
            let system = class!(system_ => empty, empty, object, "System", format_inst_np, empty, empty);
            addClass(system_, system);
            let system_meta = pinit!(meta_ => array!(
                method!("log:","log: prim",empty,empty,empty,nilObject)
            ), empty, object_meta, system);
            addClass(system_meta_, system_meta);
        }
    }
    #[test]
    #[should_panic(expected = "no dispatch found")]
    fn dispatch_non_existant_class() {
        let mut thread:Thread = Default::default();
        thread.push(nilObject);
        dispatch(&mut thread,intern("foo"));
    }

    #[test]
    fn add_integers_immediate() {
        use crate::interpreter::primitives::smallInteger::*;
        init_classes();
        let mut thread:Thread = Default::default();
        assert_eq!(thread.size(),0);
        thread.push_i48(25);
        thread.push_i48(17);
        let mut method = Method::new(classUndefinedObject,0,0,intern("doIt").immediateHash(),0);
        method.instr(addI);
        method.execute(&mut thread);
        assert_eq!(thread.size(),1);
        assert_eq!(thread.top().as_i48(),42);
    }

    #[test]
    fn add_integers_dispatch() {
        use crate::interpreter::primitives::smallInteger::*;
        init_classes();
        let mut thread:Thread = Default::default();
        assert_eq!(thread.size(),0);
        thread.push_i48(25);
        thread.push_i48(17);
        let mut method = Method::new(classUndefinedObject,0,0,intern("doIt").immediateHash(),0);
        method.instr_with(dispatch,intern("+"));
        method.execute(&mut thread);
        assert_eq!(thread.size(),1);
        assert_eq!(thread.top().as_i48(),42);
    }

//    #[test]
    fn integers_factorial() {
        use crate::interpreter::primitives::smallInteger::*;
        init_classes();
        let mut thread:Thread = Default::default();
        assert_eq!(thread.size(),0);
        thread.push_i48(1);
        let mut method = Method::new(classUndefinedObject,0,0,intern("doIt").immediateHash(),0);
        method.instr_with(dispatch,intern("factorial"));
        method.execute(&mut thread);
        assert_eq!(thread.size(),1);
        assert_eq!(thread.top().as_i48(),1);
    }

    #[test]
    fn integers_foo() {
        use crate::interpreter::primitives::smallInteger::*;
        init_classes();
        let mut thread:Thread = Default::default();
        assert_eq!(thread.size(),0);
        thread.push_i48(4);
        let mut method = Method::new(classUndefinedObject,0,0,intern("doIt").immediateHash(),0);
        method.instr_with(dispatch,intern("foo"));
        method.execute(&mut thread);
        println!("thread {:?}",thread);
        assert_eq!(thread.size(),1);
        assert_eq!(thread.top().as_i48(),42);
    }

    #[test]
    fn integers_bar() {
        use crate::interpreter::primitives::smallInteger::*;
        init_classes();
        let mut thread:Thread = Default::default();
        assert_eq!(thread.size(),0);
        thread.push_i48(4);
        let mut method = Method::new(classUndefinedObject,0,0,intern("doIt").immediateHash(),0);
        method.instr_with(dispatch,intern("bar"));
        method.execute(&mut thread);
        println!("thread {:?}",thread);
        assert_eq!(thread.size(),1);
        assert_eq!(thread.top().as_i48(),42);
    }

/*    #[test]
    fn system_start() {
        init_classes();
        let mut thread:Thread = Default::default();
        let classIndex = class_index("System");
        let class = getClass(classIndex);
        thread.push(class);
        let mut method = Method::new(classIndex,0,0,intern("doIt").immediateHash(),0);
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
#[inline]
fn hash_mod(len:usize,selector:usize) -> usize {
    selector%(len-DISPATCH_OVERFLOW)
}
