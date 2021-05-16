use crate::object::*;
use crate::treap::LockingTreap;
#[repr(C)]  // don't shuffle the fields
#[derive(Copy, Clone)]
pub struct Class {
    superclass: Object,
//    class encoded in the header
}
static first_classes: &[Object]= &[
    unarySymbolOf("Object",10+classObject),
    unarySymbolOf("BlockClosure",10+classBlockClosure),
    unarySymbolOf("False",10+classFalse),
    unarySymbolOf("True",10+classTrue),
    unarySymbolOf("UndefinedObject",10+classUndefinedObject),
    unarySymbolOf("SmallInteger",10+classSmallInteger),
    unarySymbolOf("Symbol",10+classSymbol),
    unarySymbolOf("Character",10+classCharacter),
    unarySymbolOf("Float",10+classFloat),
    unarySymbolOf("String",10+classString),
    unarySymbolOf("Class",10+classClass),
    unarySymbolOf("Metaclass",10+classMetaclass),
    unarySymbolOf("Behavior",10+classBehavior),
    unarySymbolOf("Array",10+classArray),
];
lazy_static!{
    static ref classTable: LockingTreap<Object> = LockingTreap::new(first_classes,nilObject);
}
pub fn name_str(class:usize) -> StaticStr {
    crate::symbol::str_of(classTable.at(class))
}
