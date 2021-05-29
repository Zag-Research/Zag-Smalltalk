use crate::object::*;
use crate::treap::LockingTreap;
static first_classes: &[Object]= &[
    uncheckedSymbolOf(10+classObject as u32),
    uncheckedSymbolOf(10+classBlockClosure as u32),
    uncheckedSymbolOf(10+classFalse as u32),
    uncheckedSymbolOf(10+classTrue as u32),
    uncheckedSymbolOf(10+classUndefinedObject as u32),
    uncheckedSymbolOf(10+classSmallInteger as u32),
    uncheckedSymbolOf(10+classSymbol as u32),
    uncheckedSymbolOf(10+classCharacter as u32),
    uncheckedSymbolOf(10+classFloat as u32),
    uncheckedSymbolOf(10+classString as u32),
    uncheckedSymbolOf(10+classClass as u32),
    uncheckedSymbolOf(10+classMetaclass as u32),
    uncheckedSymbolOf(10+classBehavior as u32),
    uncheckedSymbolOf(10+classArray as u32),
];
lazy_static!{
    static ref classTable: LockingTreap<Object> = LockingTreap::new(first_classes,nilObject);
}
pub fn name_str(class:u16) -> StaticStr {
    crate::symbol::str_of(classTable.at(class as usize))
}
pub fn class_index(symbol:Object) -> u16 {
    classTable.intern(symbol) as u16
}
