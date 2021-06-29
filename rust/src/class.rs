use crate::object::*;
use crate::treap::LockingTreap;
const first_classes: &[Object]= &[
    uncheckedSymbolOf(10+classObject as u32),
    uncheckedSymbolOf(10+classBlockClosure as u32),
    uncheckedSymbolOf(10+classFalse as u32),
    uncheckedSymbolOf(10+classTrue as u32),
    uncheckedSymbolOf(10+classUndefinedObject as u32),
    uncheckedSymbolOf(10+classSmallInteger as u32),
    uncheckedSymbolOf(10+classSymbol as u32),
    uncheckedSymbolOf(10+classCharacter as u32),
    uncheckedSymbolOf(10+classFloat as u32),
    uncheckedSymbolOf(10+classArray as u32),
    uncheckedSymbolOf(10+classString as u32),
    uncheckedSymbolOf(10+classClass as u32),
    uncheckedSymbolOf(10+classMetaclass as u32),
    uncheckedSymbolOf(10+classBehavior as u32),
    uncheckedSymbolOf(10+classMethod as u32),
    uncheckedSymbolOf(10+classSystem as u32),
    uncheckedSymbolOf(10+classReturn as u32),
    uncheckedSymbolOf(10+classSend as u32),
    uncheckedSymbolOf(10+classLiteral as u32),
    uncheckedSymbolOf(10+classLoad as u32),
    uncheckedSymbolOf(10+classStore as u32),
];
static_assertions::const_assert_eq!(first_classes.len(),number_of_constant_classes);
lazy_static!{
    static ref classTable: LockingTreap<Object> = LockingTreap::new(first_classes,nilObject);
}
pub const MAX_CLASS: ClassIndex = 1000;
pub const METACLASS_OFFSET: ClassIndex = MAX_CLASS+1;
pub fn is_meta(class:ClassIndex) -> bool {
    class>=METACLASS_OFFSET
}
pub fn name_str(class:ClassIndex) -> StaticStr {
    crate::symbol::str_of(classTable.at((class%METACLASS_OFFSET) as usize))
}
pub fn class_index(symbol:&'static str) -> ClassIndex {
    class_index_from_symbol(crate::symbol::intern(symbol)) as ClassIndex
}
pub fn class_index_from_symbol(symbol:Object) -> ClassIndex {
    classTable.intern(symbol) as ClassIndex
}
