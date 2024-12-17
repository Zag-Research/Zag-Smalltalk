use crate::object::*;
use crate::treap::LockingTreap;
const first_classes: &[Object]= &[
    uncheckedSymbolOf(11+classObject as u32),
    uncheckedSymbolOf(11+classBlockClosure as u32),
    uncheckedSymbolOf(11+classFalse as u32),
    uncheckedSymbolOf(11+classTrue as u32),
    uncheckedSymbolOf(11+classUndefinedObject as u32),
    uncheckedSymbolOf(11+classSmallInteger as u32),
    uncheckedSymbolOf(11+classSymbol as u32),
    uncheckedSymbolOf(11+classCharacter as u32),
    uncheckedSymbolOf(11+classFloat as u32),
    uncheckedSymbolOf(11+classArray as u32),
    uncheckedSymbolOf(11+classString as u32),
    uncheckedSymbolOf(11+classClass as u32),
    uncheckedSymbolOf(11+classMetaclass as u32),
    uncheckedSymbolOf(11+classBehavior as u32),
    uncheckedSymbolOf(11+classMethod as u32),
    uncheckedSymbolOf(11+classSystem as u32),
    uncheckedSymbolOf(11+classReturn as u32),
    uncheckedSymbolOf(11+classSend as u32),
    uncheckedSymbolOf(11+classLiteral as u32),
    uncheckedSymbolOf(11+classLoad as u32),
    uncheckedSymbolOf(11+classStore as u32),
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
