const std = @import("std");
const object = @import("../object.zig");
const class = @import("../class.zig");
const dispatch = @import("../dispatch.zig");
const SymbolMethod = dispatch.SymbolMethod;
const MethodReturns = dispatch.MethodReturns;
const heap = @import("../heap.zig");

const Normal = MethodReturns.Normal;
const PrimitiveFailed = MethodReturns.PrimitiveFailed;
const NonLocal = MethodReturns.NonLocal;
const ExceptionSignaled = MethodReturns.ExceptionSignaled;

fn @"+"(thread : *Thread) MethodReturns {
    return Normal;
}
    
pub fn init(arena: *heap.Arena) {
    const symbolMethods = [_]SymbolMethod{
        {"+",@"+"},
    };
    dispatch.addClass(class.SmallInteger,symbolMethods[0..]
