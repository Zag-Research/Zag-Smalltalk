const std = @import("std");
const object = @import("../object.zig");
const class = @import("../class.zig");
const dispatch = @import("../dispatch.zig");
const MethodReturns = dispatch.MethodReturns;
const heap = @import("../heap.zig");

const Normal = MethodReturns.Normal;
const PrimitiveFailed = MethodReturns.PrimitiveFailed;
const NonLocal = MethodReturns.NonLocal;
const ExceptionSignaled = MethodReturns.ExceptionSignaled;
const getSym = @import("symbol.zig").getSymbol;

fn @"+"(thread : *Thread) MethodReturns {
    return Normal;
}
fn @"-"(thread : *Thread) MethodReturns {
    return Normal;
}
fn @"="(thread : *Thread) MethodReturns {
    return Normal;
}
fn @"*"(thread : *Thread) MethodReturns {
    return Normal;
}
fn factorial(thread : *Thread) MethodReturns {
    return Normal;
}
    
pub fn init(arena: *heap.Arena) {
    const symbolMethods = [_]SymbolMethod{
        {getSym("+"),@"+"},
        {getSym("-"),@"-"},
        {getSym("="),@"="},
        {getSym("*"),@"*"},
        {getSym("factorial"),factorial},
    };
    dispatch.addClass(class.SmallInteger,symbolMethods[0..]);
}
