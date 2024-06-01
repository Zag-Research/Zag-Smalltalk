Class {
	#name : 'ASCompilerInlineTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'asserting' }
ASCompilerInlineTest >> assert: aMethod inlinesTo: ast [

	| result |
	result := ASPharoCompiler new compileMethod: aMethod.
	"result inlineMethod."
	self assert: result equals: ast
]

{ #category : 'asserting' }
ASCompilerInlineTest >> assert: aCompileMethod outputInlinesTo: code [

	| compiledAST file compiler output |
	compiledAST := ASPharoCompiler new astFor: aCompileMethod.

	file := ASZigFileContext new.
	compiler := ASZigThreadedOutput withInlining
		context: file;
		generateCode: compiledAST for: aCompileMethod classBinding value.

	output := ReadWriteStream on: String new.
	compiler codeGenerator inspect.
	file outputTo: output.

	output contents.
	"self assert: output contents equals: code"
]

{ #category : 'asserting' }
ASCompilerInlineTest >> assert: aCompileMethod outputTo: code [

	| compiledAST file output |
	compiledAST := ASPharoCompiler new astFor: aCompileMethod.
	file := ASZigFileContext new.
	ASZigThreadedOutput new
		context: file;
		generateCode: compiledAST for: aCompileMethod classBinding value.

	output := ReadWriteStream on: String new.
	file outputTo: output.

	output contents.
	self assert: output contents equals: code
]

{ #category : 'asserting' }
ASCompilerInlineTest >> class: aClass selector: aSelector [
	aClass ifNil: [ ^nil ].
	aClass methodDictionary at: aSelector ifPresent: [ ^ aClass >> aSelector ].
	^ self class: aClass superclass selector: aSelector
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockReturnInline [
"
blockReturn: p1
	true ifTrue: [ ^ p1 ]
"
	self
		assert: (self class: ASCompileTestClass1 selector: #blockReturn:)
		outputInlinesTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var @"MblockReturn:" = compileMethod(Sym.@"ifTrue:",0,9,.ASCompileTestClass1,.{
&e.pushLiteral,Object.from(true),
&e.pushNil,,
&e.pushStack,0,
&e.setupSend,Sym.value,
&e.dynamicDispatch,
&e.drop,
&e.returnSelf,
 });
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
MblockReturn:.setLiterals(&[_]Object{},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockValueInlined [
	"blockValue

	^ [ 4 ] value "
	self
		assert: ASCompileTestClass1 >> #blockValue 
		outputInlinesTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfoo = compileMethod(Mfoo,0,6,.ASCompileTestClass1,.{
&e.pushStack,1,
&e.pushLiteral,Object.from(42),
&e.returnTop,
 });
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
Mfoo.setLiterals(&[_]Object{},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBracketSendInlined [

	self
		assert: ASCompileTestClass1 >> #bracketSend
		outputInlinesTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfoo = compileMethod(Mfoo,0,3,.SmallInteger,.{
&e.pushContext,"^",
":Mfoo:1",
&e.pushLocal,0,
&e.case,,
&e.setupSend,Sym.i_1,
&e.dynamicDispatch,
 });
const Sym = struct {
	bar: Object,
    const ss = heap.compileStrings(.{
	"bar",
    });
    usingnamespace symbol.symbols;
    fn init() Sym {
        return .{
	.bar = symbol.intern(ss[0].asObject()),
        };
    }
};
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
    sym = Sym.init();
Mfoo.setLiterals(&[_]Object{sym.bar},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacci [ 

	self
		assert: Integer >> #fibonacci
		outputTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfoo = compileMethod(Mfoo,0,3,.SmallInteger,.{
&e.pushContext,"^",
":Mfoo:1",
&e.pushLocal,0,
&e.setupSend,Sym.i_1,
&e.dynamicDispatch,
 });
const Sym = struct {
	bar: Object,
    const ss = heap.compileStrings(.{
	"bar",
    });
    usingnamespace symbol.symbols;
    fn init() Sym {
        return .{
	.bar = symbol.intern(ss[0].asObject()),
        };
    }
};
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
    sym = Sym.init();
Mfoo.setLiterals(&[_]Object{sym.bar},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacciInline [
"fibonacci

	self <= 2 ifTrue: [ ^ 1 ].
	^ (self - 1) fibonacci + (self - 2) fibonacci"
	self
		assert: (self class: SmallInteger selector: #fibonacci)
		outputInlinesTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfibonacci = compileMethod(Sym.fibonacci,0,4,.Integer,.{
&e.pushContext,"^",
&e.pushStack,2,
&e.pushLiteral,Object.from(2),
&e.<=,
&e.BlockClosure.fullClosure,"1mref",
&e.setupSend,Sym.@"ifTrue:",
&e.dynamicDispatch,
&e.drop,
&e.pushStack,1,
&e.pushLiteral,Object.from(1),
&e.-,
&e.setupSend,Sym.fibonacci,
&e.dynamicDispatch,
&e.pushStack,2,
&e.pushLiteral,Object.from(2),
&e.-,
&e.setupSend,Sym.fibonacci,
&e.dynamicDispatch,
&e.setupSend,Sym.@"+",
&e.dynamicDispatch,
 });
var Mvalue = compileMethod(Sym.value,0,3,.BlockClosure,.{
&e.pushLiteral,Object.from(1),
&e.returnNonLocal,
 });
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
Mfibonacci.setLiterals(&[_]Object{},&[_]Object{},null);
Mvalue.setLiterals(&[_]Object{},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo [
"foo
	^ self bar"
"bar
	^ 42"
	self
		assert: ASCompileTestClass1 >> #foo
		outputTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfoo = compileMethod(Mfoo,0,3,.SmallInteger,.{
&e.pushContext,"^",
":Mfoo:1",
&e.pushLocal,0,
&e.setupSend,Sym.i_1,
&e.dynamicDispatch,
 });
const Sym = struct {
	bar: Object,
    const ss = heap.compileStrings(.{
	"bar",
    });
    usingnamespace symbol.symbols;
    fn init() Sym {
        return .{
	.bar = symbol.intern(ss[0].asObject()),
        };
    }
};
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
    sym = Sym.init();
Mfoo.setLiterals(&[_]Object{sym.bar},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFooInlined [
"foo
	^ self bar"
"bar
	^ 42"
	self
		assert: ASCompileTestClass1 >> #foo
		outputInlinesTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfoo = compileMethod(Mfoo,0,6,.ASCompileTestClass1,.{
&e.pushStack,1,
&e.pushLiteral,Object.from(42),
&e.returnTop,
 });
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
Mfoo.setLiterals(&[_]Object{},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testGives43 [

	self
		assert: ASCompileTestClass1 >> #gives43
		inlinesTo: (ASMethodNode
				 selector: #gives43
				 locals: #( #forty2 )
				 body: (ASSequence newFromArray: {
							  (ASAssign variable: #forty2 expression: (ASSend
									    target: (ASSend
											     target: (ASSelf method: #gives43)
											     selector: #unaries
											     args: {  })
									    selector: #*
									    args: { (ASLiteral literal: 2) })).
							  (ASReturn expression: (ASSend
									    target: (ASSelf method: #gives43)
									    selector: #plus1:
									    args: { (ASSend
											     target: (ASRef variable: #forty2)
											     selector: #-
											     args: { (ASLiteral literal: 4) }) })) }))
]

{ #category : 'tests' }
ASCompilerInlineTest >> testInlinePlus1Inlined [
"inlinePlus1
	^ self plus1: 30"
"plus1: aNumber
	^ aNumber + 1"
	self
		assert: ASCompileTestClass1 >> #inlinePlus1 
		outputInlinesTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var MinlinePlus1 = compileMethod(MinlinePlus1,0,9,.ASCompileTestClass1,.{
&e.pushContext,"^",
&e.pushStack,3,
&e.pushLiteral,Object.from(30),
&e.pushStack,2,
&e.pushLiteral,Object.from(1),
&e.setupSend,Sym.@"+",
&e.dynamicDispatch,
 });
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
MinlinePlus1.setLiterals(&[_]Object{},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]

{ #category : 'tests' }
ASCompilerInlineTest >> testWhileTrueInlined [
	"whileTrue

	| dirty |
	[
	dirty := false.
	dirty ] whileTrue"

	self
		assert: ASCompileTestClass1 >> #whileTrue
		outputInlinesTo: 'const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfoo = compileMethod(Mfoo,0,6,.ASCompileTestClass1,.{
&e.pushStack,1,
&e.pushLiteral,Object.from(42),
&e.returnTop,
 });
var sym: Sym = undefined;
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
Mfoo.setLiterals(&[_]Object{},&[_]Object{},null);
}
pub fn main() !void {
    initSmalltalk();
}
'
]
