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

	| compiledAST file output |
	compiledAST := ASPharoCompiler new astFor: aCompileMethod.

	file := ASZigFileContext new.
	ASZigThreadedOutput withInlining
		context: file;
		generateCode: compiledAST for: aCompileMethod classBinding value.

	output := ReadWriteStream on: String new.
	file outputTo: output.

	output contents.
	self assert: output contents equals: code
]

{ #category : 'asserting' }
ASCompilerInlineTest >> assert: aCompileMethod outputTo: code [

	| compiledAST file output |
	compiledAST := ASPharoCompiler new astFor: aCompileMethod.

	file := ASZigFileContext new.
	ASZigThreadedOutput new
		context: file;
		generateCode: compiledAST for: SmallInteger.

	output := ReadWriteStream on: String new.
	file outputTo: output.

	output contents.
	self assert: output contents equals: code
]

{ #category : 'tests' }
ASCompilerInlineTest >> testCodeGeneratorInlining [

	| cg |
	cg := ASCodeGenerator new.
	cg send: #foo targetClass: ASCompileTestClass1 tail: false.
	cg tryInlining.
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
ASCompilerInlineTest >> testFoo [

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
