Class {
	#name : 'ASCompilerInlineTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'hand compiled' }
ASCompilerInlineTest class >> inlined_fibonacci [
	" self <= 2 ifTrue: [ ^ 1 ]
 ^ (self - 1) fibonacci + (self - 2) fibonacci "

	| cg targets |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #fibonacci.
	cg locals: #(  ) parameters: #(  ).
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #'<='.
	cg pushNil.
	targets := cg
		           classCase: {
				           True.
				           False }
		           selector: #ifTrue:.
	cg setBasicBlock: targets first.
	cg drop.
	cg drop.
	cg pushLiteral: 1.
	cg returnTop.
	cg setBasicBlock: targets last.
	cg drop.
	cg drop.
	cg pushSelf.
	cg pushLiteral: 1.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg sendEmbedded: #+.
	cg returnTop.
	cg finalAnalysis.
	^ cg
]

{ #category : 'hand compiled' }
ASCompilerInlineTest class >> inlined_fibonacci_alt [
	" ^ self <= 2 ifTrue: [ 1 ]
    ifFalse: [ (self - 1) fibonacci + (self - 2) fibonacci ]"

	| cg targets common |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #fibonacci.
	cg locals: #(  ) parameters: #(  ).
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #'<='.
	cg pushNil.
	common := cg newInline.
	targets := cg
		           classCase: {
				           True.
				           False }
		           selector: #ifTrue:.
	cg setBasicBlock: targets first.
	cg drop.
	cg pushLiteral: 1.
	cg branch: common.
	cg setBasicBlock: targets last.
	cg drop.
	cg pushSelf.
	cg pushLiteral: 1.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg sendEmbedded: #+.
	cg branch: common.
	cg setBasicBlock: common.
	cg returnTop.
	cg finalAnalysis.
	^ cg
]

{ #category : 'hand compiled' }
ASCompilerInlineTest class >> inlined_insertionSorted [
" Array>>#size, Array>>at:, Array>>at:put: and BlockClosure>>#value are primitive

defined in SequencableCollection	
	insertionSorted
	2 to: self size do: [ :i | self insertElementAt: i ]
	
	insertElementAt: i
	| new |
	new := self at: i.
	i to: 2 by: -1 do: [ :j |
		| old |
		old := self at: j - 1.
		new >= old ifTrue: [
			self at: j put: new.
			^ self ].
		self at: j put: old ].
	self at: 1 put: new
	
defined in Number
	to: stop do: aBlock
	| nextValue |
	nextValue := self.
	[ nextValue <= stop ] whileTrue: [
		aBlock value: nextValue.
		nextValue := nextValue + 1 ]

defined in BlockClosure
	whileTrue: aBlock
	self value ifFalse: [ ^ nil ].
	aBlock value.
	^ self whileTrue: aBlock
	"

	| cg |
	cg := ASCodeGenerator new.
	cg targetClass: Array.
	cg selector: #insertionSorted.
	cg locals: #(  ) parameters: #(  ).
	cg pushLiteral: 2.
	cg pushSelf.

	^ cg
]

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

{ #category : 'running' }
ASCompilerInlineTest >> setUp [

	super setUp.

	ASInliner reset
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockReturnEasyInline [
	"
blockReturn: p1
	true ifTrue: [ ^ 42 ]
"

	(ASNoOutput withInlining compileMethod:
		 ASCompileTestClass1 >> #blockReturnEasy) inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockReturnInline [
	"
blockReturn: p1
	true ifTrue: [ ^ p1 ]
"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockReturn:.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockValueInlined [
	"blockValue

	^ [ 4 ] value "

	(ASNoOutput withInlining compileMethod: ASCompileTestClass1 >> #blockValue)
		inspect.
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBracketSendInlined [
"bracketSend

	5 + (self fib: 2)"
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
ASCompilerInlineTest >> testCollectionAnyOne [
	"blockValue

	^ [ 4 ] value "

	(ASNoOutput withInlining compileMethod: ASCompileTestClass1 >> #collectionAnyOne)
		inspect.
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacci [
	"
fibonacci

	self < 2 ifTrue: [ ^ self ].
	^ (self - 1) fibonacci + (self - 2) fibonaccis"

	| cg |
	cg := ASCodeGenerator
		      targetClass: SmallInteger
		      selector: #fibonacci.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacciFastInline [
	"
fibonacci_fast

	^ self fibonacci_accumulator: 1 prev: 0
	
fibonacci_accumulator: accumulator prev: prev

	self = 0 ifTrue: [ ^ prev ].
	^ self - 1
		  fibonacci_accumulator: prev + accumulator
		  prev: accumulator"

	| cg |
	cg := ASCodeGenerator
		      targetClass: SmallInteger
		      selector: #fibonacci_fast.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacciFastInline_prev [
	"
fibonacci_accumulator: accumulator prev: prev

	self = 0 ifTrue: [ ^ prev ].
	^ self - 1
		  fibonacci_accumulator: prev + accumulator
		  prev: accumulator"

	| cg |
	cg := ASCodeGenerator
		      targetClass: SmallInteger
		      selector: #fibonacci_accumulator:prev:.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacci_fast_forCorrectness [

	self assert: 0 fibonacci_fast equals: 0 fibonacci.
	self assert: 1 fibonacci_fast equals: 1 fibonacci.
	self assert: 2 fibonacci_fast equals: 2 fibonacci.
	self assert: 3 fibonacci_fast equals: 3 fibonacci.
	self assert: 4 fibonacci_fast equals: 4 fibonacci
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
ASCompilerInlineTest >> testFoo1Inlined [
	"foo
	^ self bar isLarge"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #foo1.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo2Inlined [
	"foo
	^ self bar"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #foo2.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo3Inlined [
	"foo3
	self bar"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #foo3.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFooInlined [
	"foo
	self bar isLarge.
	^ 42"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator targetClass: ASCompileTestClass1 selector: #foo.
	cg
		inline: true;
		finalAnalysis;
		inspect
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

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #inlinePlus1.
	cg inspect;halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testInsertionSort [

	self
		assert: #( 1 2 3 4 5 6 ) copy insertionSorted
		equals: #( 1 2 3 4 5 6 ).
	self
		assert: #( 6 2 4 3 5 1 ) copy insertionSorted
		equals: #( 1 2 3 4 5 6 )
]

{ #category : 'tests' }
ASCompilerInlineTest >> testNegatedInlined [
	"inlinePlus1
	^ self plus1: 30"

	"plus1: aNumber
	^ aNumber + 1"

	| cg |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #negated.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testRecursiveMethod [
	"recursiveMethod
	^ self recursiveMethod"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #recursiveMethod.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testWhileTrueInlined [
	"whileTrue

	| dirty |
	[
	dirty := false.
	dirty ] whileTrue"

	(ASNoOutput withInlining compileMethod: ASCompileTestClass1 >> #whileTrue) inspect
]

{ #category : 'hand compiled' }
ASCompilerInlineTest >> test_inlined_insertionSorted [

	| cg |
	cg := self class inlined_insertionSorted.
	cg inspect
]
