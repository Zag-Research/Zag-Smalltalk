Class {
	#name : 'ASLLVMOutputTest',
	#superclass : 'ASLLVMTest',
	#instVars : [
		'output'
	],
	#category : 'Zag-LLVM-Tests',
	#package : 'Zag-LLVM-Tests'
}

{ #category : 'running' }
ASLLVMOutputTest >> setUp [

	super setUp.

	"Create new instance of generator and set testing inst. var"
	output := ASLLMethodOutput new context: file "testing".
	ASInliner reset
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVMOutput [
	"in-progress"

	| ast |
	"Creates Zag AST - ASMethodNode"
	ast := self astFor: ASCompileTestClass1 >> #simple1.
	"Output LLVM IR to generator's writestream"
	output generateCode: ast
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_blockReturnEasy [

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockReturnEasy.
	cg
		inline: true;
		finalAnalysis;flowAnalysis.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	output asString inspect.
	self assert: output name equals: 'blockReturnEasy'
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_blockValue [

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockValue.
	cg
		inline: true;
		finalAnalysis;
		flowAnalysis.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	output asString inspect.
	self assert: output name equals: 'blockValue'
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_fibonacci [
	" self <= 2 ifTrue: [ ^ 1 ]
 ^ (self - 1) fibonacci + (self - 2) fibonacci "

	| cg |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #fibonacci.
	cg inspect.
	self halt.
	cg
		inline: true;
		finalAnalysis;flowAnalysis.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	file asString inspect.
	self assert: output name equals: 'fibonacci'
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_fibonacci_alt [
"^ self <= 2 ifTrue: [ 1 ]
    ifFalse: [ (self - 1) fibonacci + (self - 2) fibonacci ]"
	| cg |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #fibonacci_alt.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	output asString inspect.
	self assert: output name equals: 'fibonacci'
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined [

	| cg |
	cg := output codeGenerator.
	cg targetClass: SmallInteger.
	cg selector: #foo:arg:.
	cg locals: #( l1 l2 ) parameters: #( p1 p2 ).
	cg pushSelf.
	cg pushLiteral: 2.
	cg popVariable: #l1.
	cg pushVariable: #p1.
	cg pushVariable: #l1.
	cg sendEmbedded: #*.
	cg sendEmbedded: #+.
	cg returnTop.
	cg inspect
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined_fibonacci [
	" self <= 2 ifTrue: [ ^ 1 ]
 ^ (self - 1) fibonacci + (self - 2) fibonacci "

	| cg |
	cg := ASCompilerInlineTest inlined_fibonacci.
	cg
		inline: true;
		finalAnalysis;flowAnalysis.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	file asString inspect
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined_fibonacci_alt [
	" ^ self <= 2 ifTrue: [ 1 ]
    ifFalse: [ (self - 1) fibonacci + (self - 2) fibonacci ]"

	| cg |
	cg := output codeGenerator: ASCompilerInlineTest inlined_fibonacci_alt.
	cg inspect.
	output outputToStream.
	file asString inspect
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined_simple [

	| cg |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #simple.
	cg locals: #(  ) parameters: #(  ).
	cg pushLiteral: 42.
	cg returnTop.
	cg
		finalAnalysis;
		flowAnalysis.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	file asString inspect
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_yourself [

	| cg |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #yourself.
	cg inspect.
	cg
		inline: true;
		finalAnalysis;flowAnalysis.
	output
		codeGenerator: cg;
		outputToStream.
	file asString inspect.
	self assert: output name equals: 'yourself'
]

{ #category : 'tests' }
ASLLVMOutputTest >> test_plus_and [
	"plus: n1 and: n2 and: n3

	^ n1 + (n2 * n3)"

	| cg |
	cg := ASCodeGenerator new.
	cg targetClass: ASCompileTestClass1.
	cg selector: #plus:and:.
	cg locals: #(  ) parameters: #( n1 n2 ).
	cg pushVariable: #n1.
	cg pushLiteral: 42.
	cg pushVariable: #n2.
	cg send: #*. " and create new ReturnBlock"
	cg send: #+. " and create new ReturnBlock"
	cg returnTop.
	cg
		inline: true;
		finalAnalysis;
		flowAnalysis.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	file asString inspect
]

{ #category : 'tests' }
ASLLVMOutputTest >> test_plus_and_and [
	"plus: n1 and: n2 and: n3

	^ n1 + (n2 * n3)"

	| cg |
	cg := ASCodeGenerator new.
	cg targetClass: ASCompileTestClass1.
	cg selector: #plus:and:and:.
	cg locals: #(  ) parameters: #( n1 n2 n3 ).
	cg pushVariable: #n1.
	cg pushVariable: #n2.
	cg pushVariable: #n3.
	cg send: #*. " and create new ReturnBlock"
	cg send: #+. " and create new ReturnBlock"
	cg returnTop.
	cg
		inspect;
		halt;
		inline: true;
		finalAnalysis;
		flowAnalysis.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	file asString inspect
]
