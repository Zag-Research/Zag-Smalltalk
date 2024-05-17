Class {
	#name : 'ASLLVMOutputTest',
	#superclass : 'ASLLVMTests',
	#instVars : [
		'output'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASLLVMOutputTest >> setUp [

	super setUp.

	"Create new instance of generator and set testing inst. var"
	output := ASLLMethodOutput testing context: file
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
ASLLVMOutputTest >> testLLVM_fibonacci [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fibonacci.
	output generateCode: ast for: SmallInteger.
	output codeGenerator inspect.
	self assert: output name equals: 'Zfibonacci'.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_fibonacci2 [

	| ast |
	ast := ASPharoCompiler new astFor: Integer >> #fibonacci.
	output generateCode: ast for: SmallInteger.
	output codeGenerator inspect.
	self assert: output name equals: 'Zfibonacci'.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined_fibonacci [

	| cg targets |
	cg := output codeGenerator.
	cg targetClass: SmallInteger.
	cg selector: #fibonacci.
	cg locals: #(  ) arguments: #(  ).
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #<.
	targets := cg
		           classCase: {
				           True.
				           False }
		           complete: false.
	cg code: targets first.
	cg pushLiteral: 1.
	cg returnTop.
	cg code: targets last.
	cg pushSelf.
	cg pushLiteral: 1.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: SmallInteger tail: false.
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: SmallInteger tail: false.
	cg sendEmbedded: #+.
	cg returnTop.
	cg nameBasicBlocks.
	cg inspect
]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined_simple [

	| cg |
	cg := output codeGenerator.
	cg targetClass: SmallInteger.
	cg selector: #simple.
	cg locals: #(  ) arguments: #(  ).
	cg pushLiteral: 42.
	cg returnTop.
	cg nameBasicBlocks.
	cg inspect.
	output outputToStream.
	self halt.
	file asString inspect
]
