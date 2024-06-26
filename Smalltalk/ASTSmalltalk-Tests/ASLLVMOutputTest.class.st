Class {
	#name : 'ASLLVMOutputTest',
	#superclass : 'ASLLVMTests',
	#instVars : [
		'output',
		'common'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASLLVMOutputTest >> setUp [

	super setUp.

	"Create new instance of generator and set testing inst. var"
	output := ASLLMethodOutput new "testing"
	 context: file
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

	| ast cg |
	ast := ASPharoCompiler new astFor: Integer >> #fibonacci.
	cg := ASCodeGenerator targetClass: SmallInteger ast: ast.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	output asString inspect.
	self assert: output name equals: 'fibonacci'.

]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_fibonacci_alt [

	| ast cg |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fibonacci.
	cg := ASCodeGenerator targetClass: SmallInteger ast: ast.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	output asString inspect.
	self assert: output name equals: 'fibonacci'.

]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined [
	" self <= 2 ifTrue: [ ^ 1 ]
 ^ (self - 1) fibonacci + (self - 2) fibonacci "

	| cg |
	cg := output codeGenerator.
	cg targetClass: SmallInteger.
	cg selector: #foo:arg: .
	cg locals: #( l1 l2 ) parameters: #( p1 p2 ).
	cg pushSelf.
	cg pushLiteral: 2.
	cg popVariable: #l1.
	cg pushVariable: #p1.
	cg pushVariable: #l1.
	cg sendEmbedded: #'*'.
	cg sendEmbedded: #'+'.
	cg returnTop.
	cg inspect.

]

{ #category : 'tests' }
ASLLVMOutputTest >> testLLVM_inlined_fibonacci [
	" self <= 2 ifTrue: [ ^ 1 ]
 ^ (self - 1) fibonacci + (self - 2) fibonacci "

	| cg |
	cg := output codeGenerator: ASCompilerInlineTest inlined_fibonacci.
	cg inspect.
	output outputToStream.
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
	cg resolveFlowDependencies.
	cg inspect.
	output
		codeGenerator: cg;
		outputToStream.
	file asString inspect
]
