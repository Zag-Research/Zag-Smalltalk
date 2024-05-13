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
