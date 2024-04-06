Class {
	#name : #ASZigLLVMOutputTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #API }
ASZigLLVMOutputTest >> testLLVMOutput [

	| ast file output |
	"Creates Zag AST - ASMethodNode"
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	"Convert to LLVM specific context"
	file := ASZigFileContext new.
	"Create new instance of generator and set testing inst. var"
	output	 := ASZigLLVMOutput testing.
	"Init file context"
	output context: file.
	"Output LLVM IR to generator's writestream"
	output generateCode: ast.
]
