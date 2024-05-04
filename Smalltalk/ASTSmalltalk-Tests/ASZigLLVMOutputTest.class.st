Class {
	#name : 'ASZigLLVMOutputTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASZigLLVMOutputTest >> testLLVMOutput [
	"in-progress"
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
