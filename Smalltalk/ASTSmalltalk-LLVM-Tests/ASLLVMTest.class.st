Class {
	#name : 'ASLLVMTest',
	#superclass : 'ASCodeTest',
	#category : 'ASTSmalltalk-LLVM-Tests',
	#package : 'ASTSmalltalk-LLVM-Tests'
}

{ #category : 'running' }
ASLLVMTest >> setUp [

	super setUp.

	"Convert to LLVM specific context"
	file := ASLLFileContext new
]
