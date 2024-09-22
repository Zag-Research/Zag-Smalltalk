Class {
	#name : 'ASLLVMTest',
	#superclass : 'ASCodeTest',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASLLVMTest >> setUp [

	super setUp.

	"Convert to LLVM specific context"
	file := ASLLFileContext new
]
