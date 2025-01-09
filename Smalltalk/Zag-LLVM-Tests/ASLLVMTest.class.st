Class {
	#name : 'ASLLVMTest',
	#superclass : 'ASCodeTest',
	#category : 'Zag-LLVM-Tests-Tests',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASLLVMTest >> setUp [

	super setUp.

	"Convert to LLVM specific context"
	file := ASLLFileContext new
]
