Class {
	#name : 'ASLLVMTest',
	#superclass : 'ASCodeTest',
	#category : 'Zag-LLVM-Tests',
	#package : 'Zag-LLVM-Tests'
}

{ #category : 'running' }
ASLLVMTest >> setUp [

	super setUp.

	"Convert to LLVM specific context"
	file := ASLLFileContext new
]
