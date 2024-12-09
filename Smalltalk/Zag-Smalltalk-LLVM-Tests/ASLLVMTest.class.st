Class {
	#name : 'ASLLVMTest',
	#superclass : 'ASCodeTest',
	#category : 'Zag-Smalltalk-LLVM-Tests',
	#package : 'Zag-Smalltalk-LLVM-Tests'
}

{ #category : 'running' }
ASLLVMTest >> setUp [

	super setUp.

	"Convert to LLVM specific context"
	file := ASLLFileContext new
]
