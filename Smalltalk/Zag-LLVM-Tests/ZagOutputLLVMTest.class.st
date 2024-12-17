"
A ZagOutputLLVMTest is a test class for testing the behavior of ZagOutputLLVM
"
Class {
	#name : 'ZagOutputLLVMTest',
	#superclass : 'TestCase',
	#instVars : [
		'output'
	],
	#category : 'Zag-LLVM-Tests',
	#package : 'Zag-LLVM-Tests'
}

{ #category : 'running' }
ZagOutputLLVMTest >> setUp [
	super setUp.

	Transcript clear.
	output := ZagOutputLLVM new generator: ZagOutputLLVMAPI new
]

{ #category : 'tests' }
ZagOutputLLVMTest >> testAdd [

	output primitiveSmallIntegerPlus
]
