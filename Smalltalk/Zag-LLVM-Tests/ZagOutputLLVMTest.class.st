"
A ZagOutputLLVMTest is a test class for testing the behavior of ZagOutputLLVM
"
Class {
	#name : 'ZagOutputLLVMTest',
	#superclass : 'TestCase',
	#instVars : [
		'output',
		'textOut'
	],
	#category : 'Zag-LLVM-Tests-Tests',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ZagOutputLLVMTest >> setUp [

	super setUp.
	textOut := ZagOutputLLVMForTest new.
	output := ZagOutputLLVM new generator: textOut
]

{ #category : 'running' }
ZagOutputLLVMTest >> tearDown [

	Transcript flush.
	super tearDown
]

{ #category : 'tests' }
ZagOutputLLVMTest >> testAdd [

	output primitiveSmallIntegerPlus.
	textOut contents inspect
]

{ #category : 'tests' }
ZagOutputLLVMTest >> testFromArray [

	output genFrom:
		#( createContext pushLocal 25 pushLocal 7 send #+ dynamicDispatch returnTOS )
			readStream
]
