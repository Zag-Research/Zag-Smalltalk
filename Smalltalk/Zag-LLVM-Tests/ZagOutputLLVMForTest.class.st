"
I set `gen` to myself, so I can produce a testable result.
"
Class {
	#name : 'ZagOutputLLVMForTest',
	#superclass : 'ZagOutputLLVMTextual',
	#category : 'Zag-LLVM-Tests-Tests',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
ZagOutputLLVMForTest >> contents [

	^ stream contents
]

{ #category : 'initialization' }
ZagOutputLLVMForTest >> initialize [

	super initialize.
	stream := WriteStream on: ''
]
