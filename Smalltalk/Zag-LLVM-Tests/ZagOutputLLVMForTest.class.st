"
I set `gen` to myself, so I can produce a testable result.
"
Class {
	#name : 'ZagOutputLLVMForTest',
	#superclass : 'ZagOutputLLVM',
	#category : 'Zag-LLVM-Tests',
	#package : 'Zag-LLVM-Tests'
}

{ #category : 'initialization' }
ZagOutputLLVMForTest >> initialize [

	super initialize.
	gen := self
]
