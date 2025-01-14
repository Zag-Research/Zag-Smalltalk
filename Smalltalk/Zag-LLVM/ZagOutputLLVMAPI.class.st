Class {
	#name : 'ZagOutputLLVMAPI',
	#superclass : 'Object',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'adding' }
ZagOutputLLVMAPI >> add: aNumber to: selfValue [

	^ self subclassResponsibility
]

{ #category : 'instance creation' }
ZagOutputLLVMAPI >> newLabel [

	^ self subclassResponsibility
]
