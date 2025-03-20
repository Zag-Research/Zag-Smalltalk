"
I encode the API needed from the LLVM generator interface.
"
Class {
	#name : 'ZagLLVMBuilderAPI',
	#superclass : 'Object',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'API' }
ZagLLVMBuilderAPI >> add: aNumber to: selfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> and: valueRef1 with: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> compare: valueRef1 is: comparison to: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> defineLabel: aLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> getContextRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> getExtraRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> getPCRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> getProcessRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> getSPRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> newLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> newRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> or: int1 with: int2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> register: register plus: offset [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> returnWith: addressOfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> shiftLeftArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> shiftRightArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> store: result at: addressOfSelf [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> tagSmallInteger: untaggedInt [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMBuilderAPI >> valueAt: aString [

	^ self subclassResponsibility
]
