"
I encode the API needed from the LLVM generator interface.
"
Class {
	#name : 'ZagLLVMGeneratorAPI',
	#superclass : 'Object',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'API' }
ZagLLVMGeneratorAPI >> add: aNumber to: selfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> and: valueRef1 with: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> compare: valueRef1 is: comparison to: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> defineLabel: aLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> getContextRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> getExtraRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> getPCRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> getProcessRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> getSPRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> newLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> newRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> or: int1 with: int2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> register: register plus: offset [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> returnWith: addressOfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> shiftLeftArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> shiftRightArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> store: result at: addressOfSelf [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> tagSmallInteger: untaggedInt [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagLLVMGeneratorAPI >> valueAt: aString [

	^ self subclassResponsibility
]
