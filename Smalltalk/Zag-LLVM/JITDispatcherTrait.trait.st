"
I encode the API needed from the LLVM generator interface.
"
Trait {
	#name : 'JITDispatcherTrait',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'API' }
JITDispatcherTrait >> add: aNumber to: selfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> and: valueRef1 with: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> compare: valueRef1 is: comparison to: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> defineLabel: aLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> getContextRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> getExtraRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> getPCRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> getProcessRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> getSPRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> newLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> newRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> or: int1 with: int2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> register: register plus: offset [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> returnWith: addressOfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> shiftLeftArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> shiftRightArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> store: result at: addressOfSelf [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> tagSmallInteger: untaggedInt [

	^ self subclassResponsibility
]

{ #category : 'API' }
JITDispatcherTrait >> valueAt: aString [

	^ self subclassResponsibility
]
