"
I encode the API needed from the LLVM generator interface.
"
Trait {
	#name : 'ZagJITDispatcherTrait',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'API' }
ZagJITDispatcherTrait >> add: aNumber to: selfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> and: valueRef1 with: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> compare: valueRef1 is: comparison to: valueRef2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> defineLabel: aLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> getContextRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> getExtraRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> getPCRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> getProcessRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> getSPRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> newLabel [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> newRegister [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> or: int1 with: int2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> register: register plus: offset [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> returnWith: addressOfValue [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> shiftLeftArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> shiftRightArithmeticly: num1 by: num2 [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> store: result at: addressOfSelf [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> tagSmallInteger: untaggedInt [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagJITDispatcherTrait >> valueAt: aString [

	^ self subclassResponsibility
]
