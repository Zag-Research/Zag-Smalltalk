Class {
	#name : 'ZagBlockClosure',
	#superclass : 'ZagSubstitutions',
	#category : 'Zag-Substitutions',
	#package : 'Zag-Substitutions'
}

{ #category : 'as yet unclassified' }
ZagBlockClosure class >> mirroredClass [

	^ BlockClosure
]

{ #category : 'evaluating' }
ZagBlockClosure >> value [

	<primitive: 206>
	
]

{ #category : 'evaluating' }
ZagBlockClosure >> value: arg1 [

	<primitive: 207>
	
]

{ #category : 'evaluating' }
ZagBlockClosure >> value: arg1 value: arg2 [

	<primitive: 208>
	
]

{ #category : 'evaluating' }
ZagBlockClosure >> value: arg1 value: arg2 value: arg3 [

	<primitive: 209>
	
]

{ #category : 'evaluating' }
ZagBlockClosure >> value: arg1 value: arg2 value: arg3 value: arg4 [

	<primitive: 210>
	
]

{ #category : 'evaluating' }
ZagBlockClosure >> valueWithArguments: anArray [
	"Primitive. Evaluate the block represented by the receiver. The argument is an Array whose elements are the arguments for the block. Fail if the length of the Array is not the same as the the number of arguments that the block was expecting. Fail if the block is already being executed. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 205>
	anArray size = self numArgs
		ifTrue: [
			self error:
				'Attempt to evaluate a block that is already being evaluated.' ]
		ifFalse: [ self numArgsError: anArray size ]
]

{ #category : 'evaluating' }
ZagBlockClosure >> whileFalse [

	"Evaluate the receiver, as long as its value is false."

	self value ifTrue: [^ self].
	^ self whileFalse
]

{ #category : 'evaluating' }
ZagBlockClosure >> whileFalse: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is false."

	self value ifTrue: [^ nil].
	aBlock value.
	^ self whileFalse: aBlock
]

{ #category : 'evaluating' }
ZagBlockClosure >> whileNil: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is nil."

	self value ifNotNil: [^ nil].
	aBlock value.
	^ self whileNil: aBlock
]

{ #category : 'evaluating' }
ZagBlockClosure >> whileNotNil: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is not nil."

	self value ifNil: [ ^ nil ].
	aBlock value.
	^ self whileNotNil: aBlock
]

{ #category : 'evaluating' }
ZagBlockClosure >> whileTrue [

	"Evaluate the receiver, as long as its value is true."

	self value ifFalse: [^ self].
	^ self whileTrue
]

{ #category : 'evaluating' }
ZagBlockClosure >> whileTrue: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is true."

	self value ifFalse: [ ^ nil ].
	aBlock value.
	^ self whileTrue: aBlock
]
