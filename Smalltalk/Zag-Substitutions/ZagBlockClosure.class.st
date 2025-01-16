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
ZagBlockClosure >> whileTrue: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is true."

	self value ifFalse: [ ^ nil ].
	aBlock value.
	^ self whileTrue: aBlock
]
