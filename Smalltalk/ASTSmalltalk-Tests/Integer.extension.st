Extension { #name : 'Integer' }

{ #category : '*ASTSmalltalk-Tests' }
Integer >> fibonacci [

	self < 2 ifTrue: [ ^ self ].
	^ (self - 1) fibonacci + (self - 2) fibonacci
]

{ #category : '*ASTSmalltalk-Tests' }
Integer >> fibonacci_accumulator: accumulator prev: prev [

	self = 0 ifTrue: [ ^ prev ].
	^ self - 1 fibonacci_accumulator: prev + accumulator prev: accumulator
]

{ #category : '*ASTSmalltalk-Tests' }
Integer >> fibonacci_alt [

	^ self <= 2
		  ifTrue: [ 1 ]
		  ifFalse: [ (self - 1) fibonacci + (self - 2) fibonacci ]
]

{ #category : '*ASTSmalltalk-Tests' }
Integer >> fibonacci_fast [

	^ self fibonacci_accumulator: 1 prev: 0
]
