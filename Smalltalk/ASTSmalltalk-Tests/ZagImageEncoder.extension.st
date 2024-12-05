Extension { #name : 'ZagImageEncoder' }

{ #category : '*ASTSmalltalk-Tests' }
ZagImageEncoder class >> convertNumber: aNumber [

	aNumber isInteger ifTrue: [ ^ self smallInteger: aNumber ].
	aNumber isFloat ifTrue: [ ^ self double: aNumber for: nil].
	self error: 'unexpected number'
]
