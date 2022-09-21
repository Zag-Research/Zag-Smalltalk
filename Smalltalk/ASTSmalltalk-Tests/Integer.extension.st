Extension { #name : #Integer }

{ #category : #'*ASTSmalltalk-Tests' }
Integer >> fibonacci [

	self < 2 ifTrue: [ ^ self ].
	^ (self - 1) fibonacci + (self - 2) fibonacci
]
