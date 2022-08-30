Extension { #name : #Integer }

{ #category : #'*ASTSmalltalk-Tests' }
Integer >> fibonacci [

	self <= 2 ifTrue: [ ^ 1 ].
	^ (self - 1) fibonacci + (self - 2) fibonacci
]
