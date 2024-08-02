Extension { #name : 'SmallInteger' }

{ #category : '*ASTSmalltalk-Tests' }
SmallInteger >> isMREven [

	self isZero ifTrue: [ ^ true ].
	^ self - 1 isMROdd
]

{ #category : '*ASTSmalltalk-Tests' }
SmallInteger >> isMROdd [

	self isZero ifTrue: [ ^ false ].
	^ self - 1 isMREven
]
