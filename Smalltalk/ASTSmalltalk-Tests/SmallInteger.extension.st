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

{ #category : '*ASTSmalltalk-Tests' }
SmallInteger >> siPrim10: aNumber [

	<primitive: 10>
	^ ASIPrimitiveError
]

{ #category : '*ASTSmalltalk-Tests' }
SmallInteger >> siPrim1: aNumber [

	<primitive: 1>
	^ ASIPrimitiveError
]

{ #category : '*ASTSmalltalk-Tests' }
SmallInteger >> siPrim2: aNumber [

	<primitive: 2>
	^ ASIPrimitiveError
]

{ #category : '*ASTSmalltalk-Tests' }
SmallInteger >> testDiv: aNumber [
	"Primitive. Divide the receiver by the argument and return the result if the division is exact. 
	Used in Zag tests"

	<primitive: 10>
	^ 42
]
