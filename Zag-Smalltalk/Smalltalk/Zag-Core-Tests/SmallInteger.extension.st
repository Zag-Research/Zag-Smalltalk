Extension { #name : 'SmallInteger' }

{ #category : '*Zag-Core-Tests' }
SmallInteger >> isMREven [

	self isZero ifTrue: [ ^ true ].
	^ self - 1 isMROdd
]

{ #category : '*Zag-Core-Tests' }
SmallInteger >> isMROdd [

	self isZero ifTrue: [ ^ false ].
	^ self - 1 isMREven
]

{ #category : '*Zag-Core-Tests' }
SmallInteger >> siPrim10: aNumber [

	<primitive: 10>
	^ ASIPrimitiveError
]

{ #category : '*Zag-Core-Tests' }
SmallInteger >> siPrim1: aNumber [

	<primitive: 1>
	^ ASIPrimitiveError
]

{ #category : '*Zag-Core-Tests' }
SmallInteger >> siPrim2: aNumber [

	<primitive: 2>
	^ ASIPrimitiveError
]

{ #category : '*Zag-Core-Tests' }
SmallInteger >> testDiv: aNumber [
	"Primitive. Divide the receiver by the argument and return the result if the division is exact. 
	Used in Zag tests"

	<primitive: 10>
	^ 42
]
