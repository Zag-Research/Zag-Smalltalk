Extension { #name : 'ArrayedCollection' }

{ #category : '*Zag-Core' }
ArrayedCollection >> astFields [
	^ self
]

{ #category : '*Zag-Core' }
ArrayedCollection >> astFormat [
	^ self subclassResponsibility
]
