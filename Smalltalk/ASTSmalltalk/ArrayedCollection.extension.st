Extension { #name : 'ArrayedCollection' }

{ #category : '*ASTSmalltalk' }
ArrayedCollection >> astFields [
	^ self
]

{ #category : '*ASTSmalltalk' }
ArrayedCollection >> astFormat [
	^ self subclassResponsibility
]
