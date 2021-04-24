Extension { #name : #ArrayedCollection }

{ #category : #'*ASTSmalltalk' }
ArrayedCollection >> astFields [
	^ self
]

{ #category : #'*ASTSmalltalk' }
ArrayedCollection >> astFormat [
	self error: 'Need astFormat for ',self className
]
