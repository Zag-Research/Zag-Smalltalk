Extension { #name : 'Object' }

{ #category : '*Zag-Compiler' }
Object >> asASLiteral [
	^ ASLiteralToken new value: self
]

{ #category : '*Zag-Compiler' }
Object >> asASString [
	^ ASString new value: self
]
