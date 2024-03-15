Extension { #name : 'OrderedCollection' }

{ #category : '*ASTSmalltalk' }
OrderedCollection >> add: anObject at: position [

	[ self size < position ] whileTrue: [ self add: nil ].
	self at: position put: anObject.
	^ position
]

{ #category : '*ASTSmalltalk' }
OrderedCollection >> zigCompileObjectReference: anASZigCompileObjectGenerator [
	^ self asArray zigCompileObject: anASZigCompileObjectGenerator
]
