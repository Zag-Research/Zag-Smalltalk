Extension { #name : 'OrderedCollection' }

{ #category : '*ASTSmalltalk' }
OrderedCollection >> zigCompileObjectReference: anASZigCompileObjectGenerator [
	^ self asArray zigCompileObject: anASZigCompileObjectGenerator
]
