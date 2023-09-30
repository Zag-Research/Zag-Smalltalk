Extension { #name : #OrderedCollection }

{ #category : #'*ASTSmalltalk' }
OrderedCollection >> asZigReference: anASZigCompileObjectGenerator [
	^ self asArray asZig: anASZigCompileObjectGenerator
]
