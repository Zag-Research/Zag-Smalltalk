Extension { #name : #Symbol }

{ #category : #'*ASTSmalltalk' }
Symbol >> zigCompileObject: anASZigCompileObjectGenerator [ 
	anASZigCompileObjectGenerator putSymbol: self
]

{ #category : #'*ASTSmalltalk' }
Symbol >> zigCompileObjectReference: anASZigCompileObjectGenerator [

	^ nil
]
