Extension { #name : #Symbol }

{ #category : #'*ASTSmalltalk' }
Symbol >> asZig: anASZigCompileObjectGenerator [ 
	anASZigCompileObjectGenerator putSymbol: self
]

{ #category : #'*ASTSmalltalk' }
Symbol >> asZigReference: anASZigCompileObjectGenerator [

	^ nil
]
