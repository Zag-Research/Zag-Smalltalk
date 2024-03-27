Extension { #name : #Boolean }

{ #category : #'*ASTSmalltalk' }
Boolean >> asZigLiteral [

	^ 'Object.from(' , self asString , ')'
]

{ #category : #'*ASTSmalltalk' }
Boolean >> astImageObject: anASImage [

	^ anASImage putBoolean: self
]

{ #category : #'*ASTSmalltalk' }
Boolean >> astObject: anASImage [ 
	^ self
]

{ #category : #'*ASTSmalltalk' }
Boolean >> isNonLocalThunkable [ 
^ true
]

{ #category : #'*ASTSmalltalk' }
Boolean >> zigCompileObjectReference: anASZigCompileObjectGenerator [ 
	^ nil
]
