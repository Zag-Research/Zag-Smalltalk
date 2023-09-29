Extension { #name : #Number }

{ #category : #'*ASTSmalltalk' }
Number >> asZig: aWriteStream [

	^ self printOn: aWriteStream
]

{ #category : #'*ASTSmalltalk' }
Number >> asZigReference: anASZigCompileObjectGenerator [ 
	^ nil
]
