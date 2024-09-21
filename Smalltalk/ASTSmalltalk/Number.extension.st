Extension { #name : 'Number' }

{ #category : '*ASTSmalltalk' }
Number >> zigCompileObject: aWriteStream [

	^ self printOn: aWriteStream
]

{ #category : '*ASTSmalltalk' }
Number >> zigCompileObjectReference: anASZigCompileObjectGenerator [ 
	^ nil
]
