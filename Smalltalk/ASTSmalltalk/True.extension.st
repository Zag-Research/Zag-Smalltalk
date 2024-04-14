Extension { #name : 'True' }

{ #category : '*ASTSmalltalk' }
True >> zigCompileObject: aWriteStream [

	aWriteStream nextPutAll: 'True'
]
