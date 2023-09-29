Extension { #name : #True }

{ #category : #'*ASTSmalltalk' }
True >> asZig: aWriteStream [

	aWriteStream nextPutAll: 'True'
]
