Extension { #name : 'ByteString' }

{ #category : '*ASTSmalltalk' }
ByteString >> astImagePut: anASImage [ 
	^ anASImage objectString: self
]

{ #category : '*ASTSmalltalk' }
ByteString >> writeRawOn: aWriteStream [ 
	aWriteStream nextPutAll: self asByteArray.

]
