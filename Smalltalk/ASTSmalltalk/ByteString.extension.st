Extension { #name : 'ByteString' }

{ #category : '*ASTSmalltalk' }
ByteString >> astImageObject: anASImage [

	^ anASImage objectString: self
]

{ #category : '*ASTSmalltalk' }
ByteString >> writeRawOn: aWriteStream [ 
	aWriteStream nextPutAll: self asByteArray.

]
