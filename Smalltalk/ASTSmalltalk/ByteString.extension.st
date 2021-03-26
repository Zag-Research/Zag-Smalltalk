Extension { #name : #ByteString }

{ #category : #'*ASTSmalltalk' }
ByteString >> writeRawOn: aWriteStream [ 
	aWriteStream nextPutAll: self asByteArray.

]
