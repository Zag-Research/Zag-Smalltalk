Extension { #name : 'ByteString' }

{ #category : '*Zag-Core' }
ByteString >> writeRawOn: aWriteStream [ 
	aWriteStream nextPutAll: self asByteArray.

]
