Extension { #name : 'ByteString' }

{ #category : '*Zag-Core' }
ByteString >> astImageObject: anASImage [

	^ anASImage objectString: self
]

{ #category : '*Zag-Core' }
ByteString >> writeRawOn: aWriteStream [ 
	aWriteStream nextPutAll: self asByteArray.

]
