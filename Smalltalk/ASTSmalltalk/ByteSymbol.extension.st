Extension { #name : #ByteSymbol }

{ #category : #'*ASTSmalltalk' }
ByteSymbol >> astObject: anASImage [ 
	^ anASImage symbol: self
]

{ #category : #'*ASTSmalltalk' }
ByteSymbol >> writeWithCountOn: aWriteStream [ 
	self size writeRaw16On: aWriteStream.
	aWriteStream nextPutAll: self asByteArray.
	self size odd ifTrue: [ aWriteStream nextPut: 0 ].


]
