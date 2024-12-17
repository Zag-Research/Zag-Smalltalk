Extension { #name : 'ByteSymbol' }

{ #category : '*Zag-Core' }
ByteSymbol >> asZigString [

	(self anySatisfy: [ :each |
		 each = $: | (each isDigit | each isLetter) not ]) ifTrue: [
		^ '@"' , self , '"' ].
	^ self
]

{ #category : '*Zag-Core' }
ByteSymbol >> astObject: anASImage [ 
	^ self
]

{ #category : '*Zag-Core' }
ByteSymbol >> writeWithCountOn: aWriteStream [ 
	self size writeRaw16On: aWriteStream.
	aWriteStream nextPutAll: self asByteArray.
	self size odd ifTrue: [ aWriteStream nextPut: 0 ].


]
