Extension { #name : 'ByteSymbol' }

{ #category : '*ASTSmalltalk' }
ByteSymbol >> asZigString [

	(self anySatisfy: [ :each |
		 each = $: | (each isDigit | each isLetter) not ]) ifTrue: [
		^ '@"' , self , '"' ].
	^ self
]

{ #category : '*ASTSmalltalk' }
ByteSymbol >> astImageObject: anASImage [

	^ anASImage putSymbol: self
]

{ #category : '*ASTSmalltalk' }
ByteSymbol >> astObject: anASImage [ 
	^ self
]

{ #category : '*ASTSmalltalk' }
ByteSymbol >> writeWithCountOn: aWriteStream [ 
	self size writeRaw16On: aWriteStream.
	aWriteStream nextPutAll: self asByteArray.
	self size odd ifTrue: [ aWriteStream nextPut: 0 ].


]
