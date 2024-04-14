Extension { #name : 'Integer' }

{ #category : '*ASTSmalltalk' }
Integer >> writeRaw16On: aWriteStream [ 
	aWriteStream nextPutAll: ((16r10000 + self) asByteArray reversed first: 2)
]

{ #category : '*ASTSmalltalk' }
Integer >> writeRaw32On: aWriteStream [ 
	aWriteStream nextPutAll: ((16r100000000 + self) asByteArray reversed first: 4)
]
