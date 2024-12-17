Extension { #name : 'True' }

{ #category : '*Zag-Core' }
True >> zigCompileObject: aWriteStream [

	aWriteStream nextPutAll: 'True'
]
