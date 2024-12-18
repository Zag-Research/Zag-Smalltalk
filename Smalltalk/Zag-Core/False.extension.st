Extension { #name : 'False' }

{ #category : '*Zag-Core' }
False >> zigCompileObject: aWriteStream [ 
	aWriteStream nextPutAll: 'False'
]
