Extension { #name : 'False' }

{ #category : '*ASTSmalltalk' }
False >> zigCompileObject: aWriteStream [ 
	aWriteStream nextPutAll: 'False'
]
