Extension { #name : #False }

{ #category : #'*ASTSmalltalk' }
False >> asZig: aWriteStream [ 
	aWriteStream nextPutAll: 'False'
]
