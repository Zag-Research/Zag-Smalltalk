Extension { #name : #UndefinedObject }

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> asZig: aWriteStream [ 
	aWriteStream nextPutAll: 'Nil'
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> asZigReference: anASZigCompileObjectGenerator [

	^ nil
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astImageObject: anASImage [ 
	^ anASImage literal: 7 class: 4
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astObject: anASImage [ 
	^ self
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> threadFormatOn: threadPrinter [

	
]
