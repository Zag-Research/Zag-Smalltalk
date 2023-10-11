Extension { #name : #UndefinedObject }

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astImageObject: anASImage [ 
	^ anASImage literal: 7 class: 4
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astObject: anASImage [ 
	^ self
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> isNonLocalThunkable [ ^ true
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> threadFormatOn: threadPrinter [

	
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> zigCompileObject: aWriteStream [ 
	aWriteStream nextPutAll: 'Nil'
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> zigCompileObjectReference: anASZigCompileObjectGenerator [

	^ nil
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> zigName [
	^ 'Nil'
]
