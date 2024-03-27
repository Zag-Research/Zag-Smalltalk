Extension { #name : #UndefinedObject }

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astImageObject: anASImage [

	^ anASImage putUndefinedObject
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
