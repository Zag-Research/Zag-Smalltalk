Extension { #name : 'UndefinedObject' }

{ #category : '*Zag-Core' }
UndefinedObject >> astObject: anASImage [ 
	^ self
]

{ #category : '*Zag-Core' }
UndefinedObject >> isNonLocalThunkable [ ^ true
]

{ #category : '*Zag-Core' }
UndefinedObject >> threadFormatOn: threadPrinter [

	
]

{ #category : '*Zag-Core' }
UndefinedObject >> validate: aStackElement [

	^ self
]

{ #category : '*Zag-Core' }
UndefinedObject >> zigCompileObject: aWriteStream [ 
	aWriteStream nextPutAll: 'Nil'
]

{ #category : '*Zag-Core' }
UndefinedObject >> zigCompileObjectReference: anASZigCompileObjectGenerator [

	^ nil
]

{ #category : '*Zag-Core' }
UndefinedObject >> zigName [
	^ 'Nil'
]
