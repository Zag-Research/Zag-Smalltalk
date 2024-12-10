Extension { #name : 'Number' }

{ #category : '*Zag-Core' }
Number >> zigCompileObject: aWriteStream [

	^ self printOn: aWriteStream
]

{ #category : '*Zag-Core' }
Number >> zigCompileObjectReference: anASZigCompileObjectGenerator [ 
	^ nil
]
