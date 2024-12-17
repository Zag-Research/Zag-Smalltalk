Extension { #name : 'Symbol' }

{ #category : '*Zag-Core' }
Symbol >> zigCompileObject: aWriteStream [

	aWriteStream putSymbol: self
]

{ #category : '*Zag-Core' }
Symbol >> zigCompileObjectReference: anASZigCompileObjectGenerator [

	^ nil
]
