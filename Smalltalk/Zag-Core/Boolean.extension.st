Extension { #name : 'Boolean' }

{ #category : '*Zag-Core' }
Boolean >> asZigLiteral [

	^ 'Object.from(' , self asString , ')'
]

{ #category : '*Zag-Core' }
Boolean >> astObject: anASImage [ 
	^ self
]

{ #category : '*Zag-Core' }
Boolean >> isNonLocalThunkable [ 
^ true
]

{ #category : '*Zag-Core' }
Boolean >> zigCompileObjectReference: anASZigCompileObjectGenerator [ 
	^ nil
]
