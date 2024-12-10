Extension { #name : 'OrderedCollection' }

{ #category : '*Zag-Core' }
OrderedCollection >> zigCompileObjectReference: anASZigCompileObjectGenerator [
	^ self asArray zigCompileObject: anASZigCompileObjectGenerator
]
