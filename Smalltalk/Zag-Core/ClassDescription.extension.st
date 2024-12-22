Extension { #name : 'ClassDescription' }

{ #category : '*Zag-Core' }
ClassDescription >> formatter [

	^ self package packageManifestOrNil formatter
]
