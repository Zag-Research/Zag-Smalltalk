Extension { #name : 'Collection' }

{ #category : '*Zag-Exporter' }
Collection >> asChunkedArraysOfSize: size [

	| result target position |
	result := target := Array new: (self size min: size).
	position := 1.
	self withIndexDo: [ :each :index |
		(position = target size and: index < self size) ifTrue: [
			position := 1.
			target
				at: target size
				put:
				(target := Array new: (result size min: self size - index + 1)) ].
		target at: position put: each asString.
		position := position + 1 ].
	^ result
]
