Extension { #name : 'SequenceableCollection' }

{ #category : '*Zag-Core-Tests' }
SequenceableCollection >> insertElementAt: i [

	| new |
	new := self at: i.
	i to: 2 by: -1 do: [ :j |
		| old |
		old := self at: j - 1.
		new >= old ifTrue: [
			self at: j put: new.
			^ self ].
		self at: j put: old ].
	self at: 1 put: new
]

{ #category : '*Zag-Core-Tests' }
SequenceableCollection >> insertionSorted [

	2 to: self size do: [ : i |
		self insertElementAt: i.
	]
]
