Extension { #name : 'SequenceableCollection' }

{ #category : '*Zag-Core' }
SequenceableCollection >> asASSequence [
	^ ASSequence newFromArray: self
]
