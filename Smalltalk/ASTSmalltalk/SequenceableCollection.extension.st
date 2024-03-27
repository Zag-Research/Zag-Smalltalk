Extension { #name : #SequenceableCollection }

{ #category : #'*ASTSmalltalk' }
SequenceableCollection >> asASSequence [
	^ ASSequence newFromArray: self
]
