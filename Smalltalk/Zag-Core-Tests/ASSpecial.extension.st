Extension { #name : 'ASSpecial' }

{ #category : '*Zag-Core-Tests' }
ASSpecial >> baseValue [
	value = #nil ifTrue: [ ^ nil ].
	self error: 'can''t do baseValue for ',value
]
