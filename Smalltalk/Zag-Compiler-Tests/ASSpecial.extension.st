Extension { #name : 'ASSpecial' }

{ #category : '*Zag-Compiler-Tests' }
ASSpecial >> baseValue [
	value = #nil ifTrue: [ ^ nil ].
	self error: 'can''t do baseValue for ',value
]
