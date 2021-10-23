Extension { #name : #ASSpecial }

{ #category : #'*ASTSmalltalk-Tests' }
ASSpecial >> baseValue [
	value = #nil ifTrue: [ ^ nil ].
	self error: 'can''t do baseValue for ',value
]
