Extension { #name : 'ASArray' }

{ #category : '*Zag-Compiler-Tests' }
ASArray >> baseValue [

	| bv |
	bv := OrderedCollection new.
	self do: [ :statement | bv add: statement baseValue ].
	^ bv asArray
]
