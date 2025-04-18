Extension { #name : 'ASCBlock' }

{ #category : '*Zag-Core-Tests' }
ASCBlock >> interpretFor: aSemanticInterpreter [

	self halt.
	^ aSemanticInterpreter pushBlock: block compiledUnit
]
