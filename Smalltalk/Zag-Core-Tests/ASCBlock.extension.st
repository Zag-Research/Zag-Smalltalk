Extension { #name : 'ASCBlock' }

{ #category : '*Zag-Core-Tests' }
ASCBlock >> interpretFor: aSemanticInterpreter [

	^ aSemanticInterpreter pushBlock: block compiledUnit
]
