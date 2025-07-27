Extension { #name : 'ASCPopVariable' }

{ #category : '*Zag-Core-Tests' }
ASCPopVariable >> interpretFor: anInterpreter [

	^ anInterpreter pop: anInterpreter top into: variableLocation offset
]
