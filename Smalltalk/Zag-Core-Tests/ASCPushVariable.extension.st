Extension { #name : 'ASCPushVariable' }

{ #category : '*Zag-Core-Tests' }
ASCPushVariable >> interpretFor: anInterpreter [

	^ anInterpreter push: (anInterpreter varAt: variableLocation)
]
