Extension { #name : 'ASCPushVariable' }

{ #category : '*Zag-Core-Tests' }
ASCPushVariable >> interpretFor: anInterpreter [
self halt.
	^ anInterpreter push: (anInterpreter varAt: variableLocation)
]
