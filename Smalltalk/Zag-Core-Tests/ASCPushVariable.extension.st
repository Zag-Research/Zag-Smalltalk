Extension { #name : 'ASCPushVariable' }

{ #category : '*Zag-Core-Tests' }
ASCPushVariable >> interpretFor: anInterpreter [

	^ anInterpreter push:
		  ((inContext and: [ variableLocation isInContext ])
			   ifTrue: [ anInterpreter contextAt: variableLocation offset ]
			   ifFalse: [
			   anInterpreter stackAt: stackOffset - variableLocation offset ])
]
