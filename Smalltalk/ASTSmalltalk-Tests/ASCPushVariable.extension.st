Extension { #name : 'ASCPushVariable' }

{ #category : '*ASTSmalltalk-Tests' }
ASCPushVariable >> interpretFor: anInterpreter [

	^ anInterpreter push: (anInterpreter stackAt: stackOffset - variableLocation offset)
]
