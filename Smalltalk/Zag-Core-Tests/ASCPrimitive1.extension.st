Extension { #name : 'ASCPrimitive1' }

{ #category : '*Zag-Core-Tests' }
ASCPrimitive1 >> interpretFor: anInterpreter [

	^ anInterpreter binary: [ :selfArg :other |
		  selfArg perform: primitive with: other ]
]
