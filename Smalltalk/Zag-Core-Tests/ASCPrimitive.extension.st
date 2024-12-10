Extension { #name : 'ASCPrimitive' }

{ #category : '*Zag-Core-Tests' }
ASCPrimitive >> interpretFor: anInterpreter [

	^ anInterpreter unary: [ :selfArg | selfArg perform: primitive ]
]
