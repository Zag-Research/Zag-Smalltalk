Extension { #name : 'ASCPrimitive' }

{ #category : '*ASTSmalltalk-Tests' }
ASCPrimitive >> interpretFor: anInterpreter [

	^ anInterpreter unary: [ :selfArg | selfArg perform: primitive ]
]
