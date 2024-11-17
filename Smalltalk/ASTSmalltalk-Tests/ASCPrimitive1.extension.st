Extension { #name : 'ASCPrimitive1' }

{ #category : '*ASTSmalltalk-Tests' }
ASCPrimitive1 >> interpretFor: anInterpreter [

	^ anInterpreter binary: [ :selfArg :other |
		  selfArg perform: primitive with: other ]
]
