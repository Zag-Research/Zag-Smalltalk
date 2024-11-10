Extension { #name : 'ASCReturnTop' }

{ #category : '*ASTSmalltalk-Tests' }
ASCReturnTop >> interpretFor: anInterpreter [

	stackElement isInContext ifTrue: [
		^ anInterpreter
			  return: anInterpreter top
			  selfContext: stackElement offset ].
	^ anInterpreter
		  return: anInterpreter top
		  selfOffset: stack offset - stackElement offset
]
