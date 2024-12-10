Extension { #name : 'ASCReturnTop' }

{ #category : '*Zag-Core-Tests' }
ASCReturnTop >> interpretFor: anInterpreter [

	(inContext and: [ stackElement isInContext ]) ifTrue: [
		^ anInterpreter
			  return: anInterpreter top
			  selfContext: stackElement offset ].
	^ anInterpreter
		  return: anInterpreter top
		  selfOffset: stack offset - stackElement offset
]
